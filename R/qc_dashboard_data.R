#' Prepare metric comparison identities without losing source associations
#' @param metrics Saved numeric evidence, including unavailable values.
#' @param inventory Derivative identities.
#' @return Metric rows with stable comparison IDs and readable group labels.
#' @noRd
qc_metric_observations <- function(metrics, inventory) {
  joined <- qc_display_identity(metrics, inventory)
  fields <- c("metric", "unit", "stage", "stream", "input_stream", "ses_id",
    "task", "space", "resolution", "atlas")
  for (field in setdiff(fields, names(joined))) joined[[field]] <- rep(NA_character_, nrow(joined))
  if (!nrow(joined)) {
    joined$group_id <- joined$group_label <- character()
    return(joined)
  }
  groups <- joined[fields]
  unknown <- joined$stage %in% "extract_rois" & is.na(joined$atlas)
  groups$atlas[unknown] <- dirname(joined$source_file[unknown])
  # FD has a common definition across acquisitions. Retain every association
  # until after filtering so a shared source remains selectable in each space.
  fd <- joined$metric %in% "mean_fd"
  groups[fd, setdiff(fields, c("metric", "unit"))] <- ""
  keys <- vapply(seq_len(nrow(groups)), function(index) {
    as.character(jsonlite::toJSON(unname(as.list(groups[index, ])), na = "null", auto_unbox = TRUE))
  }, character(1))
  ordered <- unique(c(keys[fd], keys[!fd]))
  joined$group_id <- paste0("metric-", match(keys, ordered))
  labels <- vapply(match(ordered, keys), function(index) {
    if (fd[index]) return(paste0("Mean FD (all tasks) / ", joined$unit[index]))
    values <- as.character(groups[index, ])
    paste(basename(values[!is.na(values) & nzchar(values)]), collapse = " / ")
  }, character(1))
  joined$group_label <- make.unique(labels)[match(keys, ordered)]
  joined
}

#' Deduplicate source observations within compatible metric comparison groups
#' @param rows Metric rows prepared by qc_metric_observations(), after scope filtering.
#' @return One row per source/group; unlocated sources retain record identity.
#' @noRd
qc_metric_sources <- function(rows) {
  source <- ifelse(is.na(rows$source_file) | !nzchar(rows$source_file), rows$record_id, rows$source_file)
  rows[!duplicated(data.frame(source = source, group = rows$group_id)), , drop = FALSE]
}

#' Group attention by acquisition, processing context, and reason
#' @param inventory Scoped derivative rows with attention explanations.
#' @return Review rows with contributing record IDs for lazy drill-down.
#' @noRd
qc_attention_groups <- function(inventory) {
  if (!"attention_reasons" %in% names(inventory)) inventory$attention_reasons <- rep("", nrow(inventory))
  identity <- intersect(c("sub_id", "ses_id", "task", "run", "acquisition", "reconstruction",
    "direction", "echo", "part", "stage", "stream"), names(inventory))
  template <- inventory[FALSE, identity, drop = FALSE]
  template$category <- template$reason <- template$record_ids <- character()
  template$products <- integer()
  template <- template[c("category", "reason", "products", identity, "record_ids")]
  flagged <- inventory[inventory$attention %in% TRUE, , drop = FALSE]
  if (!nrow(flagged)) return(template)
  reasons <- strsplit(ifelse(is.na(flagged$attention_reasons) | !nzchar(flagged$attention_reasons),
    "Recorded attention flag", flagged$attention_reasons), ";\\s*")
  rows <- flagged[rep(seq_len(nrow(flagged)), lengths(reasons)), c(identity, "record_id"), drop = FALSE]
  rows$reason <- unlist(reasons, use.names = FALSE)
  grouped <- data.table::as.data.table(rows)[, list(products = length(unique(get("record_id"))),
    record_ids = paste(unique(get("record_id")), collapse = "\n")), by = c(identity, "reason")]
  grouped$category <- ifelse(grepl("missing|not run|unavailable|invalid|ambiguous", grouped$reason, ignore.case = TRUE),
    "Missing/invalid evidence", ifelse(grepl("failed|error|blocked|cancelled", grouped$reason,
      ignore.case = TRUE), "Computational/workflow failure", "Descriptive review"))
  as.data.frame(grouped)[names(template)]
}

#' Embed one canonical detail payload with precomputed relative evidence URLs
#' @param x Saved snapshot; never queried or modified.
#' @param inventory Normalized display inventory.
#' @param metrics Metric rows with comparison identities.
#' @param output_dir Report directory used to resolve relative links.
#' @return Inert JSON script for indexed, on-demand browser details.
#' @noRd
qc_dashboard_payload <- function(x, inventory, metrics, output_dir) {
  paths <- unique(na.omit(x$evidence$path))
  links <- stats::setNames(vapply(paths, qc_evidence_href, character(1), output_dir = output_dir), paths)
  evidence <- qc_display_identity(x$evidence, inventory)
  evidence$href <- unname(links[evidence$path])
  jobs <- x$jobs
  jobs$provenance_path <- if (nrow(jobs)) ifelse(is.na(jobs$sequence_id), NA_character_,
    file.path(x$metadata$log_directory, "runs", jobs$sequence_id, "provenance.json")) else character()
  job_fields <- intersect(c("stdout_log", "stderr_log", "job_manifest_path", "runtime_receipt_path", "provenance_path"), names(jobs))
  log_paths <- unique(na.omit(unlist(jobs[job_fields], use.names = FALSE)))
  log_paths <- log_paths[nzchar(log_paths)]
  job_links <- data.frame(path = log_paths, exists = file.exists(log_paths),
    href = vapply(log_paths, qc_evidence_href, character(1), output_dir = output_dir))
  # Row details need identifiers and links, not serialized manifests or job
  # objects. Full tracking and manifest history remains in the exported tables.
  jobs <- jobs[intersect(c("job_id", "sequence_id", "attempt", job_fields), names(jobs))]
  manifests <- x$manifest_files[x$manifest_files$path %in% inventory$derivative_file,
    intersect(c("path", "job_id", "run_id", "attempt", "is_current_attempt", "status"),
      names(x$manifest_files)), drop = FALSE]
  payload <- list(metadata = x$metadata[c("created_at", "scheduler_refreshed", "scope", "run_id", "submission")],
    validation_settings = if (is.data.frame(x$metadata$settings))
      unname(as.list(x$metadata$settings$value[x$metadata$settings$setting == "Validation enabled"])) else list(),
    inventory = inventory, acquisitions = x$acquisitions, workflow = x$workflow,
    coverage = qc_snapshot_coverage(x),
    metrics = metrics, evidence = evidence, checks = x$checks, manifests = manifests,
    jobs = jobs, job_links = job_links)
  json <- jsonlite::toJSON(payload, dataframe = "rows", auto_unbox = TRUE, na = "null",
    null = "null", digits = NA)
  # Paths and saved messages are untrusted text, not executable HTML.
  json <- gsub("<", "\\u003c", json, fixed = TRUE)
  htmltools::tags$script(id = "bg-qc-data", type = "application/json", htmltools::HTML(json))
}

#' Render scope-aware headline counters with explicit observational denominators
#' @param totals Snapshot totals used before browser initialization.
#' @return HTML counters updated from the shared browser scope.
#' @noRd
qc_summary_cards <- function(totals) {
  labels <- c(participants = "Participants", raw_bold_files = "Raw BOLD files",
    available_outputs = "Resolved output paths available", passed_required_audits = "Required audits",
    review_items = "Review items (including coverage)")
  htmltools::tags$div(class = "qc-summary", lapply(names(labels), function(field) {
    htmltools::tags$div(htmltools::tags$span(labels[[field]]),
      htmltools::tags$strong(`data-qc-total` = field, totals[[field]]))
  }))
}
