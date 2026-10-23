#' Join QC evidence to participant and acquisition identities for display
#' @param rows Evidence table with record_id.
#' @param inventory Derivative inventory.
#' @return Evidence rows with readable acquisition identities.
#' @noRd
qc_display_identity <- function(rows, inventory) {
  fields <- intersect(c("sub_id", "ses_id", "task", "run", "stage", "stream",
    "space", "resolution", "input_stream", "atlas"), names(inventory))
  fields <- setdiff(fields, names(rows))
  identity <- inventory[match(rows$record_id, inventory$record_id), fields, drop = FALSE]
  rownames(identity) <- NULL
  cbind(identity, rows)
}

#' Summarize outputs without counting supporting inputs as selected outputs
#' @param x Collected QC snapshot.
#' @return Snapshot totals, participant rows, and stage/stream output summaries.
#' @noRd
qc_dashboard_summary <- function(x) {
  inventory <- x$inventory
  if (!"record_role" %in% names(inventory)) inventory$record_role <- rep("output", nrow(inventory))
  if (!"validation_required" %in% names(inventory)) {
    inventory$validation_required <- rep(NA, nrow(inventory))
  }
  output <- inventory[inventory$record_role == "output", , drop = FALSE]
  participants <- sort(unique(na.omit(c(x$workflow$sub_id, inventory$sub_id,
    x$acquisitions$sub_id))))
  participant_rows <- lapply(participants, function(participant) {
    rows <- output[output$sub_id == participant & !is.na(output$sub_id), , drop = FALSE]
    evidence_rows <- inventory[inventory$sub_id == participant & !is.na(inventory$sub_id), , drop = FALSE]
    units <- x$workflow[x$workflow$sub_id == participant & !is.na(x$workflow$sub_id), , drop = FALSE]
    required <- rows$validation_required %in% TRUE
    data.frame(sub_id = participant,
      raw_bold_files = sum(x$acquisitions$sub_id == participant, na.rm = TRUE),
      available_outputs = sum(rows$output_status == "available"), output_records = nrow(rows),
      passed_required_audits = sum(required & rows$validation_status == "passed"),
      required_audits = sum(required), attention_records = sum(evidence_rows$attention),
      completed_units = sum(units$status == "COMPLETED"), workflow_units = nrow(units))
  })
  participant_table <- qc_bind_rows(participant_rows, data.frame(sub_id = character(),
    raw_bold_files = integer(), available_outputs = integer(), output_records = integer(),
    passed_required_audits = integer(), required_audits = integer(), attention_records = integer(),
    completed_units = integer(), workflow_units = integer()))
  groups <- split(seq_len(nrow(inventory)), paste(inventory$record_role,
    inventory$stage, ifelse(is.na(inventory$stream), "", inventory$stream), sep = "\r"))
  stage_rows <- lapply(groups, function(indices) {
    rows <- inventory[indices, , drop = FALSE]
    data.frame(record_role = rows$record_role[1L], stage = rows$stage[1L], stream = rows$stream[1L],
      available = sum(rows$output_status == "available"), records = nrow(rows),
      missing_or_empty = sum(rows$output_status != "available"),
      attention_records = sum(rows$attention))
  })
  list(participants = participant_table,
    outputs = qc_bind_rows(stage_rows, data.frame(record_role = character(), stage = character(),
      stream = character(), available = integer(), records = integer(),
      missing_or_empty = integer(), attention_records = integer())),
    totals = list(participants = length(participants), raw_bold_files = nrow(x$acquisitions),
      outputs = nrow(output), available_outputs = sum(output$output_status == "available"),
      upstream_inputs = sum(inventory$record_role == "upstream_input"),
      required_audits = sum(output$validation_required %in% TRUE),
      passed_required_audits = sum(output$validation_required %in% TRUE & output$validation_status == "passed"),
      attention_records = sum(inventory$attention), collection_issues = nrow(x$issues),
      completed_units = sum(x$workflow$status == "COMPLETED"), workflow_units = nrow(x$workflow),
      job_records = nrow(x$jobs)))
}

#' Summarize manifest associations by workflow stage and comparison result
#' @param files Full exported manifest association table.
#' @return Compact table with association and distinct-path counts per group.
#' @noRd
qc_manifest_summary <- function(files) {
  fields <- intersect(c("stage", "stream", "status"), names(files))
  template <- files[FALSE, fields, drop = FALSE]
  template$associations <- integer()
  template$unique_paths <- integer()
  if (!nrow(files)) return(template)
  groups <- split(seq_len(nrow(files)), do.call(paste, c(lapply(files[fields],
    function(value) ifelse(is.na(value), "", value)), sep = "\r")))
  qc_bind_rows(lapply(groups, function(indices) {
    cbind(files[indices[1L], fields, drop = FALSE], associations = length(indices),
      unique_paths = length(unique(files$path[indices])))
  }), template)
}

#' Build shared exact-match table controls from the snapshot identities
#' @param x Collected QC snapshot.
#' @return Escaped HTML controls; overview totals and plot selectors stay fixed.
#' @noRd
qc_dashboard_controls <- function(x) {
  fields <- c(sub_id = "Participant", ses_id = "Session", task = "Task",
    stage = "Stage", stream = "Stream", space = "Space")
  htmltools::tags$div(class = "qc-controls", lapply(names(fields), function(field) {
    observed <- unlist(lapply(list(x$inventory, x$acquisitions,
      x$workflow), function(rows) rows[[field]]))
    values <- sort(unique(na.omit(observed)))
    id <- paste0("qc-filter-", field)
    htmltools::tags$div(class = "qc-control", htmltools::tags$label(`for` = id, fields[[field]]),
      htmltools::tags$select(id = id, `data-qc-filter` = field,
        htmltools::tags$option(value = "", "All"),
        if (anyNA(observed)) htmltools::tags$option(value = "__qc_missing__",
          if (field == "ses_id") "No session label" else "Unavailable"),
        lapply(values, function(value) {
          htmltools::tags$option(value = value, value)
        })))
  }), htmltools::tags$button(type = "button", `data-qc-reset` = "true", "Reset shared filters"),
    htmltools::tags$p(class = "qc-filter-note", `aria-live` = "polite", id = "qc-filter-note",
      "All records shown. These controls filter tables with matching identity columns; snapshot totals and plots retain their own scope."))
}

#' Present full audit messages and scalar details in a readable panel
#' @param checks Saved check rows for one derivative or expanded check.
#' @return HTML panels; large arrays remain in the downloadable checks table.
#' @noRd
qc_check_panels <- function(checks) {
  htmltools::tagList(lapply(seq_len(nrow(checks)), function(i) {
    check <- checks[i, , drop = FALSE]
    details <- if ("details_json" %in% names(checks)) tryCatch(
      jsonlite::fromJSON(check$details_json), error = function(e) NULL) else NULL
    measurements <- if (is.list(details)) details[vapply(details, function(value)
      is.atomic(value) && length(value) == 1L, logical(1))] else list()
    htmltools::tags$section(class = "qc-check",
      htmltools::tags$h4(gsub("_", " ", check$step), " ", qc_status_badge(check$status)),
      htmltools::tags$p(class = "qc-message", check$message),
      if ("warnings" %in% names(checks) && nzchar(check$warnings)) {
        htmltools::tags$p(class = "qc-warning", "Warnings: ", check$warnings)
      },
      htmltools::tags$p(class = "qc-muted", "Audit recorded: ", qc_scalar(check$audited_at),
        " | Check started: ", qc_scalar(check$started_at),
        " | Elapsed seconds: ", qc_scalar(check$elapsed_seconds)),
      if (length(measurements)) htmltools::tags$dl(class = "qc-measurements",
        lapply(names(measurements), function(name) htmltools::tagList(
          htmltools::tags$dt(gsub("_", " ", name)),
          htmltools::tags$dd(as.character(measurements[[name]])))))
    )
  }))
}
