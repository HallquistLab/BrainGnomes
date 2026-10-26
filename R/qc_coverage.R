#' Identify filename-shaped historical stream labels without guessing attribution
#' @param streams Recorded stream labels.
#' @param stages Corresponding processing stages.
#' @param configured Named list of configured stream names by stage.
#' @return Logical vector; explicitly configured names are never reclassified.
#' @noRd
qc_legacy_streams <- function(streams, stages, configured = NULL) {
  vapply(seq_along(streams), function(index) {
    value <- streams[index]
    !is.na(value) && stages[index] %in% c("postprocess", "extract_rois") &&
      !value %in% configured[[stages[index]]] &&
      grepl("^sub-|_desc-|_bold(\\.|$)", value)
  }, logical(1))
}

#' Describe coverage at raw-acquisition and unresolved workflow-unit grain
#' @param x QC snapshot containing acquisitions, derivatives, workflow, and metadata.
#' @return Coverage rows with observational denominators and explicit review reasons.
#' @details Multiple spaces/products do not increase raw acquisition coverage.
#'   Missing output entities are compatible with a raw acquisition (e.g., combined
#'   echoes); this is not a claim about the intended number of spatial products.
#' @noRd
qc_acquisition_coverage <- function(x) {
  identity <- c("sub_id", "ses_id", "task", "run", "acquisition", "reconstruction",
    "direction", "echo", "part")
  template <- as.data.frame(stats::setNames(rep(list(character()), length(identity)), identity))
  template$stage <- template$stream <- template$coverage_status <- template$reason <-
    template$record_ids <- template$source_file <- character()
  template$resolved_outputs <- template$available_outputs <- integer()
  template$attention <- logical()
  inventory <- x$inventory
  workflow <- x$workflow
  stages <- x$metadata$selected_steps
  if (is.null(stages)) stages <- unique(c(workflow$stage, inventory$stage))
  stages <- intersect(stages, c("fmriprep", "mriqc", "aroma", "postprocess", "extract_rois"))
  rows <- list()
  for (stage in stages) {
    streams <- if (stage %in% c("postprocess", "extract_rois")) {
      unique(c(x$metadata$configured_streams[[stage]],
        inventory$stream[inventory$stage == stage], workflow$stream[workflow$stage == stage]))
    } else NA_character_
    if (stage %in% c("postprocess", "extract_rois")) {
      streams <- streams[!is.na(streams) & !qc_legacy_streams(streams, rep(stage, length(streams)),
        x$metadata$configured_streams)]
    }
    for (stream in streams) {
      units <- workflow[workflow$stage == stage &
        ifelse(is.na(workflow$stream), "", workflow$stream) == ifelse(is.na(stream), "", stream), , drop = FALSE]
      products <- inventory[inventory$stage == stage &
        ifelse(is.na(inventory$stream), "", inventory$stream) == ifelse(is.na(stream), "", stream), , drop = FALSE]
      raw <- x$acquisitions
      if (identical(x$metadata$scope, "run") && is.data.frame(x$metadata$work_units) &&
          nrow(x$metadata$work_units)) {
        raw$stage <- rep(stage, nrow(raw))
        raw$stream <- rep(stream, nrow(raw))
        raw <- raw[qc_retry_rows(raw, x$metadata$work_units), , drop = FALSE]
      }
      # Units without acquisitions still need a visible denominator. They must
      # not acquire fabricated scan identities or a false zero-of-zero pass.
      absent <- units[!is.na(units$sub_id), , drop = FALSE]
      absent <- absent[!vapply(seq_len(nrow(absent)), function(index) {
        any(raw$sub_id == absent$sub_id[index] & (is.na(absent$ses_id[index]) |
          (!is.na(raw$ses_id) & raw$ses_id == absent$ses_id[index])), na.rm = TRUE)
      }, logical(1)), , drop = FALSE]
      for (field in setdiff(identity, names(raw))) raw[[field]] <- rep(NA_character_, nrow(raw))
      for (field in setdiff(identity, names(absent))) absent[[field]] <- rep(NA_character_, nrow(absent))
      absent$source_file <- rep(NA_character_, nrow(absent))
      targets <- unique(rbind(raw[c(identity, "source_file")], absent[c(identity, "source_file")]))
      for (index in seq_len(nrow(targets))) {
        target <- targets[index, , drop = FALSE]
        compatible <- !is.na(products$sub_id) & products$sub_id == target$sub_id &
          ifelse(is.na(products$ses_id), "", products$ses_id) == ifelse(is.na(target$ses_id), "", target$ses_id)
        for (field in setdiff(identity, c("sub_id", "ses_id"))) {
          compatible <- compatible & (is.na(products[[field]]) | is.na(target[[field]]) |
            products[[field]] == target[[field]])
        }
        matched <- products[which(compatible), , drop = FALSE]
        tracked <- units[!is.na(units$sub_id) & units$sub_id == target$sub_id &
          (is.na(units$ses_id) | (!is.na(target$ses_id) & units$ses_id == target$ses_id)), , drop = FALSE]
        available <- sum(matched$output_status == "available")
        status <- if (is.na(target$source_file)) "inputs_not_observed" else if (nrow(matched)) {
          if (available == nrow(matched)) "observed" else "missing_outputs"
        } else if (stage == "aroma") "workflow_only" else "unresolved_outputs"
        problem <- status %in% c("inputs_not_observed", "missing_outputs", "unresolved_outputs")
        workflow_problem <- any(tracked$status %in% c("NOT_TRACKED", "FAILED", "BLOCKED", "CANCELLED", "UNKNOWN"))
        reason <- switch(status,
          inputs_not_observed = "No raw BOLD acquisition observed for this configured unit; output scope unresolved",
          missing_outputs = "Some resolved output paths are missing or empty",
          unresolved_outputs = "Raw BOLD present but no derivative paths resolved for this stage/stream",
          workflow_only = "AROMA file coverage is not resolved; consult recorded workflow evidence",
          observed = "Resolved output paths available; not proof of workflow completion or scientific quality")
        if (workflow_problem) reason <- paste(reason, paste("Workflow", paste(unique(tracked$status), collapse = ", ")), sep = "; ")
        rows[[length(rows) + 1L]] <- cbind(target[identity], stage = stage, stream = stream,
          coverage_status = status, reason = reason, record_ids = paste(matched$record_id, collapse = "\n"),
          source_file = target$source_file, resolved_outputs = nrow(matched), available_outputs = available,
          attention = problem || workflow_problem)
      }
    }
  }
  qc_bind_rows(rows, template)
}

#' Supply coverage for saved snapshots without reading or changing study files
#' @param x Saved QC snapshot, including older snapshots without coverage.
#' @return Existing coverage or a derived observational coverage table.
#' @noRd
qc_snapshot_coverage <- function(x) {
  if (is.data.frame(x$coverage)) x$coverage else qc_acquisition_coverage(x)
}

#' Add unresolved coverage and submission problems to the derivative review queue
#' @param inventory Normalized derivative rows.
#' @param coverage Coverage rows at their recorded acquisition/unit grain.
#' @param metadata Snapshot metadata, including optional saved submission state.
#' @return Uniform attention rows without inventing derivative associations.
#' @noRd
qc_review_queue <- function(inventory, coverage, metadata) {
  result <- qc_attention_groups(inventory)
  gaps <- coverage[coverage$attention %in% TRUE, , drop = FALSE]
  if (nrow(gaps)) {
    rows <- result[rep(NA_integer_, nrow(gaps)), , drop = FALSE]
    for (field in intersect(names(rows), names(gaps))) rows[[field]] <- gaps[[field]]
    rows$category <- rep("Coverage/workflow unresolved", nrow(gaps))
    rows$reason <- gaps$reason
    rows$products <- gaps$resolved_outputs
    result <- rbind(result, rows)
  }
  submission <- metadata$submission
  if (identical(metadata$scope, "run") &&
      (!identical(submission$state, "submitted") || !isTRUE(submission$acknowledged_jobs > 0L))) {
    row <- result[NA_integer_, , drop = FALSE]
    row$category <- "Submission unconfirmed"
    row$reason <- "Saved request has incomplete or unavailable submission acknowledgements; existing files do not confirm execution. Inspect run provenance and job recovery before resubmitting."
    row$record_ids <- ""
    row$products <- 0L
    result <- rbind(result, row)
  }
  rownames(result) <- NULL
  result
}

#' Render prominent snapshot scope and execution caveats without network access
#' @param metadata Saved metadata.
#' @return Escaped HTML banner identifying project/run scope and evidence access.
#' @noRd
qc_scope_banner <- function(metadata) {
  run <- identical(metadata$scope, "run")
  htmltools::tags$div(class = "qc-snapshot-banner",
    htmltools::tags$strong(if (run) paste("Saved run:", metadata$run_id) else "Project-wide snapshot"),
    if (run) htmltools::tags$p("Recorded request: ", metadata$recorded_at,
      " | Submission: ", value_or_default(metadata$submission$state, "unknown"),
      " | Acknowledged jobs: ", if (is.null(metadata$submission$acknowledged_jobs) ||
        is.na(metadata$submission$acknowledged_jobs)) "unknown" else metadata$submission$acknowledged_jobs,
      " | Tracked at submission: ", if (is.null(metadata$submission$tracked_jobs) ||
        is.na(metadata$submission$tracked_jobs)) "unknown" else metadata$submission$tracked_jobs),
    htmltools::tags$p("Files observed: ", metadata$created_at,
      ". File availability is not evidence that this attempt executed or that all expected acquisitions were processed."),
    htmltools::tags$details(htmltools::tags$summary("Evidence access and privacy"),
      htmltools::tags$p("Download and open this HTML locally for offline review. Keep TSV/RDS/JSON files beside it for full downloads. Original reports and images require the source filesystem; their interactive dependencies may require internet. The dashboard does not contact those services automatically. Keep participant identifiers and source paths private.")))
}
