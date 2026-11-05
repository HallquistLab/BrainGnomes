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

#' Match derivative products to a raw acquisition without requiring output-only entities
#' @param products Derivative inventory rows.
#' @param target One raw acquisition or unresolved workflow-unit row.
#' @return Logical vector identifying compatible product rows.
#' @noRd
qc_coverage_matches <- function(products, target) {
  compatible <- !is.na(products$sub_id) & products$sub_id == target$sub_id &
    ifelse(is.na(products$ses_id), "", products$ses_id) == ifelse(is.na(target$ses_id), "", target$ses_id)
  for (field in c("task", "run", "acquisition", "reconstruction", "direction", "echo", "part")) {
    compatible <- compatible & (is.na(products[[field]]) | is.na(target[[field]]) |
      products[[field]] == target[[field]])
  }
  compatible %in% TRUE
}

#' Assess input selectors using recorded acquisition identities and upstream paths
#' @param specs Postprocessing input specifications, combined as alternatives.
#' @param acquisitions Raw acquisition rows with source_file and BIDS entities.
#' @param upstream Recorded fMRIPrep inventory rows; no filesystem reads are made.
#' @return TRUE for eligible inputs, FALSE for definite exclusions, NA if unresolved.
#' @noRd
qc_selector_applicability <- function(specs, acquisitions, upstream) {
  if (!checkmate::test_character(specs, min.len = 1L, any.missing = FALSE)) {
    return(rep(NA, nrow(acquisitions)))
  }
  # Inspect one alternative at a time. Custom regexes can confirm selection from
  # observed inputs, but a nonmatch cannot prove exclusion of a missing product.
  evaluate_spec <- function(spec) {
    tryCatch({
      pattern <- construct_bids_regex(spec)
      suppressWarnings(grepl(pattern, "", perl = TRUE)) # Validate even without upstream files.
      # A full regex (including regex-valued entities) can depend on output-only
      # fields or cross entity boundaries. Only observed matches prove selection;
      # a missing/nonmatching filename must never prove an acquisition excluded.
      tokens <- strsplit(trimws(spec), "\\s+")[[1L]]
      values <- sub("^[^:]+:", "", tokens)
      if (grepl("^regex:", trimws(spec)) || any(!grepl("^[A-Za-z0-9]+$", values))) {
        selected <- upstream[grepl(pattern, basename(upstream$derivative_file), perl = TRUE), , drop = FALSE]
        return(vapply(seq_len(nrow(acquisitions)), function(index) {
          if (any(qc_coverage_matches(selected, acquisitions[index, , drop = FALSE]))) TRUE else NA
        }, logical(1)))
      }
      keys <- tolower(sub(":.*$", "", tokens))
      fields <- c(sub = "sub_id", subject = "sub_id", ses = "ses_id", session = "ses_id",
        task = "task", acq = "acquisition", acquisition = "acquisition",
        rec = "reconstruction", reconstruction = "reconstruction", dir = "direction",
        direction = "direction", run = "run", echo = "echo")
      # Space, resolution, and description constrain derivative products. They
      # must not exclude raw scans whose task/run identities are eligible.
      keep <- keys %in% names(fields)
      values <- values[keep]
      mapped <- unname(fields[keys[keep]])
      first <- !duplicated(mapped)
      values <- values[first]
      mapped <- mapped[first]
      result <- rep(TRUE, nrow(acquisitions))
      for (index in seq_along(values)) {
        matches <- acquisitions[[mapped[index]]] == values[index]
        # A definite mismatch wins even if another required entity is absent.
        result <- result & matches
      }
      result
    }, error = function(e) rep(NA, nrow(acquisitions)))
  }
  selected <- rep(FALSE, nrow(acquisitions))
  for (spec in specs) selected <- selected | evaluate_spec(spec)
  selected
}

#' Resolve a native stream's acquisition applicability from frozen snapshot settings
#' @param snapshot QC snapshot with optional input_selection metadata.
#' @param acquisitions Raw acquisition and unresolved-unit rows.
#' @param stage Processing stage.
#' @param stream Stream name.
#' @return Logical eligibility vector; unknown selectors remain NA, never exclusions.
#' @noRd
qc_stream_applicability <- function(snapshot, acquisitions, stage, stream) {
  selection <- snapshot$metadata$input_selection
  if (!stage %in% c("postprocess", "extract_rois") || is.null(selection)) {
    # Older snapshots have no selector evidence. Preserve their conservative
    # coverage behavior rather than consulting today's mutable configuration.
    return(rep(TRUE, nrow(acquisitions)))
  }
  parents <- if (stage == "postprocess") stream else selection$extract_rois[[stream]]
  upstream <- snapshot$inventory[snapshot$inventory$stage == "fmriprep", , drop = FALSE]
  result <- rep(FALSE, nrow(acquisitions))
  if (!length(parents)) result[] <- NA
  for (parent in parents) {
    result <- result | qc_selector_applicability(selection$postprocess[[parent]], acquisitions, upstream)
  }
  # A synthetic unit has no acquisition identity to exclude. Keep its missing
  # inputs visible, including expected participants whose data have not arrived.
  result[is.na(acquisitions$source_file)] <- TRUE
  result
}

#' Describe coverage at raw-acquisition and unresolved workflow-unit grain
#' @param snapshot QC snapshot containing acquisitions, derivatives, workflow, and metadata.
#' @return Coverage rows with observational denominators and explicit review reasons.
#' @details Multiple spaces/products do not increase raw acquisition coverage.
#'   Missing output entities are compatible with a raw acquisition (e.g., combined
#'   echoes); this is not a claim about the intended number of spatial products.
#' @noRd
qc_acquisition_coverage <- function(snapshot) {
  identity <- c("sub_id", "ses_id", "task", "run", "acquisition", "reconstruction",
    "direction", "echo", "part")
  template <- as.data.frame(stats::setNames(rep(list(character()), length(identity)), identity))
  template$stage <- template$stream <- template$coverage_status <- template$reason <-
    template$record_ids <- template$source_file <- character()
  template$resolved_outputs <- template$available_outputs <- integer()
  template$attention <- logical()
  inventory <- snapshot$inventory
  workflow <- snapshot$workflow
  stages <- snapshot$metadata$selected_steps
  if (is.null(stages)) stages <- unique(c(workflow$stage, inventory$stage))
  stages <- intersect(stages, c("fmriprep", "mriqc", "aroma", "postprocess", "extract_rois"))
  rows <- list()
  for (stage in stages) {
    streams <- if (stage %in% c("postprocess", "extract_rois")) {
      unique(c(snapshot$metadata$configured_streams[[stage]],
        inventory$stream[inventory$stage == stage], workflow$stream[workflow$stage == stage]))
    } else NA_character_
    if (stage %in% c("postprocess", "extract_rois")) {
      streams <- streams[!is.na(streams) & !qc_legacy_streams(streams, rep(stage, length(streams)),
        snapshot$metadata$configured_streams)]
    }
    for (stream in streams) {
      units <- workflow[workflow$stage == stage &
        ifelse(is.na(workflow$stream), "", workflow$stream) == ifelse(is.na(stream), "", stream), , drop = FALSE]
      products <- inventory[inventory$stage == stage &
        ifelse(is.na(inventory$stream), "", inventory$stream) == ifelse(is.na(stream), "", stream), , drop = FALSE]
      raw <- snapshot$acquisitions
      if (identical(snapshot$metadata$scope, "run") && is.data.frame(snapshot$metadata$work_units) &&
          nrow(snapshot$metadata$work_units)) {
        raw$stage <- rep(stage, nrow(raw))
        raw$stream <- rep(stream, nrow(raw))
        raw <- raw[qc_retry_rows(raw, snapshot$metadata$work_units), , drop = FALSE]
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
      applicable <- qc_stream_applicability(snapshot, targets, stage, stream)
      for (index in seq_len(nrow(targets))) {
        target <- targets[index, , drop = FALSE]
        matched <- products[qc_coverage_matches(products, target), , drop = FALSE]
        tracked <- units[!is.na(units$sub_id) & units$sub_id == target$sub_id &
          (is.na(units$ses_id) | (!is.na(target$ses_id) & units$ses_id == target$ses_id)), , drop = FALSE]
        available <- sum(matched$output_status == "available")
        status <- if (isFALSE(applicable[index])) "excluded_by_configuration" else if (is.na(target$source_file)) "inputs_not_observed" else if (nrow(matched)) {
          if (available == nrow(matched)) "observed" else "missing_outputs"
        } else if (is.na(applicable[index])) "selection_unresolved" else if (stage == "aroma") "workflow_only" else "unresolved_outputs"
        problem <- status %in% c("inputs_not_observed", "missing_outputs", "unresolved_outputs", "selection_unresolved")
        workflow_problem <- !isFALSE(applicable[index]) && any(tracked$status %in% c("NOT_TRACKED", "FAILED", "BLOCKED", "CANCELLED", "UNKNOWN"))
        reason <- switch(status,
          excluded_by_configuration = "Acquisition excluded by this stream's configured input selectors; no output expected",
          selection_unresolved = "Input selection cannot be established from recorded acquisition/upstream evidence; not a confirmed missing output",
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
#' @param snapshot Saved QC snapshot, including older snapshots without coverage.
#' @return Existing coverage or a derived observational coverage table.
#' @noRd
qc_snapshot_coverage <- function(snapshot) {
  if (is.data.frame(snapshot$coverage)) snapshot$coverage else qc_acquisition_coverage(snapshot)
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
    rows$reason <- paste0(gaps$stage, ifelse(is.na(gaps$stream), "", paste0(" / ", gaps$stream)),
      ": ", gaps$reason)
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
