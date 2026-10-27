#' Resolve the configuration and exact scope used by a QC snapshot
#'
#' @param scfg Resolved project configuration.
#' @param run_id Optional saved run ID or `latest`.
#' @param subjects Additional project expectations; disallowed for a saved run.
#' @return Configuration, scope, selected stages, and provenance metadata.
#' @noRd
qc_collection_context <- function(scfg, run_id, subjects) {
  checkmate::assert_string(run_id, null.ok = TRUE, min.chars = 1L)
  context <- list(config = scfg, subjects = subjects, run_id = NULL,
    steps = status_spec(scfg)$steps, recorded_at = NULL, provenance_file = NULL,
    work_units = NULL, submission = NULL)
  if (is.null(run_id)) return(context)
  if (!is.null(subjects)) stop("subjects adds project expectations; do not combine it with run_id.", call. = FALSE)
  saved <- get_run_provenance(scfg, run_id)
  if (!identical(saved$execution$scope_status, "resolved")) {
    stop("The saved run's participant scope is unresolved. Collect a project snapshot or wait for subject discovery.", call. = FALSE)
  }
  # JSON serializes an empty participant table as []; retain its typed schema
  # for valid runs containing only project-level work.
  if (is.list(saved$execution$subjects) && !length(saved$execution$subjects)) {
    saved$execution$subjects <- data.frame(sub_id = character(), ses_id = character())
  }
  if (!is.list(saved$configuration$values) || !is.data.frame(saved$execution$subjects)) {
    stop("The saved run lacks a usable configuration or participant scope.", call. = FALSE)
  }
  checkmate::assert_names(names(saved$execution$subjects), must.include = c("sub_id", "ses_id"))
  # Use the recorded values, not the current YAML or a mutable snapshot file.
  cfg <- project_config_from_input(structure(saved$configuration$values, class = "bg_project_cfg"))
  steps <- as.character(saved$request$steps)
  for (stage in supported_project_steps()) cfg[[stage]]$enable <- stage %in% steps
  for (stage in c("postprocess", "extract_rois")) {
    field <- if (stage == "postprocess") "postprocess_streams" else "extract_streams"
    selected <- as.character(saved$request[[field]])
    if (length(selected)) cfg[[stage]] <- cfg[[stage]][c("enable", selected)]
  }
  # ROI filenames are resolved through the saved postprocessing streams even
  # when extraction alone was selected. These records are labelled inputs.
  if ("extract_rois" %in% steps) cfg$postprocess$enable <- TRUE
  context$config <- cfg
  context$subjects <- saved$execution$subjects
  context$run_id <- saved$run_id
  context$steps <- steps
  context$recorded_at <- saved$recorded_at
  context$provenance_file <- saved$provenance_file
  context$work_units <- normalize_retry_work_units(saved$request$work_units)
  recorded_state <- qc_scalar(saved$state)
  context$submission <- list(state = if (is.na(recorded_state)) "unknown" else recorded_state,
    acknowledged_jobs = if (is.null(saved$submission$submitted_job_ids)) NA_integer_ else
      length(unique(unlist(saved$submission$submitted_job_ids, use.names = FALSE))),
    tracked_jobs = if (is.data.frame(saved$submission$tracked_jobs)) nrow(saved$submission$tracked_jobs) else
      if (is.list(saved$submission$tracked_jobs) && !length(saved$submission$tracked_jobs)) 0L else NA_integer_)
  context
}

#' Select acquisition and derivative rows belonging to a saved run
#'
#' @param rows Rows with BIDS identities and optional stage/stream columns.
#' @param context Collection context from qc_collection_context().
#' @param subject_evidence Allow participant-wide reports to support sessions.
#' @return Logical vector; project collection retains every row.
#' @noRd
qc_scope_rows <- function(rows, context, subject_evidence = FALSE) {
  if (is.null(context$run_id)) return(rep(TRUE, nrow(rows)))
  scope <- context$subjects
  keep <- rep(FALSE, nrow(rows))
  for (i in seq_len(nrow(scope))) {
    participant <- sub("^sub-", "", as.character(scope$sub_id[i]))
    session <- if ("ses_id" %in% names(scope)) sub("^ses-", "", as.character(scope$ses_id[i])) else NA_character_
    # Realized run scope records individual inputs. A missing session means a
    # sessionless input, as in retry_includes_unit(), rather than all sessions.
    session_matches <- ifelse(is.na(rows$ses_id), "", rows$ses_id) ==
      ifelse(is.na(session), "", session)
    keep <- keep | (!is.na(rows$sub_id) & rows$sub_id == participant &
      (session_matches | (subject_evidence & is.na(rows$ses_id))))
  }
  if ("stage" %in% names(rows)) {
    supporting <- c("fmriprep", if ("extract_rois" %in% context$steps) "postprocess")
    keep <- keep & rows$stage %in% c(context$steps, supporting)
    for (stage in c("postprocess", "extract_rois")) {
      selected <- setdiff(names(context$config[[stage]]), "enable")
      keep[rows$stage == stage] <- keep[rows$stage == stage] &
        !is.na(rows$stream[rows$stage == stage]) & rows$stream[rows$stage == stage] %in% selected
    }
    units <- context$work_units
    if (is.data.frame(units) && nrow(units)) {
      # Exact retry units must not become a subject x session x stream product.
      for (stage in context$steps) {
        indices <- which(rows$stage == stage)
        stage_units <- units[units$stage == stage, , drop = FALSE]
        allowed <- qc_retry_rows(rows[indices, , drop = FALSE], stage_units)
        keep[indices] <- keep[indices] & allowed
      }
    }
  }
  keep[is.na(keep)] <- FALSE
  keep
}

#' Match exact retry identities using the production stage-specific scope rules
#' @param rows Workflow or derivative rows with stage and participant identities.
#' @param units Saved exact work units for the selected stages.
#' @return Logical selection; subject-wide stages intentionally ignore sessions.
#' @noRd
qc_retry_rows <- function(rows, units) {
  vapply(seq_len(nrow(rows)), function(index) {
    row <- rows[index, , drop = FALSE]
    if (is.na(row$sub_id)) return(any(units$stage == row$stage & is.na(units$sub_id)))
    retry_includes_unit(units, row$stage, row$sub_id, row$ses_id,
      if ("stream" %in% names(row)) row$stream else NA_character_)
  }, logical(1))
}

#' Preserve unknown diagnostic requirements rather than assuming a setting
#' @param value A saved logical setting or NULL.
#' @return TRUE, FALSE, or NA when the setting is unknown.
#' @noRd
qc_required <- function(value) {
  if (isTRUE(value)) TRUE else if (isFALSE(value)) FALSE else NA
}

#' Distinguish disabled diagnostics from missing required evidence
#' @param status Observed diagnostic status.
#' @param required Logical or unknown requirement.
#' @return Status preserving any existing evidence, including historical audits.
#' @noRd
qc_applicability <- function(status, required) {
  if (status != "unavailable") return(status)
  if (isTRUE(required)) "missing" else if (isFALSE(required)) "not_applicable" else "unavailable"
}

#' Explain why each derivative needs attention
#' @param inventory Derivative inventory including observed and workflow states.
#' @return One readable, semicolon-separated explanation per record.
#' @noRd
qc_attention_reasons <- function(inventory) {
  vapply(seq_len(nrow(inventory)), function(i) {
    row <- inventory[i, , drop = FALSE]
    reasons <- character()
    if (row$output_status != "available") reasons <- c(reasons, paste("Output", gsub("_", " ", row$output_status)))
    if (row$validation_status %in% c("failed", "error", "invalid", "ambiguous", "missing", "not_run")) {
      reasons <- c(reasons, paste("Saved audit", gsub("_", " ", row$validation_status)))
    }
    if (row$diagnostics_status %in% c("invalid", "roi_loss", "missing")) {
      reasons <- c(reasons, paste("Diagnostics", gsub("_", " ", row$diagnostics_status)))
    }
    if (isTRUE(row$audit_warnings)) reasons <- c(reasons, "Saved audit has warnings")
    if (row$association_status == "shared_destination") reasons <- c(reasons, "Multiple streams share this output")
    if (row$workflow_status %in% c("FAILED", "BLOCKED", "CANCELLED")) reasons <- c(reasons, paste("Workflow", tolower(row$workflow_status)))
    paste(reasons, collapse = "; ")
  }, character(1))
}

#' Describe selected processing settings in human-readable rows
#' @param scfg Configuration used to resolve expectations.
#' @param steps Selected workflow stages.
#' @return Table of stage, stream, setting, and recorded value.
#' @noRd
qc_report_settings <- function(scfg, steps) {
  rows <- list()
  for (stage in intersect(steps, c("postprocess", "extract_rois"))) {
    for (stream in setdiff(names(scfg[[stage]]), "enable")) {
      cfg <- scfg[[stage]][[stream]]
      values <- if (stage == "postprocess") list(
        "Input selection" = cfg$input_regex, "Output description" = cfg$bids_desc,
        "Processing order" = dry_run_postprocess_sequence(cfg),
        "Smoothing enabled" = cfg$spatial_smooth$enable,
        "Smoothing kernel (mm)" = cfg$spatial_smooth$fwhm_mm,
        "Temporal filtering enabled" = cfg$temporal_filter$enable,
        "High-pass frequency (Hz)" = cfg$temporal_filter$high_pass_hz,
        "Low-pass frequency (Hz)" = cfg$temporal_filter$low_pass_hz,
        "Intensity normalization enabled" = cfg$intensity_normalize$enable,
        "Intensity normalization mode" = cfg$intensity_normalize$mode,
        "Validation enabled" = cfg$validate_postproc_steps,
        "Scrubbing enabled" = cfg$scrubbing$enable,
        "Apply scrubbing to volumes" = cfg$scrubbing$apply,
        "Interpolate scrubbed volumes" = cfg$scrubbing$interpolate,
        "Scrubbing rule" = cfg$scrubbing$expression) else list(
        "Input streams" = cfg$input_streams, "Atlases" = cfg$atlases,
        "Minimum voxels per ROI" = cfg$min_vox_per_roi,
        "Save diagnostics" = cfg$save_diagnostics)
      for (name in names(values)) {
        value <- values[[name]]
        if (!length(value)) next
        rows[[length(rows) + 1L]] <- data.frame(stage = stage, stream = stream,
          setting = name, value = paste(value, collapse = ", "))
      }
    }
  }
  qc_bind_rows(rows, data.frame(stage = character(), stream = character(),
    setting = character(), value = character()))
}

#' Collect selected scalar validation measurements without parsing prose
#' @param state Collector environment containing metric rows.
#' @param record Derivative record ID.
#' @param check Saved validation check with structured details.
#' @param path Original audit path.
#' @return NULL invisibly; full structured details are also preserved in checks.
#' @noRd
qc_collect_check_metrics <- function(state, record, check, path) {
  definitions <- list(
    apply_mask = c(max_absolute_error = "Maximum absolute mask replay error.",
      n_mismatched = "Number of mismatched values in sampled mask replay."),
    spatial_smooth = c(pre_fwhm_mm = "Estimated smoothness before smoothing.",
      post_fwhm_mm = "Estimated smoothness after smoothing.",
      post_expected_mm = "Expected total smoothness from the saved calibration.",
      delta_diff_mm = "Observed minus expected smoothness increase.",
      tolerance_mm = "Saved smoothness comparison tolerance."),
    intensity_normalize = c(value_relative_error = "Maximum relative intensity multiplication error."),
    temporal_filter = c(avg_reduction_outside_db = "Mean attenuation outside the requested band.",
      avg_passband_change_db = "Mean change within the requested passband.",
      fraction_voxels_outside_reduced = "Fraction of sampled voxels with reduced outside-band power.",
      fraction_voxels_passband_preserved = "Fraction of sampled voxels meeting the passband preservation rule."))
  step <- qc_scalar(check$step)
  if (is.na(step) || !is.list(check$details)) return(invisible(NULL))
  fields <- definitions[[step]]
  for (name in intersect(names(fields), names(check$details))) {
    value <- suppressWarnings(as.numeric(qc_scalar(check$details[[name]])))
    if (!is.finite(value)) next
    unit <- if (grepl("_mm$", name)) "mm" else if (grepl("_db$", name)) "dB" else if (name == "n_mismatched") "values" else "dimensionless"
    n <- value_or_default(check$details$volumes_compared,
      value_or_default(check$details$volumes_used, check$details$n_voxels_used))
    qc_add_metric(state, record, paste(step, name, sep = ": "), value, unit,
      paste(fields[[name]], "Saved computational audit; does not establish image quality."),
      path, if (checkmate::test_number(n)) as.integer(n) else NA_integer_, NA_integer_)
  }
  invisible(NULL)
}
