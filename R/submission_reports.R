#' Persist the controller's small report before displaying or enforcing it
#' @param scfg Project configuration.
#' @param run_id Owning controller run, or NULL before run creation.
#' @param report Submission preflight report.
#' @return Report with its saved path when a run directory exists.
#' @noRd
save_submission_preflight <- function(scfg, run_id, report) {
  if (isTRUE(report$dry_run) || is.null(run_id) ||
      !checkmate::test_string(scfg$metadata$log_directory)) return(report)
  report_dir <- file.path(scfg$metadata$log_directory, "runs", run_id)
  if (!dir.exists(report_dir)) return(report)
  report$run_id <- run_id
  report$report_file <- file.path(report_dir, "submission-preflight.json")
  write_json_atomic(report, report$report_file)
  report
}

#' Display a bounded preflight summary without changing tracking state
#' @param report Submission preflight report.
#' @return Report, invisibly.
#' @noRd
print_submission_preflight <- function(report) {
  if (nrow(report$repairs)) {
    cli::cli_alert_info("Automatically repaired {nrow(report$repairs)} stale job record{?s} using confirmed scheduler failures/cancellations.")
    print(utils::head(report$repairs[, c("job_id", "status", "scheduler_status"), drop = FALSE], 20L), row.names = FALSE)
  }
  if (isTRUE(report$scope_deferred)) {
    cli::cli_alert_info("Subject/session checks are deferred until Flywheel sync finishes. The controller will check the discovered scope before submitting subject jobs.")
  }
  if (nrow(report$blockers)) {
    print(utils::head(report$blockers, 20L), row.names = FALSE)
    cli::cli_alert_warning("Submission preflight found {nrow(report$blockers)} relevant blocker(s). Inspect with inspect_project(input, refresh = TRUE); use reconcile_project_jobs() or resolve_project_submission() after confirming the evidence. force does not bypass blockers.")
  } else if (!isTRUE(report$scope_deferred) && !isTRUE(report$dry_run)) {
    cli::cli_alert_success("Submission preflight passed for the resolved request.")
  } else if (isTRUE(report$dry_run)) {
    cli::cli_alert_info("Pending metadata checked. Dry run skipped scheduler confirmation and automatic repairs.")
  }
  if (!is.null(report$report_file)) {
    cli::cli_alert_info("Saved controller submission preflight: {report$report_file}")
  }
  invisible(report)
}

#' Stop a live submission with its complete actionable preflight report
#' @param report Submission preflight report.
#' @return NULL invisibly, or a bg_submission_preflight_error.
#' @noRd
require_submission_ready <- function(report) {
  if (!isTRUE(report$dry_run) && nrow(report$blockers)) {
    message <- paste0("Submission preflight found ", nrow(report$blockers),
      " relevant blocker(s). Inspect with inspect_project(input, refresh = TRUE); ",
      "use reconcile_project_jobs() or resolve_project_submission() after confirming the evidence. ",
      "force does not bypass blockers.")
    stop(structure(list(message = message, call = NULL, report = report),
      class = c("bg_submission_preflight_error", "error", "condition")))
  }
  invisible(NULL)
}

#' Read the controller's final report, falling back to the initial run assessment
#' @param scfg Project configuration.
#' @param run_id One selected run, or NULL when no run is known.
#' @param fallback Optional initial report already read from provenance.
#' @param read_initial Read initial provenance when no controller report exists.
#' @return A normalized saved report, or NULL. No scheduler queries or repairs.
#' @noRd
read_run_submission_preflight <- function(scfg, run_id, fallback = NULL, read_initial = TRUE) {
  if (!is.null(fallback)) {
    for (field in c("blockers", "repairs")) {
      if (!length(fallback[[field]])) fallback[[field]] <- data.frame()
    }
    fallback$source <- "initial"
  }
  if (!checkmate::test_string(run_id)) return(fallback)
  log_dir <- scfg$metadata$log_directory
  if (!checkmate::test_string(log_dir)) return(fallback)
  run_dir <- file.path(log_dir, "runs", run_id)
  controller_file <- file.path(run_dir, "submission-preflight.json")
  initial_file <- file.path(run_dir, "provenance.json")
  controller <- file.exists(controller_file)
  source_file <- if (controller) controller_file else initial_file
  if (!controller && (!read_initial || !file.exists(initial_file))) return(fallback)
  result <- tryCatch({
    record <- jsonlite::read_json(source_file, simplifyVector = TRUE)
    report <- if (controller) record else record$submission_preflight
    if (is.null(report)) return(fallback)
    if (!is.list(report) || !all(c("blockers", "repairs", "scope_deferred") %in% names(report))) {
      stop("Report is missing submission readiness fields.")
    }
    if (!is.null(report$schema_version) &&
        !identical(report$schema_version, "brain-gnomes-submission-preflight-v1")) {
      stop("Unsupported submission preflight report schema.")
    }
    if (!is.null(report$run_id) && !identical(report$run_id, run_id)) {
      stop("Controller report belongs to another run.")
    }
    for (field in c("blockers", "repairs")) {
      if (!length(report[[field]])) report[[field]] <- data.frame()
      if (!is.data.frame(report[[field]])) stop("Invalid report table: ", field)
    }
    report$source <- if (controller) "controller" else "initial"
    report$report_file <- source_file
    report
  }, error = function(error) {
    warning("Could not read submission preflight report: ", source_file, ": ",
            conditionMessage(error), call. = FALSE)
    report <- if (is.null(fallback)) list(blockers = data.frame(), repairs = data.frame(),
      scope_deferred = TRUE) else fallback
    report$read_error <- conditionMessage(error)
    report$report_file <- source_file
    report
  })
  result
}

#' Restrict a saved assessment to a subject without treating it as live status
#' @param report Saved submission preflight report, possibly NULL.
#' @param subject_id Optional normalized subject ID.
#' @param job_id Optional exact scheduler job ID.
#' @return Report containing only that subject's blocker and repair rows.
#' @noRd
scope_submission_preflight <- function(report, subject_id = NULL, job_id = NULL) {
  if (is.null(report) || (is.null(subject_id) && is.null(job_id))) return(report)
  for (field in c("blockers", "repairs")) {
    rows <- report[[field]]
    if (!is.data.frame(rows) || !nrow(rows)) next
    keep <- rep(TRUE, nrow(rows))
    if (!is.null(subject_id)) {
      keep <- keep & if ("sub_id" %in% names(rows)) !is.na(rows$sub_id) & rows$sub_id == subject_id else FALSE
    }
    if (!is.null(job_id)) {
      keep <- keep & if ("job_id" %in% names(rows)) !is.na(rows$job_id) & rows$job_id == job_id else FALSE
    }
    report[[field]] <- rows[keep, , drop = FALSE]
  }
  report
}

#' Display saved submission evidence without claiming it describes current state
#' @param report Saved submission preflight report, possibly NULL.
#' @return Report invisibly.
#' @noRd
print_saved_submission_preflight <- function(report) {
  if (is.null(report)) return(invisible(NULL))
  cli::cli_h3("Recorded submission preflight")
  cli::cli_text("{nrow(report$blockers)} blocker(s) and {nrow(report$repairs)} repair(s) recorded at {value_or_default(report$checked_at, 'an unknown time')}.")
  if (isTRUE(report$scope_deferred)) cli::cli_alert_info("Subject/session checks were still deferred in this report.")
  if (!is.null(report$read_error)) cli::cli_alert_warning("Saved report could not be read: {report$read_error}")
  if (!is.null(report$report_file)) cli::cli_text("Report: {.file {report$report_file}}")
  cli::cli_text("This is saved evidence; current submission outcomes are listed separately.")
  invisible(report)
}
