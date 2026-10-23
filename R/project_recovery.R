#' Recover stale active job records using scheduler accounting
#'
#' Preview scheduler observations with the default `dry_run = TRUE`. With
#' `dry_run = FALSE`, only confirmed scheduler failures and cancellations update
#' active database rows. Missing accounting records and unavailable schedulers
#' never authorize retries. Scheduler completion alone does not establish that
#' required outputs or completion manifests are valid; these rows are returned
#' with action `VERIFY_OUTPUTS` and remain unchanged.
#'
#' The recorded end time for a recovered failure is the reconciliation time when
#' no worker end time exists. This function submits and cancels no jobs.
#' Unbound uncertain submissions require [resolve_project_submission()].
#'
#' Unlike a [run_project()] preflight, this explicit operation reviews all active
#' tracked jobs in the project, not just the scope of a new request. Its preview
#' contacts the scheduler even though it does not update SQLite. Neither mode
#' reads imaging files or verifies completed outputs.
#' Unknown legacy tracking states and dependency-blocked allocations without
#' confirmed scheduler termination are included. For a dependency-blocked row,
#' confirmed failure/cancellation records `scheduler_terminal_state` but retains
#' logical status `FAILED_BY_EXT`; run-based retries still require
#' `include_blocked = TRUE` to select that work.
#'
#' For future runs, tracked submission errors stop the submission loop at the
#' first failure, retaining scheduler stdout and stderr beside the job manifest.
#' Explicit supported Slurm policy/request rejections are retryable; transport
#' errors remain uncertain. Retries automatically reconcile matching active
#' attempts against confirmed scheduler failures before checking duplicates.
#'
#' Keep `logs/runs` with the tracking database: it contains the evidence needed
#' for recovery. For input problems, finish conversion before preprocessing,
#' run [run_bids_validation()] and inspect its report before launching a cohort.
#' Changing the output directory does not repair shared BIDS inputs. Never
#' remove or rename anatomical files solely to suppress validator errors;
#' determine which acquisitions and derived files should be retained first.
#' @param input Project configuration, YAML file, or project directory.
#' @param dry_run Preview changes without updating SQLite. Defaults to `TRUE`.
#' @return A data frame with database and scheduler states, proposed actions,
#'   and an `applied` flag for each active tracked job. `UPDATE_TERMINAL_FAILURE`
#'   proposes a confirmed failure/cancellation repair; `CONFIRM_OUTCOME` and
#'   `VERIFY_OUTPUTS` require further investigation and leave records unchanged.
#'   Other records have action `UNCHANGED`.
#' @examples
#' \dontrun{
#' scfg <- load_project("/project/my_study")
#' recovery <- reconcile_project_jobs(scfg)
#' recovery[, c("job_id", "status", "scheduler_status", "action", "applied")]
#' # After reviewing the observations, explicitly apply confirmed repairs.
#' reconcile_project_jobs(scfg, dry_run = FALSE)
#' }
#' @seealso [inspect_project()] for status, [diagnose_project()] for recovery
#'   guidance, and [resolve_project_submission()] for uncertain acceptance.
#'   See `vignette("diagnosing_pipeline", package = "BrainGnomes")` for the
#'   recovery decision guide and automatic preflight workflow.
#' @export
reconcile_project_jobs <- function(input = getwd(), dry_run = TRUE) {
  checkmate::assert_flag(dry_run)
  scfg <- project_config_from_input(input)
  reconcile_tracked_jobs(scfg$metadata$sqlite_db, dry_run = dry_run)
}

#' Resolve an uncertain submission after confirming its outcome
#'
#' Supply either the confirmed scheduler allocation ID or a rejection reason
#' describing authoritative evidence that no job was accepted. Queue absence,
#' elapsed time, and missing logs are not evidence of rejection. This is an
#' explicit administrative recovery operation; it never guesses an outcome.
#' Binding an allocation preserves any worker-reported status. Rejection is
#' allowed only for `UNKNOWN` or `SUBMITTING` attempts with no bound job.
#'
#' There is no dry-run mode: this function updates the database immediately.
#' Supply exactly one of `scheduler_job_id` and `rejection_reason`, for the
#' exact recorded attempt. Binding does not verify outputs or authorize a
#' duplicate launch. If acceptance cannot be confirmed or ruled out, leave the
#' attempt unresolved and seek scheduler support. Do not delete tracking rows
#' or move to a new output directory to bypass the submission guard.
#'
#' After resolution, inspect the current state again. Correct confirmed
#' rejection causes before repeating the intended [run_project()] request.
#' An unbound attempt may have no tracked job for [retry_project_run()] to select.
#' @param input Project configuration, YAML file, or project directory.
#' @param contract_id Exact UUID from `inspect_project(input)$submissions`.
#' @param scheduler_job_id Confirmed root allocation ID in the recorded scheduler's
#'   format, or `NULL`. Slurm task IDs and diagnostic text are not allocation IDs;
#'   TORQUE element IDs are canonicalized to their array root.
#' @param rejection_reason Evidence of confirmed rejection, or `NULL`.
#' @return The resolved submission record, invisibly.
#' @examples
#' \dontrun{
#' scfg <- load_project("/project/my_study")
#' diagnosis <- diagnose_project(scfg, interactive = FALSE)
#' diagnosis$submission_issues
#' contract_id <- "REPLACE_WITH_EXACT_CONTRACT_UUID"
#'
#' # Only after confirming this exact attempt was accepted:
#' resolve_project_submission(scfg, contract_id, scheduler_job_id = "12345")
#' }
#' \dontrun{
#' # Alternative to binding: only after authoritative confirmation of rejection.
#' # Do not run this after binding the same attempt above.
#' resolve_project_submission(scfg, contract_id,
#'   rejection_reason = "Administrator confirmed this exact request was rejected.")
#' }
#' @seealso [diagnose_project()] for submission issues and
#'   [reconcile_project_jobs()] for known scheduler allocations.
#'   See `vignette("diagnosing_pipeline", package = "BrainGnomes")` for
#'   evidence requirements and the end-to-end recovery workflow.
#' @export
resolve_project_submission <- function(input = getwd(), contract_id,
                                       scheduler_job_id = NULL,
                                       rejection_reason = NULL) {
  checkmate::assert_string(contract_id, min.chars = 1L)
  checkmate::assert_string(scheduler_job_id, min.chars = 1L, null.ok = TRUE)
  checkmate::assert_string(rejection_reason, min.chars = 1L, null.ok = TRUE)
  if (is.null(scheduler_job_id) == is.null(rejection_reason)) {
    stop("Supply exactly one confirmed scheduler_job_id or rejection_reason.", call. = FALSE)
  }
  scfg <- project_config_from_input(input)
  sqlite_db <- scfg$metadata$sqlite_db
  if (!is.null(scheduler_job_id)) {
    con <- job_submission_connect(sqlite_db)
    record <- tryCatch(DBI::dbGetQuery(con,
      "SELECT tracking_metadata FROM job_submission_attempts WHERE contract_id = ?",
      params = list(contract_id)), finally = DBI::dbDisconnect(con))
    if (nrow(record) != 1L) stop("Submission attempt is not registered: ", contract_id, call. = FALSE)
    metadata <- jsonlite::fromJSON(record$tracking_metadata[[1L]])
    scheduler <- value_or_default(metadata$scheduler,
      value_or_default(scfg$compute_environment$scheduler, "local"))
    executable <- if (scheduler %in% c("slurm", "sbatch")) "sbatch" else if (
      scheduler %in% c("torque", "qsub")) "qsub" else "sh"
    canonical <- sub("\\[[0-9]+\\]", "[]", scheduler_job_id)
    valid <- if (executable == "qsub") {
      grepl("^[0-9]+(\\[\\])?([.][A-Za-z0-9_-]+)*$", canonical)
    } else grepl("^[0-9]+$", canonical)
    if (!valid || grepl("[[:space:]]", canonical)) {
      stop("Invalid scheduler allocation ID; supply the confirmed root allocation identifier, not a task ID or diagnostic text.", call. = FALSE)
    }
    bind_job_submission(sqlite_db, contract_id, scheduler_job_id)
  } else {
    if (!nzchar(trimws(rejection_reason))) {
      stop("A rejection reason must describe authoritative evidence, not blank text.", call. = FALSE)
    }
    job_submission_transaction(sqlite_db, function(con) {
      changed <- DBI::dbExecute(con, paste(
        "UPDATE job_submission_attempts SET submission_state = 'REJECTED',",
        "submission_detail = COALESCE(submission_detail || char(10), '') || ? WHERE contract_id = ?",
        "AND submission_state IN ('UNKNOWN','SUBMITTING') AND scheduler_job_id IS NULL",
        "AND NOT EXISTS (SELECT 1 FROM job_tracking WHERE contract_id = ?)"
      ), params = list(paste0("Manual resolution at ", job_contract_timestamp(), ": ",
                             rejection_reason), contract_id, contract_id))
      if (changed != 1L) stop("Only an unbound uncertain submission can be rejected.", call. = FALSE)
    })
  }
  records <- inspect_job_submissions(sqlite_db)
  invisible(records[records$contract_id == contract_id, , drop = FALSE])
}
