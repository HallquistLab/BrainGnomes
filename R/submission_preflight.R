#' Read only pending submission identities and active job metadata
#' @param sqlite_db Existing project tracking database, possibly absent.
#' @return Lists of active jobs and uncertain submissions, without job blobs.
#' @noRd
read_submission_preflight_records <- function(sqlite_db) {
  result <- list(jobs = data.frame(), submissions = data.frame())
  if (!checkmate::test_file_exists(sqlite_db)) return(result)
  con <- DBI::dbConnect(RSQLite::SQLite(), sqlite_db, flags = RSQLite::SQLITE_RO,
                        synchronous = NULL)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  RSQLite::sqliteSetBusyHandler(con, 10000L)
  has_jobs <- DBI::dbExistsTable(con, "job_tracking")
  if (has_jobs) {
    fields <- DBI::dbListFields(con, "job_tracking")
    columns <- c("job_id", "job_name", "contract_id", "unit_key", "job_role",
                 "scheduler", "status", "stage", "stream", "sub_id", "ses_id", "sequence_id")
    selected <- ifelse(columns %in% fields, columns, paste("NULL AS", columns))
    result$jobs <- DBI::dbGetQuery(con, paste("SELECT", paste(selected, collapse = ","),
      "FROM job_tracking WHERE", pending_tracked_job_clause(fields)))
  }
  if (DBI::dbExistsTable(con, "job_submission_attempts")) {
    # Project scalar JSON fields in SQLite; never deserialize scheduler objects,
    # read manifests, or load completed submission history for this gate.
    metadata <- c("stage", "stream", "sub_id", "ses_id", "job_name", "scheduler")
    selected <- paste0("CASE WHEN json_valid(a.tracking_metadata) THEN CASE WHEN ",
      "json_type(a.tracking_metadata, '$.", metadata, "') IN ('text','integer','real') ",
      "THEN json_extract(a.tracking_metadata, '$.", metadata, "') END END AS ", metadata)
    orphan <- if (has_jobs && "contract_id" %in% fields) paste(
      "OR (a.submission_state = 'ACCEPTED' AND NOT EXISTS",
      "(SELECT 1 FROM job_tracking job WHERE job.contract_id = a.contract_id",
      "AND job.job_id = a.scheduler_job_id))"
    ) else if (!has_jobs) "OR a.submission_state = 'ACCEPTED'" else ""
    result$submissions <- DBI::dbGetQuery(con, paste(
      "SELECT a.contract_id, a.unit_key, a.job_role, a.submission_state AS status,",
      "a.scheduler_job_id AS job_id, a.run_id AS sequence_id,", paste(selected, collapse = ","),
      "FROM job_submission_attempts a WHERE a.submission_state IN ('UNKNOWN','SUBMITTING')", orphan
    ))
  }
  result
}

#' Recover scalar scope from modern unit keys and legacy scheduler names
#' @param records Pending submission or active tracking metadata.
#' @return Records with normalized stage, subject, session and stream columns.
#' @noRd
normalize_submission_preflight_records <- function(records) {
  if (!nrow(records)) return(records)
  fields <- c("stage", "stream", "sub_id", "ses_id", "job_name", "unit_key",
              "job_role", "contract_id", "job_id", "scheduler", "sequence_id")
  for (field in fields) {
    if (!field %in% names(records)) records[[field]] <- NA_character_
    records[[field]] <- as.character(records[[field]])
    records[[field]][!is.na(records[[field]]) & !nzchar(records[[field]])] <- NA_character_
  }
  for (index in seq_len(nrow(records))) {
    key <- records$unit_key[[index]]
    if (is.na(key)) next
    parts <- strsplit(paste0(key, "::"), "::", fixed = TRUE)[[1L]]
    mapping <- if (parts[[1L]] == "subject" && length(parts) >= 4L) {
      c(sub_id = 2L, ses_id = 3L, stage = 4L, stream = 5L)
    } else if (parts[[1L]] == "project" && length(parts) >= 2L) {
      c(stage = 2L, stream = 3L)
    } else integer()
    for (field in names(mapping)) {
      position <- mapping[[field]]
      if (is.na(records[[field]][[index]]) && position <= length(parts) && nzchar(parts[[position]])) {
        records[[field]][[index]] <- parts[[position]]
      }
    }
  }
  missing <- is.na(records$stage)
  records$stage[missing] <- .job_stage(records$job_name[missing])
  missing <- is.na(records$sub_id)
  records$sub_id[missing] <- .extract_job_token(records$job_name[missing], "sub")
  missing <- is.na(records$ses_id)
  records$ses_id[missing] <- .extract_job_token(records$job_name[missing], "ses")
  missing <- is.na(records$stream)
  records$stream[missing] <- .job_stream(records$job_name[missing], records$stage[missing])
  records
}

#' Select pending records that can overlap the requested work
#' @param records Normalized pending metadata.
#' @param execution Resolved selection, with subjects and optional retry units.
#' @param current_run_id Owning controller run; its project dependencies are allowed.
#' @return Logical membership vector. Unknown scope is handled conservatively.
#' @noRd
submission_preflight_scope <- function(records, execution, current_run_id = NULL) {
  if (!nrow(records)) return(logical())
  stages <- execution$steps
  downstream <- setdiff(stages, "flywheel_sync")
  project_stages <- c(if ("flywheel_sync" %in% stages) "flywheel_sync",
    if ("fmriprep" %in% stages) "fsaverage_setup",
    if (any(stages %in% c("mriqc", "fmriprep", "aroma"))) "prefetch_templates",
    if (length(downstream)) "subject_submission")
  project <- records$stage %in% project_stages
  if (!is.null(current_run_id)) {
    project <- project & (is.na(records$sequence_id) | records$sequence_id != current_run_id)
  }
  # A pre-sync subject list is provisional, even when it is nonempty. Never
  # use it as proof that all future subject/session work has been checked.
  if (isTRUE(execution$scope_deferred)) {
    return(project | is.na(records$stage) | records$stage == "other")
  }
  subjects <- execution$subjects
  wanted <- records$stage %in% downstream
  streamed <- records$stage %in% c("postprocess", "extract_rois")
  wanted <- wanted & (!streamed | is.na(records$stream) |
    (records$stage == "postprocess" & records$stream %in% execution$postprocess_streams) |
    (records$stage == "extract_rois" & records$stream %in% execution$extract_streams))
  wanted <- wanted & (is.na(records$sub_id) | records$sub_id %in% subjects$sub_id)
  session_level <- records$stage %in% c("bids_conversion", "postprocess", "extract_rois")
  session_key <- paste(subjects$sub_id, ifelse(is.na(subjects$ses_id), "", subjects$ses_id), sep = "::")
  record_key <- paste(records$sub_id, ifelse(is.na(records$ses_id), "", records$ses_id), sep = "::")
  wanted <- wanted & (!session_level | is.na(records$sub_id) |
    is.na(records$ses_id) | record_key %in% session_key)
  if (!is.null(execution$work_units)) {
    units <- execution$work_units
    # Hash membership keeps exact retries linear in the pending/requested row
    # counts rather than comparing every pending record with every retry unit.
    requested_keys <- paste(units$stage, units$sub_id,
      ifelse(is.na(units$ses_id), "", units$ses_id),
      ifelse(is.na(units$stream), "", units$stream), sep = "::")
    record_keys <- paste(records$stage, records$sub_id,
      ifelse(session_level & !is.na(records$ses_id), records$ses_id, ""),
      ifelse(streamed & !is.na(records$stream), records$stream, ""), sep = "::")
    partial_keys <- paste(records$stage, records$sub_id,
      ifelse(session_level & !is.na(records$ses_id), records$ses_id, ""), sep = "::")
    requested_partial <- paste(units$stage, units$sub_id,
      ifelse(is.na(units$ses_id), "", units$ses_id), sep = "::")
    exact <- is.na(records$sub_id) | record_keys %in% requested_keys |
      (streamed & is.na(records$stream) & partial_keys %in% requested_partial)
    # Missing legacy session metadata is unknown scope, not proof that the
    # earlier allocation cannot overlap a specific-session retry.
    subject_stream_keys <- paste(records$stage, records$sub_id,
      ifelse(streamed & !is.na(records$stream), records$stream, ""), sep = "::")
    requested_subject_stream <- paste(units$stage, units$sub_id,
      ifelse(is.na(units$stream), "", units$stream), sep = "::")
    subject_keys <- paste(records$stage, records$sub_id, sep = "::")
    requested_subjects <- paste(units$stage, units$sub_id, sep = "::")
    exact <- exact | (session_level & is.na(records$ses_id) &
      (subject_stream_keys %in% requested_subject_stream |
        (streamed & is.na(records$stream) & subject_keys %in% requested_subjects)))
    wanted <- wanted & exact
  }
  project | wanted | is.na(records$stage) | records$stage == "other"
}

#' Assess submission readiness without changing tracking records or printing
#' @param scfg Project configuration.
#' @param execution Selected stages, streams, subjects and deferred-scope flag.
#' @param refresh Query scheduler accounting for relevant active jobs.
#' @param current_run_id Optional owning run for a deferred controller.
#' @param observations Optional prior accounting results to reuse without querying.
#' @return Read-only assessment with blockers, accounting observations and scope.
#' @noRd
assess_project_submission <- function(scfg, execution, refresh = FALSE,
                                      current_run_id = NULL, observations = NULL) {
  sqlite_db <- scfg$metadata$sqlite_db
  records <- read_submission_preflight_records(sqlite_db)
  jobs <- normalize_submission_preflight_records(records$jobs)
  jobs <- jobs[submission_preflight_scope(jobs, execution, current_run_id), , drop = FALSE]
  scheduler_checked <- refresh || !is.null(observations)
  if (refresh && nrow(jobs)) {
    observations <- reconcile_tracked_jobs(sqlite_db, dry_run = TRUE, jobs = jobs)
  }
  jobs <- normalize_submission_preflight_records(records$jobs)
  jobs <- jobs[submission_preflight_scope(jobs, execution, current_run_id), , drop = FALSE]
  uncertain <- normalize_submission_preflight_records(records$submissions)
  uncertain <- uncertain[submission_preflight_scope(uncertain, execution, current_run_id), , drop = FALSE]
  columns <- c("job_id", "contract_id", "stage", "sub_id", "ses_id", "stream", "status", "reason")
  blockers <- as.data.frame(setNames(rep(list(character()), length(columns)), columns))
  if (nrow(jobs)) {
    jobs$reason <- if (!scheduler_checked) "Scheduler confirmation required; dry run makes no scheduler queries." else {
      states <- observations$scheduler_status[match(jobs$job_id, observations$job_id)]
      ifelse(states %in% "COMPLETED", "Scheduler completed; verify outputs before resolving stale tracking.",
        ifelse(states %in% c("QUEUED", "RUNNING", "SUSPENDED"),
          "Earlier job is still active.", ifelse(states %in% c("FAILED", "CANCELLED"),
            "Confirmed scheduler failure/cancellation; tracking repair is available.",
            "Scheduler outcome is unconfirmed; absence is not rejection.")))
    }
    if (!is.null(observations) && "query_detail" %in% names(observations)) {
      details <- observations$query_detail[match(jobs$job_id, observations$job_id)]
      include <- !is.na(details) & nzchar(details)
      jobs$reason[include] <- paste(jobs$reason[include], details[include])
    }
    blockers <- rbind(blockers, jobs[, columns, drop = FALSE])
  }
  if (nrow(uncertain)) {
    uncertain$reason <- "Confirm submission outcome, then use resolve_project_submission()."
    blockers <- rbind(blockers, uncertain[, columns, drop = FALSE])
  }
  list(schema_version = "brain-gnomes-submission-preflight-v1",
    repairs = data.frame(), blockers = blockers, observations = observations,
    dry_run = !scheduler_checked,
    checked_at = job_contract_timestamp(),
    scope_deferred = isTRUE(execution$scope_deferred))
}

#' Prepare a submission using assessment, explicit repairs, reporting and enforcement
#' @param scfg Project configuration.
#' @param execution Selected stages, streams, subjects and deferred-scope flag.
#' @param dry_run Avoid scheduler queries, repairs and report-file writes.
#' @param current_run_id Optional owning run for a deferred controller.
#' @return The preflight report; live blockers raise bg_submission_preflight_error.
#' @noRd
prepare_project_submission <- function(scfg, execution, dry_run = FALSE, current_run_id = NULL) {
  cli::cli_alert_info("Checking submission records for this request (metadata only)...")
  report <- assess_project_submission(scfg, execution, refresh = !dry_run,
                                      current_run_id = current_run_id)
  if (!dry_run && !is.null(report$observations) && nrow(report$observations) > 0L) {
    applied <- apply_tracked_job_repairs(scfg$metadata$sqlite_db, report$observations)
    # Reassess scalar state after optimistic repairs to respect worker changes
    # and newly pending submissions without another accounting query.
    report <- assess_project_submission(scfg, execution, current_run_id = current_run_id,
                                        observations = applied)
    report$repairs <- applied[applied$applied, , drop = FALSE]
  }
  report$observations <- NULL
  report <- save_submission_preflight(scfg, current_run_id, report)
  print_submission_preflight(report)
  require_submission_ready(report)
  report
}

#' Detect a conversion lock that would contaminate or destabilize BIDS reads
#' @param scfg Project configuration.
#' @param steps Selected stages.
#' @return An actionable issue string, or NULL when no lock blocks this request.
#' @noRd
project_bids_lock_issue <- function(scfg, steps) {
  if (!length(intersect(steps, c("bids_conversion", "mriqc", "fmriprep"))) ||
      !checkmate::test_string(scfg$metadata$bids_directory)) return(NULL)
  lock <- file.path(scfg$metadata$bids_directory, "heudiconv.lock")
  if (!file.exists(lock)) return(NULL)
  paste0("BIDS conversion lock exists: ", lock,
         ". Finish any active conversion before submitting this run. If the lock is stale, ",
         "confirm no conversion is active before moving it out of the BIDS dataset; ",
         "then run run_bids_validation() and inspect its report.")
}
