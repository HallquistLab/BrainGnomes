#' Identify allocations whose tracking does not establish scheduler termination
#' @param jobs Tracking rows with status and optional scheduler terminal evidence.
#' @return Logical membership, including unknown states and unconfirmed blocks.
#' @noRd
pending_tracked_job_rows <- function(jobs) {
  status <- toupper(trimws(as.character(jobs$status)))
  terminal <- if ("scheduler_terminal_state" %in% names(jobs)) {
    toupper(trimws(as.character(jobs$scheduler_terminal_state)))
  } else rep(NA_character_, nrow(jobs))
  !status %in% c("COMPLETED", "FAILED", "CANCELLED", "FAILED_BY_EXT") |
    (status %in% "FAILED_BY_EXT" & !terminal %in% c("FAILED", "CANCELLED", "COMPLETED"))
}

#' Select tracking states that do not establish scheduler termination
#' @param fields Columns present in the tracking table.
#' @param table_alias Optional SQL alias for the tracking table.
#' @return A SQL predicate using only trusted internal column names.
#' @noRd
pending_tracked_job_clause <- function(fields, table_alias = NULL) {
  prefix <- if (is.null(table_alias)) "" else paste0(table_alias, ".")
  status <- paste0("COALESCE(UPPER(TRIM(", prefix, "status)), '')")
  unconfirmed <- if ("scheduler_terminal_state" %in% fields) {
    paste0(" AND COALESCE(UPPER(TRIM(", prefix, "scheduler_terminal_state)), '') NOT IN ('FAILED','CANCELLED','COMPLETED')")
  } else ""
  paste0("(", status, " NOT IN ('COMPLETED','FAILED','CANCELLED','FAILED_BY_EXT') OR (", status,
    " = 'FAILED_BY_EXT'", unconfirmed, "))")
}

#' Reconcile active tracking rows against confirmed scheduler failures
#' @param sqlite_db Existing tracking database path.
#' @param unit_key Optional logical work unit to restrict the query.
#' @param job_role Optional role to restrict the query.
#' @param dry_run Return proposed changes without writing them.
#' @param jobs Optional preselected active tracking metadata, avoiding a second scan.
#' @return Scheduler observations and whether each proposed change was applied.
#' @noRd
reconcile_tracked_jobs <- function(sqlite_db, unit_key = NULL, job_role = NULL,
                                      dry_run = TRUE, jobs = NULL) {
  empty <- data.frame(job_id = character(), contract_id = character(),
    unit_key = character(), job_role = character(), scheduler = character(),
    status = character(), scheduler_status = character(), query_detail = character(), action = character(),
    applied = logical())
  if (!checkmate::test_file_exists(sqlite_db)) return(empty)
  con <- DBI::dbConnect(RSQLite::SQLite(), sqlite_db, flags = RSQLite::SQLITE_RO,
                        synchronous = NULL)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  RSQLite::sqliteSetBusyHandler(con, 10000L)
  if (!DBI::dbExistsTable(con, "job_tracking")) return(empty)
  fields <- DBI::dbListFields(con, "job_tracking")
  columns <- c("job_id", "contract_id", "unit_key", "job_role", "scheduler", "status")
  selected <- ifelse(columns %in% fields, columns, paste("NULL AS", columns))
  query <- paste("SELECT", paste(selected, collapse = ","),
                 "FROM job_tracking WHERE", pending_tracked_job_clause(fields))
  params <- list()
  if (!is.null(unit_key)) {
    query <- paste(query, "AND unit_key = ? AND COALESCE(job_role, '') = ?")
    params <- list(unit_key, if (is.null(job_role) || is.na(job_role)) "" else job_role)
  }
  if (is.null(jobs)) {
    jobs <- if (length(params)) DBI::dbGetQuery(con, query, params = params) else {
      DBI::dbGetQuery(con, query)
    }
  }
  jobs$scheduler_status <- rep(NA_character_, nrow(jobs))
  jobs$query_detail <- rep(NA_character_, nrow(jobs))
  missing_scheduler <- is.na(jobs$scheduler) | !nzchar(trimws(jobs$scheduler))
  jobs$query_detail[missing_scheduler] <- "Tracking record has no scheduler identity; restore it from submission evidence before recovery."
  jobs$action <- rep("UNCHANGED", nrow(jobs))
  jobs$applied <- rep(FALSE, nrow(jobs))
  if (!nrow(jobs)) return(jobs)
  for (scheduler in unique(jobs$scheduler)) {
    index <- which(!is.na(jobs$scheduler) & jobs$scheduler == scheduler)
    if (!length(index)) next
    # Bound command lengths and batch accounting requests rather than querying
    # each participant separately. No output files are examined here.
    chunks <- split(index, ceiling(seq_along(index) / 200L))
    for (chunk in chunks) {
      observed <- scheduler_job_status(unique(jobs$job_id[chunk]), scheduler)
      jobs$scheduler_status[chunk] <- observed$scheduler_status[
        match(jobs$job_id[chunk], observed$job_id)
      ]
      if ("query_detail" %in% names(observed)) {
        jobs$query_detail[chunk] <- observed$query_detail[match(jobs$job_id[chunk], observed$job_id)]
      }
      if (all(observed$scheduler_status %in% "UNAVAILABLE")) {
        remaining <- index[index > max(chunk)]
        jobs$scheduler_status[remaining] <- "UNAVAILABLE"
        jobs$query_detail[remaining] <- jobs$query_detail[[chunk[[1L]]]]
        break
      }
    }
  }
  failed <- jobs$scheduler_status %in% c("FAILED", "CANCELLED")
  jobs$action[failed] <- "UPDATE_TERMINAL_FAILURE"
  jobs$action[jobs$scheduler_status %in% "COMPLETED"] <- "VERIFY_OUTPUTS"
  jobs$action[is.na(jobs$scheduler_status) | jobs$scheduler_status %in%
                c("MISSING", "UNKNOWN", "UNAVAILABLE")] <- "CONFIRM_OUTCOME"
  if (!dry_run && any(failed)) jobs <- apply_tracked_job_repairs(sqlite_db, jobs)
  jobs
}

#' Apply previously observed terminal failures without another scheduler query
#' @param sqlite_db Existing tracking database.
#' @param jobs Scheduler observations from reconcile_tracked_jobs().
#' @return Observations with applied flags, preserving concurrent worker changes.
#' @noRd
apply_tracked_job_repairs <- function(sqlite_db, jobs) {
  jobs$applied <- rep(FALSE, nrow(jobs))
  failed <- jobs$scheduler_status %in% c("FAILED", "CANCELLED")
  if (!any(failed)) return(jobs)
  con <- job_submission_connect(sqlite_db)
  fields <- tryCatch(DBI::dbListFields(con, "job_tracking"),
                     finally = DBI::dbDisconnect(con))
  if (!all(c("contract_id", "scheduler_terminal_state", "failure_category") %in% fields)) {
    ensure_tracking_db_schema(sqlite_db)
  }
  job_submission_transaction(sqlite_db, function(connection) {
    for (index in which(failed)) {
      # Compare identity and observed state before changing any tracking row.
      jobs$applied[[index]] <<- DBI::dbExecute(connection, paste(
        "UPDATE job_tracking SET status = ?, scheduler_terminal_state = ?,",
        "time_ended = COALESCE(time_ended, ?), failure_category = COALESCE(failure_category, 'SCHEDULER')",
        "WHERE job_id = ? AND contract_id IS ? AND status IS ?"
      ), params = list(if (identical(toupper(trimws(jobs$status[[index]])), "FAILED_BY_EXT")) "FAILED_BY_EXT" else {
                         jobs$scheduler_status[[index]]
                       }, jobs$scheduler_status[[index]],
                       job_contract_timestamp(), jobs$job_id[[index]],
                       jobs$contract_id[[index]], jobs$status[[index]])) == 1L
    }
  })
  jobs
}
