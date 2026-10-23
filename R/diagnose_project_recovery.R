#' Identify unresolved submission outcomes in an existing inspection snapshot
#' @param inspection Project inspection, already scoped by run and subject.
#' @param job_id Optional exact scheduler allocation focus.
#' @param subject_id Optional normalized subject focus.
#' @return Submission issue rows with explicit recovery guidance.
#' @noRd
diagnosis_submission_issues <- function(inspection, job_id = NULL, subject_id = NULL) {
  fields <- c("contract_id", "scheduler_job_id", "run_id", "unit_key", "job_role",
              "sub_id", "ses_id", "stage", "stream", "submission_state", "submission_detail", "action")
  empty <- as.data.frame(setNames(rep(list(character()), length(fields)), fields))
  records <- inspection$submissions
  if (!is.data.frame(records) || !nrow(records)) return(empty)
  if (!is.null(job_id)) {
    records <- records[!is.na(records$scheduler_job_id) & records$scheduler_job_id == job_id, , drop = FALSE]
  }
  if (!nrow(records)) return(empty)
  if (!is.null(subject_id)) {
    records <- records[!is.na(records$sub_id) & records$sub_id == subject_id, , drop = FALSE]
  }
  if (!nrow(records)) return(empty)
  identities <- paste(records$unit_key, records$job_role, sep = "::")
  latest <- !duplicated(identities, fromLast = TRUE)
  unconfirmed <- records$submission_state %in% c("UNKNOWN", "SUBMITTING")
  rejected <- latest & records$submission_state == "REJECTED"
  missing_binding <- records$submission_state == "ACCEPTED" &
    !submission_bindings_present(records, inspection$jobs)
  records$action <- ifelse(unconfirmed,
    "Confirm scheduler acceptance, then use resolve_project_submission().",
    ifelse(missing_binding,
      "Restore the confirmed allocation binding with resolve_project_submission().",
      "Correct the scheduler rejection, then retry run_project()."))
  records <- records[unconfirmed | rejected | missing_binding, , drop = FALSE]
  for (field in setdiff(fields, names(records))) records[[field]] <- NA_character_
  records[, fields, drop = FALSE]
}

#' Select timing and accounting problems from an existing inspection snapshot
#' @param inspection Scoped project inspection.
#' @param job_id Optional exact allocation focus.
#' @param subject_id Optional normalized subject focus.
#' @return Active rows needing confirmation, reconciliation or timing review.
#' @noRd
diagnosis_active_issues <- function(inspection, job_id = NULL, subject_id = NULL) {
  active <- inspection$active
  if (!is.data.frame(active)) return(data.frame())
  wanted <- active$health %in% c("STALE_DATABASE", "DATABASE_LAG", "UNCONFIRMED", "OVERDUE", "SUSPENDED")
  if (!is.null(job_id)) wanted <- wanted & active$job_id == job_id
  if (!is.null(subject_id)) wanted <- wanted & !is.na(active$sub_id) & active$sub_id == subject_id
  active[wanted, , drop = FALSE]
}

#' Show current recovery issues without changing scheduler or database state
#' @param diagnosis Structured diagnosis containing submission and active issues.
#' @return Diagnosis invisibly, with bounded issue tables in the console.
#' @noRd
print_diagnosis_recovery <- function(diagnosis) {
  if (nrow(diagnosis$submission_issues)) {
    cli::cli_alert_warning("{nrow(diagnosis$submission_issues)} submission outcome(s) need attention.")
    columns <- c("contract_id", "scheduler_job_id", "sub_id", "stage", "submission_state", "action")
    print(utils::head(diagnosis$submission_issues[, columns, drop = FALSE], 20L), row.names = FALSE)
    cli::cli_text("Use diagnosis$submission_issues for full details. Queue absence alone does not establish rejection.")
    cli::cli_text("Confirm acceptance before using resolve_project_submission(); correct confirmed rejections before retrying run_project().")
  }
  if (nrow(diagnosis$active_issues)) {
    cli::cli_alert_warning("{nrow(diagnosis$active_issues)} active tracking record(s) need scheduler or timing review.")
    columns <- intersect(c("job_id", "sub_id", "stage", "database_status", "scheduler_status", "health"),
                         names(diagnosis$active_issues))
    print(utils::head(diagnosis$active_issues[, columns, drop = FALSE], 20L), row.names = FALSE)
    cli::cli_text("Inspect with refresh = TRUE; preview confirmed failure/cancellation recovery with reconcile_project_jobs().")
  }
  invisible(diagnosis)
}

#' Review recovery evidence before opening the tracked-job browser
#' @param diagnosis Structured project diagnosis.
#' @return TRUE to browse jobs, FALSE to return the diagnosis without mutations.
#' @noRd
diagnosis_review_recovery <- function(diagnosis) {
  has_issues <- nrow(diagnosis$submission_issues) > 0L || nrow(diagnosis$active_issues) > 0L
  has_report <- !is.null(diagnosis$submission_preflight)
  if (!has_issues && !has_report) return(TRUE)
  repeat {
    labels <- c(if (has_issues) "View submission issues and recovery guidance",
      if (has_report) "View recorded submission preflight", "Continue to tracked jobs", "Return diagnosis")
    actions <- c(if (has_issues) "issues", if (has_report) "report", "jobs", "return")
    choice <- diagnosis_prompt_menu(labels, "Review submission readiness and recovery")
    action <- actions[[choice]]
    if (action == "jobs") return(TRUE)
    if (action == "return") return(FALSE)
    if (action == "issues") print_diagnosis_recovery(diagnosis)
    if (action == "report") {
      report <- diagnosis$submission_preflight
      print_saved_submission_preflight(report)
      if (nrow(report$blockers)) print(utils::head(report$blockers, 20L), row.names = FALSE)
    }
  }
}
