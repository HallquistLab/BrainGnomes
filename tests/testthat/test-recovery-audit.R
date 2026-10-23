#' Build independent tracking and configuration for recovery race regressions
#' @param stage Processing stage for the selected work unit.
#' @return An isolated project and one registered submission attempt.
make_recovery_audit_fixture <- function(stage = "postprocess") {
  root <- tempfile("recovery-audit-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE), envir = parent.frame())
  db <- file.path(root, "tracking.sqlite")
  create_tracking_db(db)
  cfg <- structure(list(metadata = list(project_name = "audit", project_directory = root,
    sqlite_db = db, log_directory = file.path(root, "logs")),
    compute_environment = list(scheduler = "slurm")), class = "bg_project_cfg")
  tracking <- list(job_name = paste0(stage, "_clean_sub-01"), sequence_id = "old-run",
    scheduler = "slurm", stage = stage, stream = if (stage == "postprocess") "clean" else NULL,
    sub_id = "01", job_role = "subject", attempt = 1L)
  tracking$unit_key <- tracking_unit_key(stage, tracking$stream, "01")
  contract <- allocate_job_contract(db, tracking)
  manifest <- write_job_manifest(contract, "true", "sbatch", "/fake/sbatch", "",
    character(), FALSE, NULL, "afterok", "sbatch", tracking)
  tracking$job_manifest_path <- manifest$path
  tracking$job_manifest_checksum <- manifest$checksum
  register_job_submission(db, contract, tracking)
  execution <- list(steps = stage, subjects = data.frame(sub_id = "01", ses_id = "A"),
    postprocess_streams = "clean", extract_streams = character(), scope_deferred = FALSE)
  list(root = root, db = db, cfg = cfg, tracking = tracking,
    contract_id = contract$contract_id, execution = execution)
}

test_that("unknown legacy sessions overlap explicit sessions including exact retries", {
  fixture <- make_recovery_audit_fixture()
  set_job_submission_state(fixture$db, fixture$contract_id, "UNKNOWN")
  execution <- fixture$execution
  for (retry in c(FALSE, TRUE)) {
    if (retry) execution$work_units <- data.frame(stage = "postprocess", stream = "clean",
      sub_id = "01", ses_id = "A")
    report <- assess_project_submission(fixture$cfg, execution)
    expect_identical(report$blockers$contract_id, fixture$contract_id)
  }
})

test_that("accepted orphan bindings block inspection and the final launch guard", {
  fixture <- make_recovery_audit_fixture("fmriprep")
  bind_job_submission(fixture$db, fixture$contract_id, "910")
  con <- job_submission_connect(fixture$db)
  DBI::dbExecute(con, "DELETE FROM job_tracking WHERE job_id = '910'")
  DBI::dbDisconnect(con)
  status <- inspect_project(fixture$cfg)
  expect_identical(status$overview$overall_status, "UNCONFIRMED_SUBMISSIONS")
  expect_identical(status$runs$status, "UNCONFIRMED")
  later <- fixture$tracking
  later$sequence_id <- "new-run"
  contract <- allocate_job_contract(fixture$db, later)
  later$job_manifest_path <- file.path(contract$directory, "manifest.json")
  later$job_manifest_checksum <- "audit"
  register_job_submission(fixture$db, contract, later)
  expect_error(set_job_submission_state(fixture$db, contract$contract_id, "SUBMITTING"),
    "duplicate|unconfirmed")
})

test_that("cancellation acknowledgement is not proof that allocation has stopped", {
  fixture <- make_recovery_audit_fixture("fmriprep")
  bind_job_submission(fixture$db, fixture$contract_id, "911")
  local_mocked_bindings(system2 = function(command, ...) {
    expect_identical(command, "scancel")
    character()
  }, .package = "base")
  local_mocked_bindings(scheduler_job_status = function(job_ids, ...) {
    data.frame(job_id = job_ids, scheduler_status = "RUNNING")
  })
  result <- cancel_project_run(fixture$cfg, "old-run", dry_run = FALSE)
  expect_identical(result$status, "cancellation_requested")
  expect_identical(get_tracked_job_status("911", sqlite_db = fixture$db)$status, "QUEUED")
  expect_error(prepare_project_submission(fixture$cfg, fixture$execution),
    class = "bg_submission_preflight_error")
})

test_that("failure cascades preserve running and terminal descendants", {
  fixture <- make_recovery_audit_fixture("fmriprep")
  states <- c("QUEUED", "STARTED", "COMPLETED", "CANCELLED", "FAILED", " queued ", "unknown", " completed ")
  bind_job_submission(fixture$db, fixture$contract_id, "parent")
  for (index in seq_along(states)) {
    job_id <- paste0("child-", index)
    insert_tracked_job(fixture$db, job_id, list(sequence_id = "old-run", status = states[[index]]))
    add_tracked_job_parent(fixture$db, job_id, "parent")
  }
  update_tracked_job_status(fixture$db, "parent", "FAILED", cascade = TRUE, strict = TRUE)
  jobs <- get_tracked_job_status(sequence_id = "old-run", sqlite_db = fixture$db)
  expect_identical(jobs$status[match(paste0("child-", seq_along(states)), jobs$job_id)],
    ifelse(toupper(trimws(states)) %in% c("QUEUED", "UNKNOWN"), "FAILED_BY_EXT", states))
})

test_that("externally blocked allocations remain launch blockers until scheduler termination", {
  fixture <- make_recovery_audit_fixture("fmriprep")
  bind_job_submission(fixture$db, fixture$contract_id, "912")
  update_tracked_job_status(fixture$db, "912", "FAILED_BY_EXT", strict = TRUE)
  observed <- "QUEUED"
  local_mocked_bindings(scheduler_job_status = function(job_ids, ...) {
    data.frame(job_id = job_ids, scheduler_status = observed)
  })
  expect_error(prepare_project_submission(fixture$cfg, fixture$execution),
    class = "bg_submission_preflight_error")
  observed <- "CANCELLED"
  expect_no_error(prepare_project_submission(fixture$cfg, fixture$execution))
  row <- get_tracked_job_status("912", sqlite_db = fixture$db)
  expect_identical(row$status, "FAILED_BY_EXT")
  expect_identical(row$scheduler_terminal_state, "CANCELLED")
})

test_that("late progress messages cannot resurrect terminal tracking rows", {
  fixture <- make_recovery_audit_fixture("fmriprep")
  for (status in c("COMPLETED", "FAILED", "CANCELLED", "completed", " failed ", "cancelled")) {
    insert_tracked_job(fixture$db, status, list(status = status))
    for (late in c("QUEUED", "STARTED")) {
      expect_error(update_tracked_job_status(fixture$db, status, late, strict = TRUE),
        "transition|terminal")
      expect_identical(get_tracked_job_status(status, sqlite_db = fixture$db)$status, status)
    }
  }
})

test_that("newer success cannot hide an older allocation that is still pending", {
  fixture <- make_recovery_audit_fixture("fmriprep")
  bind_job_submission(fixture$db, fixture$contract_id, "913")
  later <- fixture$tracking
  later[c("job_manifest_path", "job_manifest_checksum")] <- NULL
  insert_tracked_job(fixture$db, "914", modifyList(later,
    list(sequence_id = "new-run", status = "COMPLETED", attempt = 2L)))
  con <- job_submission_connect(fixture$db)
  DBI::dbExecute(con, "UPDATE job_tracking SET time_submitted = '2026-10-20T12:00:00Z' WHERE job_id = '913'")
  DBI::dbExecute(con, "UPDATE job_tracking SET time_submitted = '2026-10-21T12:00:00Z' WHERE job_id = '914'")
  DBI::dbDisconnect(con)
  inspection <- inspect_project(fixture$cfg)
  expect_identical(inspection$subjects$status, "COMPLETED")
  expect_identical(inspection$overview$overall_status, "ACTIVE_PRIOR_ATTEMPTS")
  expect_identical(inspection$active$job_id, "913")
  expect_false(inspection$active$is_current_attempt)
  expect_identical(diagnose_project(inspection, interactive = FALSE)$active_issues$job_id, "913")
})

test_that("failure traversal handles cycles and preserves excluded branches", {
  fixture <- make_recovery_audit_fixture("fmriprep")
  for (job_id in c("root", "child", "grandchild", "excluded", "protected")) {
    insert_tracked_job(fixture$db, job_id)
  }
  add_tracked_job_parent(fixture$db, "child", "root")
  add_tracked_job_parent(fixture$db, "grandchild", "child")
  add_tracked_job_parent(fixture$db, "root", "grandchild")
  add_tracked_job_parent(fixture$db, "excluded", "root")
  add_tracked_job_parent(fixture$db, "protected", "excluded")
  expect_no_error(update_tracked_job_status(fixture$db, "root", "FAILED",
    cascade = TRUE, exclude = "excluded", strict = TRUE))
  expect_identical(get_tracked_job_status("grandchild", sqlite_db = fixture$db)$status, "FAILED_BY_EXT")
  expect_identical(get_tracked_job_status("root", sqlite_db = fixture$db)$status, "FAILED")
  expect_identical(get_tracked_job_status("excluded", sqlite_db = fixture$db)$status, "QUEUED")
  expect_identical(get_tracked_job_status("protected", sqlite_db = fixture$db)$status, "QUEUED")
})

test_that("duplicate array progress retains the original start time", {
  fixture <- make_recovery_audit_fixture()
  bind_job_submission(fixture$db, fixture$contract_id, "915")
  update_tracked_job_status(fixture$db, "915", "STARTED", strict = TRUE)
  con <- job_submission_connect(fixture$db)
  DBI::dbExecute(con, "UPDATE job_tracking SET time_started = '2026-10-19T12:00:00Z' WHERE job_id = '915'")
  DBI::dbDisconnect(con)
  update_tracked_job_status(fixture$db, "915", "STARTED", strict = TRUE)
  expect_identical(get_tracked_job_status("915", sqlite_db = fixture$db)$time_started,
    "2026-10-19T12:00:00Z")
})

test_that("status reads do not create missing databases and tolerate pre-blob schemas", {
  fixture <- make_recovery_audit_fixture()
  missing <- file.path(fixture$root, "missing.sqlite")
  expect_warning(expect_null(get_tracked_job_status("916", sqlite_db = missing)), "Cannot find")
  expect_false(file.exists(missing))
  legacy <- file.path(fixture$root, "legacy.sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), legacy)
  DBI::dbExecute(con, "CREATE TABLE job_tracking (job_id TEXT, status TEXT)")
  DBI::dbExecute(con, "INSERT INTO job_tracking VALUES ('916', 'QUEUED')")
  DBI::dbDisconnect(con)
  expect_identical(get_tracked_job_status("916", sqlite_db = legacy)$status, "QUEUED")
})

test_that("independent submitters cannot both pass the durable final guard", {
  skip_on_os("windows")
  fixture <- make_recovery_audit_fixture("fmriprep")
  later <- fixture$tracking
  later$sequence_id <- "competing-run"
  contract <- allocate_job_contract(fixture$db, later)
  later$job_manifest_path <- file.path(contract$directory, "manifest.json")
  later$job_manifest_checksum <- "audit"
  register_job_submission(fixture$db, contract, later)
  scheduler <- file.path(fixture$root, "fake-sbatch")
  invocations <- file.path(fixture$root, "invocations")
  writeLines(c("#!/bin/sh", paste("echo accepted >>", shQuote(invocations)),
    "echo 'Submitted batch job 920'"), scheduler)
  Sys.chmod(scheduler, "0755")
  barrier <- file.path(fixture$root, "start")
  # Wait for the barrier, claim this registered UUID, and invoke the scheduler.
  # Return either the acknowledgement or the final guard's error message.
  submit <- function(contract_id) {
    deadline <- Sys.time() + 10
    while (!file.exists(barrier)) {
      if (Sys.time() > deadline) stop("Submission test barrier timed out")
      Sys.sleep(0.01)
    }
    tryCatch({
      set_job_submission_state(fixture$db, contract_id, "SUBMITTING")
      system2(scheduler, stdout = TRUE, stderr = TRUE)
    }, error = conditionMessage)
  }
  processes <- lapply(c(fixture$contract_id, contract$contract_id), function(contract_id) {
    parallel::mcparallel(submit(contract_id), silent = TRUE)
  })
  file.create(barrier)
  results <- unlist(parallel::mccollect(processes), use.names = FALSE)
  expect_length(readLines(invocations), 1L)
  expect_equal(sum(results == "Submitted batch job 920"), 1L)
  expect_true(any(grepl("duplicate launch", results)))
  attempts <- read_job_submission_attempts(fixture$db)
  expect_equal(sum(attempts$submission_state == "SUBMITTING"), 1L)
  expect_equal(sum(attempts$submission_state == "PREPARED"), 1L)
})

test_that("a real accounting client cannot overwrite concurrent worker completion", {
  skip_on_os("windows")
  fixture <- make_recovery_audit_fixture("fmriprep")
  bind_job_submission(fixture$db, fixture$contract_id, "921")
  scheduler <- file.path(fixture$root, "sacct")
  code <- paste0("con <- DBI::dbConnect(RSQLite::SQLite(), ",
    encodeString(fixture$db, quote = '"'), "); ",
    "DBI::dbExecute(con, \"UPDATE job_tracking SET status = 'COMPLETED' WHERE job_id = '921'\"); ",
    "DBI::dbDisconnect(con)")
  writeLines(c("#!/bin/sh",
    paste(shQuote(file.path(R.home("bin"), "Rscript")), "--vanilla -e", shQuote(code), ">/dev/null || exit $?"),
    "printf 'JobID|State\\n921|CANCELLED\\n'"), scheduler)
  Sys.chmod(scheduler, "0755")
  withr::local_envvar(PATH = paste(fixture$root, Sys.getenv("PATH"), sep = .Platform$path.sep))
  result <- reconcile_project_jobs(fixture$cfg, dry_run = FALSE)
  expect_identical(result$scheduler_status, "CANCELLED")
  expect_false(result$applied)
  expect_identical(get_tracked_job_status("921", sqlite_db = fixture$db)$status, "COMPLETED")
})

test_that("manual resolution rejects malformed IDs and blank rejection evidence", {
  fixture <- make_recovery_audit_fixture("fmriprep")
  set_job_submission_state(fixture$db, fixture$contract_id, "UNKNOWN")
  for (job_id in c("job 123", "123_2", " 123", "123;othercluster", "123\n456")) {
    expect_error(resolve_project_submission(fixture$cfg, fixture$contract_id,
      scheduler_job_id = job_id), "Invalid scheduler allocation")
  }
  expect_error(resolve_project_submission(fixture$cfg, fixture$contract_id,
    rejection_reason = " \n "), "authoritative evidence")
  expect_identical(read_job_submission_attempts(fixture$db)$submission_state, "UNKNOWN")
})

test_that("unknown legacy states cannot bypass preflight and can be safely reconciled", {
  fixture <- make_recovery_audit_fixture("fmriprep")
  states <- c("UNKNOWN", NA_character_, "queued", "unrecognized-state")
  for (index in seq_along(states)) insert_tracked_job(fixture$db, as.character(930 + index),
    list(stage = "fmriprep", sub_id = "01", scheduler = "slurm", status = states[[index]]))
  observed <- "MISSING"
  local_mocked_bindings(scheduler_job_status = function(job_ids, ...) {
    data.frame(job_id = job_ids, scheduler_status = observed)
  })
  report <- assess_project_submission(fixture$cfg, fixture$execution)
  expect_equal(nrow(report$blockers), length(states))
  expect_error(prepare_project_submission(fixture$cfg, fixture$execution),
    class = "bg_submission_preflight_error")
  observed <- "CANCELLED"
  report <- prepare_project_submission(fixture$cfg, fixture$execution)
  expect_equal(nrow(report$repairs), length(states))
  expect_true(all(report$repairs$applied))
})

test_that("damaged submission metadata stays visible and blocks conservatively", {
  fixture <- make_recovery_audit_fixture("fmriprep")
  set_job_submission_state(fixture$db, fixture$contract_id, "UNKNOWN")
  con <- job_submission_connect(fixture$db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  for (encoded in c("{ broken", '{"stage":"fmriprep","sub_id":["01","02"]}')) {
    DBI::dbExecute(con, "UPDATE job_submission_attempts SET tracking_metadata = ?",
      params = list(encoded))
    records <- inspect_job_submissions(fixture$db)
    expect_false(is.na(records$metadata_error))
    expect_identical(inspect_project(fixture$cfg)$overview$overall_status, "UNCONFIRMED_SUBMISSIONS")
    expect_identical(assess_project_submission(fixture$cfg, fixture$execution)$blockers$contract_id,
      fixture$contract_id)
  }
})

test_that("failed binding transactions leave no partial acceptance or tracking row", {
  fixture <- make_recovery_audit_fixture("fmriprep")
  set_job_submission_state(fixture$db, fixture$contract_id, "UNKNOWN")
  con <- job_submission_connect(fixture$db)
  metadata <- jsonlite::fromJSON(DBI::dbGetQuery(con,
    "SELECT tracking_metadata FROM job_submission_attempts")$tracking_metadata)
  metadata$n_nodes <- 0L
  DBI::dbExecute(con, "UPDATE job_submission_attempts SET tracking_metadata = ?",
    params = list(as.character(jsonlite::toJSON(metadata, auto_unbox = TRUE, null = "null"))))
  DBI::dbDisconnect(con)
  expect_error(bind_job_submission(fixture$db, fixture$contract_id, "940"), "CHECK constraint")
  expect_identical(read_job_submission_attempts(fixture$db)$submission_state, "UNKNOWN")
  expect_true(is.na(read_job_submission_attempts(fixture$db)$scheduler_job_id))
  expect_equal(nrow(get_tracked_job_status("940", sqlite_db = fixture$db)), 0L)
})

test_that("SQL and in-memory pending predicates agree across legacy and blocked states", {
  fixture <- make_recovery_audit_fixture()
  rows <- expand.grid(status = c(NA_character_, "", "queued", "STARTED", "FAILED_BY_EXT",
    "FAILED", "COMPLETED", "CANCELLED", "future-state"),
    scheduler_terminal_state = c(NA_character_, "", "FAILED", "cancelled", "COMPLETED"),
    stringsAsFactors = FALSE)
  rows$job_id <- as.character(seq_len(nrow(rows)))
  con <- job_submission_connect(fixture$db)
  DBI::dbAppendTable(con, "job_tracking", rows)
  selected <- DBI::dbGetQuery(con, paste("SELECT job_id FROM job_tracking WHERE",
    pending_tracked_job_clause(DBI::dbListFields(con, "job_tracking"))))
  DBI::dbDisconnect(con)
  expect_setequal(selected$job_id, rows$job_id[pending_tracked_job_rows(rows)])
})

test_that("dependency-blocked allocations can still be explicitly cancelled", {
  fixture <- make_recovery_audit_fixture("fmriprep")
  bind_job_submission(fixture$db, fixture$contract_id, "942")
  update_tracked_job_status(fixture$db, "942", "FAILED_BY_EXT", strict = TRUE)
  preview <- cancel_project_run(fixture$cfg, "old-run")
  expect_identical(preview$job_id, "942")
  expect_identical(preview$status, "would_cancel")
  observed <- "CANCELLED"
  local_mocked_bindings(scheduler_job_status = function(job_ids, ...) {
    data.frame(job_id = job_ids, scheduler_status = observed)
  })
  reconcile_project_jobs(fixture$cfg, dry_run = FALSE)
  expect_equal(nrow(cancel_project_run(fixture$cfg, "old-run")), 0L)
  expect_error(update_tracked_job_status(fixture$db, "942", "STARTED", strict = TRUE), "transition")
})

test_that("mismatched accepted allocation identities cannot masquerade as a binding", {
  fixture <- make_recovery_audit_fixture("fmriprep")
  bind_job_submission(fixture$db, fixture$contract_id, "943")
  update_tracked_job_status(fixture$db, "943", "COMPLETED", strict = TRUE)
  con <- job_submission_connect(fixture$db)
  DBI::dbExecute(con, "UPDATE job_submission_attempts SET scheduler_job_id = '944'")
  DBI::dbDisconnect(con)
  expect_identical(inspect_project(fixture$cfg)$overview$overall_status, "UNCONFIRMED_SUBMISSIONS")
  diagnosis <- diagnose_project(fixture$cfg, interactive = FALSE)
  expect_identical(diagnosis$submission_issues$contract_id, fixture$contract_id)
  expect_identical(assess_project_submission(fixture$cfg, fixture$execution)$blockers$contract_id,
    fixture$contract_id)
})

test_that("the final guard also protects allocations without a submission registry row", {
  for (status in c("QUEUED", "STARTED", "FAILED_BY_EXT", "UNKNOWN", NA_character_)) {
    fixture <- make_recovery_audit_fixture("fmriprep")
    legacy <- fixture$tracking
    legacy[c("job_manifest_path", "job_manifest_checksum")] <- NULL
    legacy$sequence_id <- "legacy-run"
    legacy$status <- status
    insert_tracked_job(fixture$db, "945", legacy)
    local_mocked_bindings(scheduler_job_status = function(job_ids, ...) {
      data.frame(job_id = job_ids, scheduler_status = "RUNNING")
    })
    expect_error(set_job_submission_state(fixture$db, fixture$contract_id, "SUBMITTING"),
      "duplicate launch")
    expect_identical(read_job_submission_attempts(fixture$db)$submission_state, "PREPARED")
  }
})

test_that("cancellation cannot overwrite completion reported by an independent worker", {
  skip_on_os("windows")
  fixture <- make_recovery_audit_fixture("fmriprep")
  bind_job_submission(fixture$db, fixture$contract_id, "946")
  scheduler <- file.path(fixture$root, "scancel")
  code <- paste0("con <- DBI::dbConnect(RSQLite::SQLite(), ",
    encodeString(fixture$db, quote = '"'), "); ",
    "DBI::dbExecute(con, \"UPDATE job_tracking SET status = 'COMPLETED' WHERE job_id = '946'\"); ",
    "DBI::dbDisconnect(con)")
  writeLines(c("#!/bin/sh", paste(shQuote(file.path(R.home("bin"), "Rscript")),
    "--vanilla -e", shQuote(code), ">/dev/null")), scheduler)
  Sys.chmod(scheduler, "0755")
  withr::local_envvar(PATH = paste(fixture$root, Sys.getenv("PATH"), sep = .Platform$path.sep))
  result <- cancel_project_run(fixture$cfg, "old-run", dry_run = FALSE)
  expect_identical(result$status, "cancellation_requested")
  expect_identical(get_tracked_job_status("946", sqlite_db = fixture$db)$status, "COMPLETED")
})

test_that("cancellation process-launch errors do not claim terminal tracking", {
  fixture <- make_recovery_audit_fixture("fmriprep")
  bind_job_submission(fixture$db, fixture$contract_id, "947")
  local_mocked_bindings(system2 = function(...) stop("Cancellation client unavailable"),
    .package = "base")
  result <- cancel_project_run(fixture$cfg, "old-run", dry_run = FALSE)
  expect_identical(result$status, "failed")
  expect_identical(get_tracked_job_status("947", sqlite_db = fixture$db)$status, "QUEUED")
})

test_that("accounting blockers retain the diagnostic needed to resolve an outage", {
  fixture <- make_recovery_audit_fixture("fmriprep")
  bind_job_submission(fixture$db, fixture$contract_id, "948")
  local_mocked_bindings(scheduler_job_status = function(job_ids, ...) {
    data.frame(job_id = job_ids, scheduler_status = "UNAVAILABLE",
      query_detail = "sacct query failed: accounting connection refused")
  })
  report <- assess_project_submission(fixture$cfg, fixture$execution, refresh = TRUE)
  expect_match(report$blockers$reason, "accounting connection refused")
  expect_identical(get_tracked_job_status("948", sqlite_db = fixture$db)$status, "QUEUED")
})

test_that("pending scans use a covering index rather than completed job object history", {
  fixture <- make_recovery_audit_fixture("fmriprep")
  con <- job_submission_connect(fixture$db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  rows <- data.frame(job_id = as.character(seq_len(250)), stage = "fmriprep", sub_id = "01",
    status = "COMPLETED", scheduler = "slurm")
  DBI::dbAppendTable(con, "job_tracking", rows)
  DBI::dbExecute(con, "UPDATE job_tracking SET job_obj = zeroblob(32768)")
  DBI::dbExecute(con, "UPDATE job_tracking SET status = 'QUEUED' WHERE job_id = '1'")
  fields <- DBI::dbListFields(con, "job_tracking")
  columns <- c("job_id", "job_name", "contract_id", "unit_key", "job_role", "scheduler",
    "status", "stage", "stream", "sub_id", "ses_id", "sequence_id")
  plan <- DBI::dbGetQuery(con, paste("EXPLAIN QUERY PLAN SELECT", paste(columns, collapse = ","),
    "FROM job_tracking WHERE", pending_tracked_job_clause(fields)))
  expect_true(any(grepl("COVERING INDEX job_tracking_pending_v1", plan$detail, fixed = TRUE)))
  expect_identical(read_submission_preflight_records(fixture$db)$jobs$job_id, "1")
})

test_that("diagnosis log discovery cannot confuse subject numbers and job ID prefixes", {
  fixture <- make_recovery_audit_fixture("fmriprep")
  insert_tracked_job(fixture$db, "123", list(sequence_id = "logs", status = "FAILED"))
  dir.create(fixture$cfg$metadata$log_directory)
  expected <- file.path(fixture$cfg$metadata$log_directory, "fmriprep_jobid-123.err")
  file.create(expected,
    file.path(fixture$cfg$metadata$log_directory, "fmriprep_jobid-1234.err"),
    file.path(fixture$cfg$metadata$log_directory, "fmriprep_sub-123_jobid-456.err"))
  expect_identical(find_run_logs(fixture$cfg, "logs")$path, expected)
  fixture$cfg$metadata$log_directory <- NULL
  expect_equal(nrow(find_run_logs(fixture$cfg, "logs")), 0L)
})

test_that("mixed UTC and local chronology cannot hide a newer failed attempt", {
  withr::local_envvar(TZ = "America/New_York")
  fixture <- make_recovery_audit_fixture("fmriprep")
  insert_tracked_job(fixture$db, "951", list(sequence_id = "utc-success",
    stage = "fmriprep", sub_id = "01", status = "COMPLETED", scheduler = "slurm"))
  insert_tracked_job(fixture$db, "952", list(sequence_id = "local-failure",
    stage = "fmriprep", sub_id = "01", status = "FAILED", scheduler = "slurm"))
  times <- c("2026-10-23T14:30:00Z", "2026-10-23 11:00:00")
  con <- job_submission_connect(fixture$db)
  DBI::dbExecute(con, "UPDATE job_tracking SET time_submitted = ? WHERE job_id = '951'",
    params = list(times[[1L]]))
  DBI::dbExecute(con, "UPDATE job_tracking SET time_submitted = ? WHERE job_id = '952'",
    params = list(times[[2L]]))
  DBI::dbExecute(con, "UPDATE job_submission_attempts SET prepared_at = '2026-10-01T12:00:00Z'")
  expect_identical(.time_extreme(times, "max"), times[[2L]])
  expect_identical(.time_extreme(times, "min"), times[[1L]])
  expect_identical(inspect_project(fixture$cfg, run_id = "latest")$run_id, "local-failure")
  expect_identical(inspect_project(fixture$cfg)$subjects$status, "FAILED")
  DBI::dbExecute(con, "DELETE FROM job_submission_attempts")
  DBI::dbDisconnect(con)
  expect_identical(inspect_project(fixture$cfg, run_id = "latest")$run_id, "local-failure")
})

test_that("new tracking timestamps retain explicit UTC across worker transitions", {
  withr::local_envvar(TZ = "America/New_York")
  fixture <- make_recovery_audit_fixture("fmriprep")
  insert_tracked_job(fixture$db, "953")
  expect_match(get_tracked_job_status("953", sqlite_db = fixture$db)$time_submitted, "Z$")
  update_tracked_job_status(fixture$db, "953", "STARTED", strict = TRUE)
  expect_match(get_tracked_job_status("953", sqlite_db = fixture$db)$time_started, "Z$")
  update_tracked_job_status(fixture$db, "953", "COMPLETED", strict = TRUE)
  expect_match(get_tracked_job_status("953", sqlite_db = fixture$db)$time_ended, "Z$")
})
