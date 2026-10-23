# Create isolated project records for scheduler recovery without real jobs.
make_recovery_fixture <- function() {
  root <- tempfile("project-recovery-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE), envir = parent.frame())
  db <- file.path(root, "tracking.sqlite")
  create_tracking_db(db)
  cfg <- structure(list(metadata = list(project_name = "recovery",
    project_directory = root, sqlite_db = db)), class = "bg_project_cfg")
  list(root = root, db = db, cfg = cfg)
}

# Register a sealed subject attempt so tests exercise real guard transitions.
add_recovery_attempt <- function(fixture) {
  tracking <- list(job_name = "fmriprep_sub-01", sequence_id = "run-one",
    scheduler = "slurm", stage = "fmriprep", sub_id = "01",
    job_role = "subject", unit_key = "subject::01::::fmriprep::", attempt = 1L)
  contract <- allocate_job_contract(fixture$db, tracking)
  manifest <- write_job_manifest(contract, "true", "sbatch", "/fake/sbatch", "",
    character(), FALSE, NULL, "afterok", "sbatch", tracking)
  tracking$job_manifest_path <- manifest$path
  tracking$job_manifest_checksum <- manifest$checksum
  register_job_submission(fixture$db, contract, tracking)
  contract$contract_id
}

test_that("inspection parses mixed local and UTC timestamps independently", {
  times <- c("2026-10-19 17:42:20.123456", "2026-10-19T21:42:20.123456Z",
             "2026-10-19T17:42:20.123456-04:00", "not-a-time", NA, "")
  parsed <- parse_inspection_time(times, "America/New_York")
  expect_equal(as.numeric(parsed[1:3]), rep(as.numeric(parsed[[1L]]), 3L))
  expect_true(all(is.na(parsed[4:6])))
  expect_length(parse_inspection_time(character()), 0L)
  expect_identical(parse_inspection_time(parsed), parsed)
})

test_that("recovery previews failures and never infers success or missing outcomes", {
  fixture <- make_recovery_fixture()
  states <- c("CANCELLED", "FAILED", "COMPLETED", "MISSING", "UNAVAILABLE", "RUNNING")
  for (index in seq_along(states)) {
    insert_tracked_job(fixture$db, as.character(index), list(
      job_name = paste0("fmriprep_sub-", index), sequence_id = "run-one",
      scheduler = "slurm", status = "QUEUED"))
  }
  local_mocked_bindings(scheduler_job_status = function(job_ids, scheduler) {
    data.frame(job_id = job_ids, scheduler_status = states[as.integer(job_ids)])
  })
  before <- get_tracked_job_status(sequence_id = "run-one", sqlite_db = fixture$db)
  preview <- reconcile_project_jobs(fixture$cfg)
  expect_identical(preview$action, c(rep("UPDATE_TERMINAL_FAILURE", 2L),
    "VERIFY_OUTPUTS", "CONFIRM_OUTCOME", "CONFIRM_OUTCOME", "UNCHANGED"))
  expect_false(any(preview$applied))
  expect_identical(get_tracked_job_status(sequence_id = "run-one", sqlite_db = fixture$db), before)
  applied <- reconcile_project_jobs(fixture$cfg, dry_run = FALSE)
  expect_identical(applied$applied, c(TRUE, TRUE, rep(FALSE, 4L)))
  after <- get_tracked_job_status(sequence_id = "run-one", sqlite_db = fixture$db)
  expect_identical(after$status, c("CANCELLED", "FAILED", rep("QUEUED", 4L)))
  expect_true(all(!is.na(after$time_ended[1:2])))
})

test_that("empty projects have no recovery work and do not create databases", {
  fixture <- make_recovery_fixture()
  expect_equal(nrow(reconcile_project_jobs(fixture$cfg)), 0L)
  fixture$cfg$metadata$sqlite_db <- file.path(fixture$root, "not-created.sqlite")
  expect_equal(nrow(reconcile_project_jobs(fixture$cfg)), 0L)
  expect_false(file.exists(fixture$cfg$metadata$sqlite_db))
})

test_that("legacy tracking schemas are migrated only when applying recovery", {
  fixture <- make_recovery_fixture()
  insert_tracked_job(fixture$db, "100", list(job_name = "fmriprep_sub-01",
    sequence_id = "run-old", scheduler = "slurm", status = "QUEUED"))
  con <- job_submission_connect(fixture$db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "DROP INDEX IF EXISTS job_tracking_pending_v1")
  DBI::dbExecute(con, "DROP INDEX IF EXISTS job_tracking_unit_role")
  for (column in c("contract_id", "unit_key", "job_role", "scheduler_terminal_state", "failure_category")) {
    DBI::dbExecute(con, paste("ALTER TABLE job_tracking DROP COLUMN", column))
  }
  local_mocked_bindings(scheduler_job_status = function(job_ids, scheduler) {
    data.frame(job_id = job_ids, scheduler_status = "CANCELLED")
  })
  preview <- reconcile_project_jobs(fixture$cfg)
  expect_identical(preview$action, "UPDATE_TERMINAL_FAILURE")
  expect_false("contract_id" %in% DBI::dbListFields(con, "job_tracking"))
  expect_true(reconcile_project_jobs(fixture$cfg, dry_run = FALSE)$applied)
  expect_true("contract_id" %in% DBI::dbGetQuery(con, "PRAGMA table_info(job_tracking)")$name)
  expect_identical(get_tracked_job_status("100", sqlite_db = fixture$db)$status, "CANCELLED")
})

test_that("retry guard recovers confirmed cancellations but blocks uncertainty", {
  fixture <- make_recovery_fixture()
  first <- add_recovery_attempt(fixture)
  set_job_submission_state(fixture$db, first, "SUBMITTING")
  bind_job_submission(fixture$db, first, "101")
  second <- add_recovery_attempt(fixture)
  observed <- "MISSING"
  local_mocked_bindings(scheduler_job_status = function(job_ids, scheduler) {
    data.frame(job_id = job_ids, scheduler_status = observed)
  })
  expect_error(set_job_submission_state(fixture$db, second, "SUBMITTING"), "duplicate launch")
  expect_identical(get_tracked_job_status("101", sqlite_db = fixture$db)$status, "QUEUED")
  observed <- "CANCELLED"
  expect_no_error(set_job_submission_state(fixture$db, second, "SUBMITTING"))
  expect_identical(get_tracked_job_status("101", sqlite_db = fixture$db)$status, "CANCELLED")
})

test_that("reconciliation preserves worker changes made during scheduler observation", {
  fixture <- make_recovery_fixture()
  first <- add_recovery_attempt(fixture)
  bind_job_submission(fixture$db, first, "102")
  local_mocked_bindings(scheduler_job_status = function(job_ids, scheduler) {
    update_tracked_job_status(fixture$db, "102", "COMPLETED", strict = TRUE)
    data.frame(job_id = job_ids, scheduler_status = "CANCELLED")
  })
  result <- reconcile_project_jobs(fixture$cfg, dry_run = FALSE)
  expect_false(result$applied[[1L]])
  expect_identical(get_tracked_job_status("102", sqlite_db = fixture$db)$status, "COMPLETED")
})

test_that("uncertain outcomes can be explicitly resolved with retained evidence", {
  fixture <- make_recovery_fixture()
  first <- add_recovery_attempt(fixture)
  set_job_submission_state(fixture$db, first, "UNKNOWN")
  expect_error(resolve_project_submission(fixture$cfg, first), "exactly one")
  expect_error(add_recovery_attempt(fixture), "unconfirmed")
  resolved <- resolve_project_submission(fixture$cfg, first,
    rejection_reason = "Scheduler administrator confirmed request rejection.")
  expect_identical(resolved$submission_state, "REJECTED")
  expect_match(resolved$submission_detail, "administrator confirmed")
  second <- add_recovery_attempt(fixture)
  set_job_submission_state(fixture$db, second, "UNKNOWN")
  resolve_project_submission(fixture$cfg, second, scheduler_job_id = "103")
  update_tracked_job_status(fixture$db, "103", "COMPLETED", strict = TRUE)
  expect_error(resolve_project_submission(fixture$cfg, second,
    rejection_reason = "Not permitted for bound jobs"), "unbound uncertain")
  resolve_project_submission(fixture$cfg, second, scheduler_job_id = "103")
  expect_identical(get_tracked_job_status("103", sqlite_db = fixture$db)$status, "COMPLETED")
})

test_that("only explicit scheduler rejections permit automatic retry", {
  expect_true(is_confirmed_submission_rejection("sbatch", 1L, NULL,
    "sbatch: error: Batch job submission failed: Job violates accounting/QOS policy"))
  expect_true(is_confirmed_submission_rejection("sbatch", 1L, NULL,
    "sbatch: error: Batch job submission failed: Invalid account or account/partition combination specified"))
  expect_false(is_confirmed_submission_rejection("sbatch", 1L, NULL,
    "sbatch: error: Batch job submission failed: Socket timed out on send/recv operation"))
  expect_false(is_confirmed_submission_rejection("sbatch", 1L, "123",
    "sbatch: error: Batch job submission failed: Invalid account"))
  expect_false(is_confirmed_submission_rejection("qsub", 1L, NULL, "Invalid account"))
})

test_that("conversion locks block BIDS readers but do not affect output-only work", {
  fixture <- make_recovery_fixture()
  fixture$cfg$metadata$bids_directory <- fixture$root
  expect_null(project_bids_lock_issue(fixture$cfg, "fmriprep"))
  file.create(file.path(fixture$root, "heudiconv.lock"))
  expect_match(project_bids_lock_issue(fixture$cfg, "fmriprep"), "confirm no conversion is active")
  expect_null(project_bids_lock_issue(fixture$cfg, "postprocess"))
  local_mocked_bindings(resolve_project_selection = function(...) list(steps = "fmriprep"))
  expect_error(resolve_project_execution(fixture$cfg, "fmriprep"), "BIDS conversion lock")
})
