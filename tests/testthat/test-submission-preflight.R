# Create a valid small project with two discoverable subjects and no real jobs.
make_preflight_project <- function() {
  root <- tempfile("submission-preflight-")
  withr::defer(unlink(root, recursive = TRUE), envir = parent.frame())
  cfg <- setup_project(project_name = "preflight", project_directory = root, interactive = FALSE)
  for (subject in c("01", "02")) dir.create(file.path(cfg$metadata$bids_directory, paste0("sub-", subject)))
  resources <- file.path(root, c("container.sif", "license.txt"))
  file.create(resources)
  cfg$fmriprep <- list(enable = TRUE, fs_license_file = resources[2], memgb = 8,
    nhours = 1, ncores = 2, output_spaces = "MNI152NLin2009cAsym")
  cfg$compute_environment$fmriprep_container <- resources[1]
  create_tracking_db(cfg$metadata$sqlite_db)
  execution <- resolve_project_execution(cfg, "fmriprep", subject_filter = "01")
  list(root = root, cfg = cfg, db = cfg$metadata$sqlite_db, execution = execution)
}

# Insert scalar attempts without creating contracts or reading runtime artifacts.
add_preflight_uncertain <- function(fixture, contract_id = "uncertain", sub_id = "01",
                                    stage = "fmriprep", stream = NULL, ses_id = NULL,
                                    state = "UNKNOWN", run_id = "older") {
  metadata <- list(stage = stage, stream = stream, sub_id = sub_id, ses_id = ses_id, sequence_id = run_id,
    scheduler = "slurm", job_name = if (is.null(sub_id)) stage else paste0(stage, "_sub-", sub_id))
  con <- job_submission_connect(fixture$db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, paste("INSERT INTO job_submission_attempts",
    "(contract_id, run_id, unit_key, job_role, submission_state, manifest_path, tracking_metadata, prepared_at)",
    "VALUES (?, ?, ?, 'subject', ?, 'missing-manifest.json', ?, '2026-10-23T12:00:00Z')"),
    params = list(contract_id, run_id, tracking_unit_key(stage, stream, sub_id, ses_id),
      state, as.character(jsonlite::toJSON(metadata, auto_unbox = TRUE, null = "null"))))
}

test_that("uncertain relevant work blocks before setup, provenance or marker changes", {
  fixture <- make_preflight_project()
  add_preflight_uncertain(fixture, sub_id = "02")
  marker_dir <- file.path(fixture$cfg$metadata$log_directory, "sub-02")
  dir.create(marker_dir)
  marker <- file.path(marker_dir, ".fmriprep_sub-02_complete")
  file.create(marker)
  local_mocked_bindings(
    setup_project_directories = function(...) stop("Unexpected directory preparation"),
    record_run_provenance = function(...) stop("Unexpected fingerprinting"),
    cluster_job_submit = function(...) stop("Unexpected submission"),
    scheduler_job_status = function(...) stop("No known allocation to query"))
  error <- tryCatch(run_project(fixture$cfg, steps = "fmriprep", force = TRUE),
    bg_submission_preflight_error = identity)
  expect_s3_class(error, "bg_submission_preflight_error")
  expect_identical(error$report$blockers$contract_id, "uncertain")
  expect_true(file.exists(marker))
  expect_match(conditionMessage(error), "force does not bypass")
})

test_that("unrelated stages, subjects, streams and sessions do not block", {
  fixture <- make_preflight_project()
  add_preflight_uncertain(fixture, "other-subject", sub_id = "02")
  add_preflight_uncertain(fixture, "other-stage", stage = "mriqc")
  insert_tracked_job(fixture$db, "unrelated", list(job_name = "fmriprep_sub-02",
    sequence_id = "older", scheduler = "slurm", status = "QUEUED"))
  local_mocked_bindings(scheduler_job_status = function(...) stop("Unrelated job must not be queried"))
  expect_equal(nrow(prepare_project_submission(fixture$cfg, fixture$execution)$blockers), 0L)
  execution <- fixture$execution
  execution$steps <- "postprocess"
  execution$postprocess_streams <- "clean"
  execution$subjects$ses_id <- "A"
  add_preflight_uncertain(fixture, "other-stream", stage = "postprocess", stream = "other", ses_id = "A")
  add_preflight_uncertain(fixture, "other-session", stage = "postprocess", stream = "clean", ses_id = "B")
  expect_equal(nrow(prepare_project_submission(fixture$cfg, execution)$blockers), 0L)
  add_preflight_uncertain(fixture, "match", stage = "postprocess", stream = "clean", ses_id = "A")
  error <- tryCatch(prepare_project_submission(fixture$cfg, execution), error = identity)
  expect_identical(error$report$blockers$contract_id, "match")
})

test_that("exact retry combinations cannot broaden into cross-product blockers", {
  fixture <- make_preflight_project()
  execution <- fixture$execution
  execution$steps <- c("fmriprep", "mriqc")
  execution$subjects <- data.frame(sub_id = c("01", "02"), ses_id = NA_character_)
  execution$work_units <- data.frame(stage = c("fmriprep", "mriqc"),
    sub_id = c("01", "02"), ses_id = NA_character_, stream = NA_character_)
  add_preflight_uncertain(fixture, "wrong-combination", sub_id = "02", stage = "fmriprep")
  expect_equal(nrow(prepare_project_submission(fixture$cfg, execution)$blockers), 0L)
})

test_that("automatic repairs are reported and active or missing jobs remain blockers", {
  fixture <- make_preflight_project()
  states <- c("CANCELLED", "RUNNING", "MISSING", "COMPLETED")
  for (index in seq_along(states)) insert_tracked_job(fixture$db, as.character(index), list(
    job_name = "fmriprep_sub-01", sequence_id = "older", scheduler = "slurm", status = "QUEUED"))
  local_mocked_bindings(scheduler_job_status = function(job_ids, scheduler) {
    data.frame(job_id = job_ids, scheduler_status = states[as.integer(job_ids)])
  })
  output <- capture.output(error <- tryCatch(prepare_project_submission(fixture$cfg, fixture$execution),
    error = identity), type = "message")
  expect_true(any(grepl("Automatically repaired 1", output, fixed = TRUE)))
  expect_identical(error$report$repairs$job_id, "1")
  expect_setequal(error$report$blockers$job_id, c("2", "3", "4"))
  expect_true(any(grepl("verify outputs", error$report$blockers$reason)))
  expect_identical(get_tracked_job_status("1", sqlite_db = fixture$db)$status, "CANCELLED")
})

test_that("dry-run preflight neither contacts schedulers nor repairs records", {
  fixture <- make_preflight_project()
  insert_tracked_job(fixture$db, "1", list(job_name = "fmriprep_sub-01",
    sequence_id = "older", scheduler = "slurm", status = "QUEUED"))
  local_mocked_bindings(scheduler_job_status = function(...) stop("Unexpected scheduler query"))
  preview <- run_project(fixture$cfg, steps = "fmriprep", subject_filter = "01", dry_run = TRUE)
  expect_true(preview$submission_preflight$dry_run)
  expect_equal(nrow(preview$submission_preflight$blockers), 1L)
  expect_identical(get_tracked_job_status("1", sqlite_db = fixture$db)$status, "QUEUED")
})

test_that("preflight rechecks worker changes and newly pending records after accounting", {
  fixture <- make_preflight_project()
  insert_tracked_job(fixture$db, "1", list(job_name = "fmriprep_sub-01",
    sequence_id = "older", scheduler = "slurm", status = "QUEUED"))
  local_mocked_bindings(scheduler_job_status = function(job_ids, scheduler) {
    update_tracked_job_status(fixture$db, "1", "COMPLETED", strict = TRUE)
    add_preflight_uncertain(fixture, "arrived-during-query")
    data.frame(job_id = job_ids, scheduler_status = "CANCELLED")
  })
  error <- tryCatch(prepare_project_submission(fixture$cfg, fixture$execution), error = identity)
  expect_identical(error$report$blockers$contract_id, "arrived-during-query")
  expect_equal(nrow(error$report$repairs), 0L)
  expect_identical(get_tracked_job_status("1", sqlite_db = fixture$db)$status, "COMPLETED")
})

test_that("accepted attempts missing their tracking row cannot silently disappear", {
  fixture <- make_preflight_project()
  add_preflight_uncertain(fixture, "missing-binding", state = "ACCEPTED")
  error <- tryCatch(prepare_project_submission(fixture$cfg, fixture$execution), error = identity)
  expect_identical(error$report$blockers$contract_id, "missing-binding")
})

test_that("deferred scopes are checked only after sync reveals new subjects and sessions", {
  fixture <- make_preflight_project()
  execution <- fixture$execution
  execution$steps <- c("flywheel_sync", "fmriprep")
  execution$scope_deferred <- TRUE
  add_preflight_uncertain(fixture, "new-subject", sub_id = "02")
  report <- prepare_project_submission(fixture$cfg, execution)
  expect_true(report$scope_deferred)
  expect_equal(nrow(report$blockers), 0L)
  execution$scope_deferred <- FALSE
  execution$subjects <- data.frame(sub_id = "02", ses_id = NA_character_)
  expect_error(prepare_project_submission(fixture$cfg, execution), class = "bg_submission_preflight_error")
  execution$scope_deferred <- TRUE
  add_preflight_uncertain(fixture, "setup", sub_id = NULL, stage = "fsaverage_setup")
  error <- tryCatch(prepare_project_submission(fixture$cfg, execution), error = identity)
  expect_identical(error$report$blockers$contract_id, "setup")
})

test_that("deferred controllers stop before participant work and allow their own setup jobs", {
  fixture <- make_preflight_project()
  execution <- fixture$execution
  add_preflight_uncertain(fixture, "new-subject", sub_id = "02")
  report_dir <- file.path(fixture$cfg$metadata$log_directory, "runs", "current")
  dir.create(report_dir, recursive = TRUE)
  local_mocked_bindings(process_subject = function(...) stop("Unexpected participant submission"))
  expect_error(submit_subjects(fixture$cfg, execution$step_flags,
    resolved_subjects = data.frame(sub_id = "02", ses_id = NA_character_),
    sequence_id = "current"), class = "bg_submission_preflight_error")
  saved <- jsonlite::read_json(file.path(report_dir, "submission-preflight.json"), simplifyVector = TRUE)
  expect_identical(saved$blockers$contract_id, "new-subject")
  insert_tracked_job(fixture$db, "setup", list(job_name = "fsaverage_setup",
    sequence_id = "current", scheduler = "slurm", status = "QUEUED"))
  insert_tracked_job(fixture$db, "controller", list(job_name = "submit_subjects",
    sequence_id = "current", scheduler = "slurm", status = "STARTED"))
  expect_equal(nrow(prepare_project_submission(fixture$cfg, execution,
    current_run_id = "current")$blockers), 0L)
})

test_that("new sessions discovered after sync are checked before streamed work", {
  fixture <- make_preflight_project()
  execution <- fixture$execution
  execution$steps <- c("flywheel_sync", "postprocess")
  execution$postprocess_streams <- "clean"
  execution$scope_deferred <- TRUE
  execution$subjects$ses_id <- "A"
  add_preflight_uncertain(fixture, "new-session", stage = "postprocess", stream = "clean", ses_id = "B")
  expect_equal(nrow(prepare_project_submission(fixture$cfg, execution)$blockers), 0L)
  execution$scope_deferred <- FALSE
  execution$subjects$ses_id <- "B"
  error <- tryCatch(prepare_project_submission(fixture$cfg, execution), error = identity)
  expect_identical(error$report$blockers$contract_id, "new-session")
})

test_that("an unavailable scheduler is not queried again for every batch", {
  fixture <- make_preflight_project()
  con <- job_submission_connect(fixture$db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbAppendTable(con, "job_tracking", data.frame(job_id = as.character(seq_len(401)),
    job_name = "fmriprep_sub-01", sequence_id = "older", scheduler = "slurm", status = "QUEUED"))
  calls <- 0L
  local_mocked_bindings(scheduler_job_status = function(job_ids, scheduler) {
    calls <<- calls + 1L
    data.frame(job_id = job_ids, scheduler_status = "UNAVAILABLE")
  })
  error <- tryCatch(prepare_project_submission(fixture$cfg, fixture$execution), error = identity)
  expect_equal(calls, 1L)
  expect_equal(nrow(error$report$blockers), 401L)
  expect_equal(nrow(error$report$repairs), 0L)
})

test_that("accounting is batched and large blobs or artifact files are never read", {
  fixture <- make_preflight_project()
  con <- job_submission_connect(fixture$db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  rows <- data.frame(job_id = as.character(seq_len(401)), job_name = "fmriprep_sub-01",
    sequence_id = "older", scheduler = "slurm", status = "QUEUED")
  DBI::dbAppendTable(con, "job_tracking", rows)
  DBI::dbExecute(con, "UPDATE job_tracking SET job_obj = zeroblob(2000000) WHERE job_id = '1'")
  calls <- integer()
  local_mocked_bindings(
    scheduler_job_status = function(job_ids, scheduler) {
      calls <<- c(calls, length(job_ids))
      data.frame(job_id = job_ids, scheduler_status = "CANCELLED")
    }, get_tracked_job_status = function(...) stop("Unexpected blob query"),
    verify_output_manifest = function(...) stop("Unexpected manifest verification"),
    collect_run_artifacts = function(...) stop("Unexpected artifact fingerprinting"))
  local_mocked_bindings(md5sum = function(...) stop("Unexpected file hashing"), .package = "tools")
  local_mocked_bindings(read_json = function(...) stop("Unexpected manifest read"), .package = "jsonlite")
  local_mocked_bindings(list.files = function(...) stop("Unexpected tree scan"), .package = "base")
  report <- prepare_project_submission(fixture$cfg, fixture$execution)
  expect_identical(calls, c(200L, 200L, 1L))
  expect_equal(nrow(report$repairs), 401L)
  expect_equal(nrow(report$blockers), 0L)
  expect_false("job_obj" %in% names(report$repairs))
})

test_that("assessment is independent of repairs, persistence, display and enforcement", {
  fixture <- make_preflight_project()
  insert_tracked_job(fixture$db, "77", list(job_name = "fmriprep_sub-01",
    sequence_id = "older", scheduler = "slurm", status = "QUEUED"))
  local_mocked_bindings(
    scheduler_job_status = function(job_ids, scheduler) {
      data.frame(job_id = job_ids, scheduler_status = "CANCELLED")
    },
    apply_tracked_job_repairs = function(...) stop("Assessment attempted repairs"),
    save_submission_preflight = function(...) stop("Assessment attempted persistence"),
    print_submission_preflight = function(...) stop("Assessment attempted presentation"),
    require_submission_ready = function(...) stop("Assessment attempted enforcement"))
  expect_no_message(report <- assess_project_submission(fixture$cfg, fixture$execution, refresh = TRUE))
  expect_identical(report$observations$action, "UPDATE_TERMINAL_FAILURE")
  expect_equal(nrow(report$repairs), 0L)
  expect_equal(nrow(report$blockers), 1L)
  expect_identical(get_tracked_job_status("77", sqlite_db = fixture$db)$status, "QUEUED")
})

test_that("diagnosis exposes submission-only problems with subject and run scope", {
  fixture <- make_preflight_project()
  add_preflight_uncertain(fixture, "first", sub_id = "01", run_id = "run-first")
  add_preflight_uncertain(fixture, "second", sub_id = "02", run_id = "run-second")
  snapshot <- inspect_project(fixture$cfg)
  local_mocked_bindings(
    apply_tracked_job_repairs = function(...) stop("Diagnosis attempted repairs"),
    scheduler_job_status = function(...) stop("Diagnosis queried a scheduler"))
  diagnosis <- diagnose_project(snapshot, subject_id = "01", interactive = FALSE)
  expect_equal(nrow(diagnosis$jobs), 0L)
  expect_identical(diagnosis$submission_issues$contract_id, "first")
  expect_equal(nrow(diagnosis$failures), 0L)
  output <- capture.output(print(diagnosis), type = "message")
  expect_true(any(grepl("submission outcome", output, fixed = TRUE)))
  expect_false(any(grepl("No failed jobs were found", output, fixed = TRUE)))
  scoped <- diagnose_project(fixture$cfg, run_id = "run-second", interactive = FALSE)
  expect_identical(scoped$submission_issues$contract_id, "second")
  expect_identical(read_job_submission_attempts(fixture$db)$submission_state, rep("UNKNOWN", 2L))
})

test_that("guided diagnosis offers recovery for submissions without job IDs", {
  fixture <- make_preflight_project()
  add_preflight_uncertain(fixture)
  answers <- c(1L, 3L)
  prompt_index <- 0L
  local_mocked_bindings(
    prompt_input = function(...) {
      prompt_index <<- prompt_index + 1L
      answers[[prompt_index]]
    },
    diagnosis_job_screen = function(...) stop("No scheduler-bound job to browse"),
    apply_tracked_job_repairs = function(...) stop("Review attempted repairs"))
  output <- capture.output(result <- diagnose_project(fixture$cfg, subject_id = "01",
    interactive = TRUE), type = "message")
  expect_s3_class(result, "bg_project_diagnosis")
  expect_identical(result$submission_issues$contract_id, "uncertain")
  expect_true(any(grepl("resolve_project_submission", output, fixed = TRUE)))
  expect_equal(prompt_index, 2L)
  expect_identical(read_job_submission_attempts(fixture$db)$submission_state, "UNKNOWN")
})

test_that("final controller reports are exposed without treating old blockers as current", {
  fixture <- make_preflight_project()
  execution <- fixture$execution
  execution$scope_deferred <- TRUE
  execution$submission_preflight <- assess_project_submission(fixture$cfg, execution)
  record_run_provenance(fixture$cfg, "controller-run", execution)
  insert_tracked_job(fixture$db, "controller", list(job_name = "submit_subjects",
    sequence_id = "controller-run", scheduler = "slurm", status = "COMPLETED"))
  add_preflight_uncertain(fixture, "first", sub_id = "01")
  add_preflight_uncertain(fixture, "second", sub_id = "02")
  execution$scope_deferred <- FALSE
  execution$subjects <- data.frame(sub_id = c("01", "02"), ses_id = NA_character_)
  report <- assess_project_submission(fixture$cfg, execution)
  report$dry_run <- FALSE
  report$observations <- NULL
  save_submission_preflight(fixture$cfg, "controller-run", report)
  before <- read_job_submission_attempts(fixture$db)
  provenance <- get_run_provenance(fixture$cfg, "controller-run")
  expect_false(provenance$submission_preflight$scope_deferred)
  expect_identical(provenance$submission_preflight$source, "controller")
  expect_equal(nrow(provenance$submission_preflight$blockers), 2L)
  focused <- inspect_project(fixture$cfg, subject_id = "01", run_id = "controller-run")
  expect_identical(focused$submission_preflight$blockers$contract_id, "first")
  resolve_project_submission(fixture$cfg, "first", rejection_reason = "Administrator confirmed rejection.")
  resolve_project_submission(fixture$cfg, "second", rejection_reason = "Administrator confirmed rejection.")
  add_preflight_uncertain(fixture, "first-final", sub_id = "01", state = "ACCEPTED", run_id = "later-run")
  add_preflight_uncertain(fixture, "second-final", sub_id = "02", state = "ACCEPTED", run_id = "later-run")
  bind_job_submission(fixture$db, "first-final", "101")
  bind_job_submission(fixture$db, "second-final", "102")
  update_tracked_job_status(fixture$db, "101", "COMPLETED", strict = TRUE)
  update_tracked_job_status(fixture$db, "102", "COMPLETED", strict = TRUE)
  diagnosis <- diagnose_project(fixture$cfg, run_id = "controller-run", interactive = FALSE)
  expect_equal(nrow(diagnosis$submission_issues), 0L)
  expect_equal(nrow(diagnosis$submission_preflight$blockers), 2L)
  expect_equal(nrow(diagnosis$failures), 0L)
  expect_equal(nrow(diagnose_project(fixture$cfg, interactive = FALSE)$submission_issues), 0L)
  expect_identical(before$submission_state, rep("UNKNOWN", 2L))
})

test_that("unreadable controller reports retain deferred initial evidence", {
  fixture <- make_preflight_project()
  execution <- fixture$execution
  execution$scope_deferred <- TRUE
  execution$submission_preflight <- assess_project_submission(fixture$cfg, execution)
  record_run_provenance(fixture$cfg, "broken-report", execution)
  report_file <- file.path(fixture$cfg$metadata$log_directory, "runs", "broken-report", "submission-preflight.json")
  writeLines("{ invalid", report_file)
  expect_warning(provenance <- get_run_provenance(fixture$cfg, "broken-report"),
    "Could not read submission preflight")
  expect_true(provenance$submission_preflight$scope_deferred)
  expect_true(nzchar(provenance$submission_preflight$read_error))
  unlink(report_file)
  provenance <- get_run_provenance(fixture$cfg, "broken-report")
  expect_s3_class(provenance$submission_preflight$blockers, "data.frame")
  expect_equal(nrow(provenance$submission_preflight$blockers), 0L)
})

test_that("the internal redesign preserves the public recovery API", {
  exports <- getNamespaceExports("BrainGnomes")
  expect_true(all(c("reconcile_project_jobs", "resolve_project_submission") %in% exports))
  expect_false(any(c("assess_project_submission", "prepare_project_submission",
    "reconcile_tracked_jobs", "apply_tracked_job_repairs", "require_submission_ready") %in% exports))
  expect_false(exists("reconcile_submission_jobs", asNamespace("BrainGnomes"), inherits = FALSE))
  expect_false(exists("submission_preflight", asNamespace("BrainGnomes"), inherits = FALSE))
})
