make_inspection_project <- function() {
  root <- tempfile("inspection-project-")
  dir.create(root)
  log_dir <- file.path(root, "logs")
  dir.create(log_dir)
  db <- file.path(root, "tracking.sqlite")
  BrainGnomes:::create_tracking_db(db)
  cfg <- structure(list(
    metadata = list(
      project_name = "inspection",
      project_directory = root,
      log_directory = log_dir,
      sqlite_db = db
    )
  ), class = "bg_project_cfg")
  list(root = root, log_dir = log_dir, db = db, cfg = cfg)
}

add_inspection_job <- function(db, job_id, job_name, run_id, status,
                               scheduler_options = "--constraint=a-very-long-value") {
  insert_tracked_job(db, job_id, list(
    job_name = job_name,
    sequence_id = run_id,
    status = status,
    scheduler = "slurm",
    scheduler_options = scheduler_options
  ))
}

test_that("inspect_project selects the latest attempt for each logical work unit", {
  fixture <- make_inspection_project()
  on.exit(unlink(fixture$root, recursive = TRUE, force = TRUE), add = TRUE)

  add_inspection_job(fixture$db, "100", "fmriprep_sub-01", "run-old", "FAILED")
  add_inspection_job(fixture$db, "101", "mriqc_sub-02", "run-old", "COMPLETED")
  add_inspection_job(fixture$db, "102", "fmriprep_sub-02_ses-A", "run-old", "COMPLETED")
  add_inspection_job(fixture$db, "200", "fmriprep_sub-01", "run-new", "COMPLETED")
  add_inspection_job(fixture$db, "201", "postprocess_rest_clean_sub-01_ses-A", "run-new", "COMPLETED")
  add_inspection_job(fixture$db, "202", "postprocess_rest_clean_sentinel", "run-new", "QUEUED")
  add_tracked_job_parent(fixture$db, "202", "201")
  add_inspection_job(fixture$db, "203", "postprocess_rest_clean_array", "run-new", "STARTED")
  add_tracked_job_parent(fixture$db, "203", "201", child_level = 2L)
  add_inspection_job(fixture$db, "204", "fmriprep_sub-02", "run-new", "FAILED")

  con <- DBI::dbConnect(RSQLite::SQLite(), fixture$db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "UPDATE job_tracking SET time_submitted = '2026-09-01 10:00:00' WHERE sequence_id = 'run-old'")
  DBI::dbExecute(con, "UPDATE job_tracking SET time_submitted = '2026-09-02 10:00:00' WHERE sequence_id = 'run-new'")

  status <- inspect_project(fixture$cfg)

  expect_s3_class(status, "bg_project_inspection")
  expect_s3_class(status$jobs, "bg_run_jobs")
  expect_s3_class(status$jobs, "data.frame")
  expect_equal(status$overview$overall_status, "IN_PROGRESS_WITH_FAILURES")
  expect_equal(status$overview$n_runs, 2L)
  expect_equal(status$overview$n_units, 4L)
  expect_equal(status$overview$n_completed, 2L)
  expect_equal(status$overview$n_running, 1L)
  expect_equal(status$overview$n_failed, 1L)

  fmriprep_01 <- subset(status$subject_stages, sub_id == "01" & stage == "fmriprep")
  expect_equal(fmriprep_01$status, "COMPLETED")
  expect_equal(fmriprep_01$run_id, "run-new")
  old_fmriprep <- status$attempts$is_current[
    status$attempts$run_id == "run-old" & status$attempts$stage == "fmriprep"
  ]
  expect_true(all(!old_fmriprep))

  fmriprep_02 <- subset(status$subject_stages, sub_id == "02" & stage == "fmriprep")
  expect_equal(fmriprep_02$status, "FAILED")
  expect_true(is.na(fmriprep_02$ses_id))

  postprocess <- subset(status$subject_stages, stage == "postprocess")
  expect_equal(postprocess$stream, "rest_clean")
  expect_equal(postprocess$sub_id, "01")
  expect_equal(postprocess$ses_id, "A")
  expect_equal(postprocess$status, "RUNNING")
  expect_equal(postprocess$n_jobs, 3L)

  inherited <- subset(status$jobs, job_id %in% c("202", "203"))
  expect_true(all(inherited$sub_id == "01"))
  expect_true(all(inherited$ses_id == "A"))
  expect_setequal(inherited$job_role, c("sentinel", "array"))

  job_output <- capture.output(
    job_messages <- capture.output(print(status$jobs), type = "message")
  )
  job_output <- c(job_output, job_messages)
  expect_true(any(grepl("Historical job rows retained", job_output, fixed = TRUE)))
  expect_false(any(grepl("constraint=a-very-long-value", job_output, fixed = TRUE)))
})

test_that("latest run selection breaks timestamp ties by tracking order", {
  fixture <- make_inspection_project()
  on.exit(unlink(fixture$root, recursive = TRUE, force = TRUE), add = TRUE)
  add_inspection_job(fixture$db, "600", "mriqc_sub-01", "run-first", "COMPLETED")
  add_inspection_job(fixture$db, "601", "mriqc_sub-01", "run-second", "FAILED")

  con <- DBI::dbConnect(RSQLite::SQLite(), fixture$db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "UPDATE job_tracking SET time_submitted = '2026-09-02 10:00:00'")

  latest <- inspect_project(fixture$cfg, run_id = "latest")
  current <- inspect_project(fixture$cfg)
  expect_equal(latest$run_id, "run-second")
  expect_equal(current$subject_stages$status, "FAILED")
})

test_that("inspect_project supports run scope and structured resolutions", {
  fixture <- make_inspection_project()
  on.exit(unlink(fixture$root, recursive = TRUE, force = TRUE), add = TRUE)
  add_inspection_job(fixture$db, "300", "mriqc_sub-01", "run-one", "COMPLETED")
  add_inspection_job(fixture$db, "301", "fmriprep_sub-01", "run-one", "QUEUED")

  status <- inspect_project(fixture$cfg, run_id = "latest")

  expect_equal(status$scope, "run")
  expect_equal(status$run_id, "run-one")
  expect_equal(status$overview$overall_status, "IN_PROGRESS")
  expect_s3_class(summary(status), "data.frame")
  expect_equal(summary(status, by = "stage"), status$stages)
  expect_equal(summary(status, by = "subject"), status$subjects)
  expect_equal(summary(status, by = "subject_stage"), status$subject_stages)
  expect_equal(summary(status, by = "runs"), status$runs)
  expect_equal(nrow(status$runs), 1L)
  expect_equal(summary(status, by = "attempt"), status$attempts)
  expect_equal(summary(status, by = "job"), status$jobs)

  output <- capture.output(
    messages <- capture.output(print(status), type = "message")
  )
  output <- c(output, messages)
  expect_true(any(grepl("Work units:", output, fixed = TRUE)))
  expect_true(any(grepl("Stage and stream progress", output, fixed = TRUE)))
  expect_false(any(grepl("scheduler_options", output, fixed = TRUE)))

  expect_error(
    inspect_project(fixture$cfg, run_id = "missing"),
    "No tracked project run"
  )
})

test_that("diagnose_project uses current failures or a selected historical run", {
  fixture <- make_inspection_project()
  on.exit(unlink(fixture$root, recursive = TRUE, force = TRUE), add = TRUE)
  add_inspection_job(fixture$db, "400", "fmriprep_sub-01", "run-old", "FAILED")
  add_inspection_job(fixture$db, "401", "fmriprep_sub-01", "run-new", "COMPLETED")
  add_inspection_job(fixture$db, "402", "mriqc_sub-02", "run-new", "FAILED")

  con <- DBI::dbConnect(RSQLite::SQLite(), fixture$db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "UPDATE job_tracking SET time_submitted = '2026-09-01 10:00:00' WHERE sequence_id = 'run-old'")
  DBI::dbExecute(con, "UPDATE job_tracking SET time_submitted = '2026-09-02 10:00:00' WHERE sequence_id = 'run-new'")

  dir.create(file.path(fixture$log_dir, "sub-02"))
  failure_log <- file.path(fixture$log_dir, "sub-02", "mriqc_jobid-402.err")
  writeLines("current failure", failure_log)

  current <- diagnose_project(fixture$cfg, interactive = FALSE)
  expect_equal(current$scope, "project")
  expect_equal(current$failures$job_id, "402")
  expect_equal(current$logs$path, failure_log)

  from_snapshot <- diagnose_project(
    inspect_project(fixture$cfg), interactive = FALSE
  )
  expect_equal(from_snapshot$failures$job_id, "402")

  historical <- diagnose_project(
    fixture$cfg, run_id = "run-old", interactive = FALSE
  )
  expect_equal(historical$scope, "run")
  expect_equal(historical$failures$job_id, "400")
})

test_that("diagnose_project defaults to the R session's interactivity", {
  expect_identical(
    formals(diagnose_project)$interactive,
    quote(base::interactive())
  )

  fixture <- make_inspection_project()
  on.exit(unlink(fixture$root, recursive = TRUE, force = TRUE), add = TRUE)
  local_mocked_bindings(
    .diagnose_pipeline_interactive = function(input, run_id = NULL) {
      expect_identical(input, fixture$cfg)
      expect_identical(run_id, "run-selected")
      "guided-browser"
    },
    .package = "BrainGnomes"
  )

  result <- diagnose_project(
    fixture$cfg, run_id = "run-selected", interactive = TRUE
  )
  expect_identical(result, "guided-browser")
})

test_that("inspection and diagnosis default to the current project directory", {
  expect_identical(formals(inspect_project)$input, quote(getwd()))
  expect_identical(formals(diagnose_project)$input, quote(getwd()))
  expect_identical(formals(diagnose_pipeline)$input, quote(getwd()))

  fixture <- make_inspection_project()
  on.exit(unlink(fixture$root, recursive = TRUE, force = TRUE), add = TRUE)
  add_inspection_job(
    fixture$db, "450", "mriqc_sub-01", "run-current", "FAILED"
  )
  yaml::write_yaml(
    as.list(fixture$cfg), file.path(fixture$root, "project_config.yaml")
  )

  from_directory <- inspect_project(fixture$root)
  expect_equal(
    from_directory$jobs$job_id[
      from_directory$jobs$lifecycle_status == "FAILED"
    ],
    "450"
  )

  withr::local_dir(fixture$root)
  from_cwd <- inspect_project()
  expect_equal(
    from_cwd$jobs$job_id[from_cwd$jobs$lifecycle_status == "FAILED"],
    "450"
  )

  diagnosis <- diagnose_project(interactive = FALSE)
  expect_equal(diagnosis$failures$job_id, "450")

  local_mocked_bindings(
    .diagnose_pipeline_interactive = function(input, run_id = NULL) {
      expect_s3_class(input, "bg_project_cfg")
      expect_equal(input$metadata$project_directory, fixture$root)
      expect_null(run_id)
      "guided-from-cwd"
    },
    .package = "BrainGnomes"
  )
  expect_warning(
    guided <- diagnose_pipeline(),
    "diagnose_project\\(\\.\\.\\., interactive = TRUE\\)"
  )
  expect_identical(guided, "guided-from-cwd")
})

test_that("current-directory discovery requires a project configuration", {
  empty_dir <- tempfile("not-a-project-")
  dir.create(empty_dir)
  on.exit(unlink(empty_dir, recursive = TRUE, force = TRUE), add = TRUE)
  withr::local_dir(empty_dir)

  expect_error(
    inspect_project(),
    "No project_config.yaml found in project directory"
  )
  expect_error(
    diagnose_project(interactive = FALSE),
    "No project_config.yaml found in project directory"
  )
  expect_error(
    suppressWarnings(diagnose_pipeline()),
    "No project_config.yaml found in project directory"
  )
})

test_that("empty inspections and deprecated run getters retain stable contracts", {
  fixture <- make_inspection_project()
  on.exit(unlink(fixture$root, recursive = TRUE, force = TRUE), add = TRUE)

  empty <- inspect_project(fixture$cfg)
  expect_equal(empty$overview$overall_status, "EMPTY")
  expect_equal(nrow(empty$jobs), 0L)
  expect_equal(nrow(empty$subjects), 0L)

  add_inspection_job(fixture$db, "500", "mriqc_sub-01", "run-one", "QUEUED")
  expect_warning(runs <- get_project_runs(fixture$cfg), "deprecated")
  expect_warning(jobs <- get_run_jobs(fixture$cfg), "deprecated")
  expect_s3_class(runs, "data.frame")
  expect_s3_class(jobs, "bg_run_jobs")
  expect_s3_class(jobs, "data.frame")
  expect_equal(jobs$sub_id, "01")
  output <- capture.output(
    messages <- capture.output(print(jobs), type = "message")
  )
  output <- c(output, messages)
  expect_false(any(grepl("constraint=a-very-long-value", output, fixed = TRUE)))
})

test_that("diagnose_pipeline delegates to the consolidated interactive diagnosis", {
  fixture <- make_inspection_project()
  on.exit(unlink(fixture$root, recursive = TRUE, force = TRUE), add = TRUE)
  local_mocked_bindings(
    .diagnose_pipeline_interactive = function(input, run_id = NULL) {
      expect_identical(input, fixture$cfg)
      expect_null(run_id)
      "guided-browser"
    },
    .package = "BrainGnomes"
  )

  expect_warning(
    result <- diagnose_pipeline(fixture$cfg),
    "deprecated"
  )
  expect_identical(result, "guided-browser")
})
