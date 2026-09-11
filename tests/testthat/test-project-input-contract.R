make_project_input_fixture <- function() {
  root <- tempfile("project-input-")
  cfg <- setup_project(
    project_name = "input-contract",
    project_directory = root,
    interactive = FALSE
  )
  list(
    root = root,
    config_file = file.path(root, "project_config.yaml"),
    cfg = cfg
  )
}

test_that("project configuration inputs resolve consistently", {
  fixture <- make_project_input_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE, force = TRUE), add = TRUE)

  inputs <- list(fixture$cfg, fixture$config_file, fixture$root)
  resolved <- lapply(inputs, BrainGnomes:::project_config_from_input)

  expect_true(all(vapply(resolved, inherits, logical(1), "bg_project_cfg")))
  expect_true(all(vapply(
    resolved,
    function(x) identical(x$metadata$project_directory, fixture$root),
    logical(1)
  )))

  withr::local_dir(fixture$root)
  from_cwd <- BrainGnomes:::project_config_from_input()
  expect_s3_class(from_cwd, "bg_project_cfg")
  expect_identical(from_cwd$metadata$project_directory, fixture$root)
})

test_that("loading accepts every project input form", {
  fixture <- make_project_input_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE, force = TRUE), add = TRUE)

  expect_identical(load_project(fixture$cfg, validate = FALSE), fixture$cfg)
  expect_s3_class(load_project(fixture$config_file, validate = FALSE), "bg_project_cfg")
  expect_s3_class(load_project(fixture$root, validate = FALSE), "bg_project_cfg")

  withr::local_dir(fixture$root)
  expect_s3_class(load_project(validate = FALSE), "bg_project_cfg")
})

test_that("project lifecycle entry points default to the current directory", {
  input_functions <- c(
    "load_project", "edit_project", "validate_project_config",
    "doctor_project", "plan_project", "get_project_runs",
    "get_run_jobs", "find_run_logs", "inspect_project", "diagnose_project",
    "cancel_project_run", "retry_project_run", "get_run_provenance"
  )
  for (fn in input_functions) {
    expect_identical(
      formals(get(fn, envir = asNamespace("BrainGnomes")))$input,
      quote(getwd()),
      info = fn
    )
  }

  scfg_functions <- c(
    "run_project", "run_bids_validation", "get_project_status",
    "get_subject_status"
  )
  for (fn in scfg_functions) {
    expect_identical(
      formals(get(fn, envir = asNamespace("BrainGnomes")))$scfg,
      quote(getwd()),
      info = fn
    )
  }
})

test_that("current-directory defaults load real project state", {
  fixture <- make_project_input_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE, force = TRUE), add = TRUE)
  withr::local_dir(fixture$root)

  expect_s3_class(validate_project_config(quiet = TRUE), "bg_project_validation")
  expect_s3_class(doctor_project(quiet = TRUE), "bg_project_doctor")
  expect_s3_class(inspect_project(), "bg_project_inspection")
  expect_s3_class(get_project_status(), "bg_status_df")
  expect_error(
    run_project(steps = "all", dry_run = TRUE),
    "No enabled processing steps were requested"
  )
  expect_error(
    run_bids_validation(),
    "without a bids_validator location"
  )
})

test_that("run entry points accept project directories and YAML files", {
  fixture <- make_project_input_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE, force = TRUE), add = TRUE)

  for (input in c(fixture$root, fixture$config_file)) {
    expect_error(
      run_project(input, steps = "all", dry_run = TRUE),
      "No enabled processing steps were requested"
    )
    expect_error(
      run_bids_validation(input),
      "without a bids_validator location"
    )
  }
})
