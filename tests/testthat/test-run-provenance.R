make_provenance_project <- function() {
  root <- tempfile("run-provenance-")
  cfg <- initialize_project("provenance", root, interactive = FALSE)
  dir.create(file.path(cfg$metadata$bids_directory, "sub-01"), recursive = TRUE)
  container <- file.path(root, "fmriprep.sif")
  license <- file.path(root, "license.txt")
  writeLines("container identity", container)
  writeLines("license identity", license)
  cfg$fmriprep <- list(
    enable = TRUE, output_spaces = "MNI152NLin2009cAsym",
    fs_license_file = license, memgb = 8, nhours = 1, ncores = 2,
    cli_options = NULL, sched_args = NULL
  )
  cfg$compute_environment$fmriprep_container <- container
  cfg <- write_project_config(cfg, overwrite = TRUE)
  list(root = root, cfg = cfg, container = container, license = license)
}

test_that("run provenance captures the complete resolved submission context", {
  fixture <- make_provenance_project()
  on.exit(unlink(fixture$root, recursive = TRUE, force = TRUE), add = TRUE)
  cfg <- fixture$cfg
  attr(cfg, "provenance_context") <- list(
    interface = "saved_plan", plan_id = "plan-123"
  )
  execution <- BrainGnomes:::resolve_project_execution(
    cfg, steps = "fmriprep", subject_filter = "01", force = TRUE
  )
  run_id <- "provenance-run-1"
  provenance_file <- BrainGnomes:::record_run_provenance(
    cfg, run_id, execution, debug = TRUE, log_level = "DEBUG"
  )

  expect_true(file.exists(provenance_file))
  expect_true(file.exists(file.path(dirname(provenance_file), "project_config.yaml")))
  expect_true(file.exists(file.path(dirname(provenance_file), "subjects.tsv")))

  provenance <- get_run_provenance(cfg, run_id)
  expect_s3_class(provenance, "bg_run_provenance")
  expect_identical(
    provenance$schema_version, "brain-gnomes-run-provenance-v1"
  )
  expect_identical(provenance$invocation$interface, "saved_plan")
  expect_identical(provenance$invocation$plan_id, "plan-123")
  expect_identical(provenance$request$steps, "fmriprep")
  expect_identical(provenance$request$subject_filter, "01")
  expect_true(provenance$request$force)
  expect_true(provenance$request$debug)
  expect_identical(provenance$request$log_level, "DEBUG")
  expect_identical(provenance$execution$subjects$sub_id, "01")
  expect_identical(provenance$execution$scope_status, "resolved")
  expect_setequal(
    provenance$execution$job_plan$stage,
    c("fsaverage_setup", "prefetch_templates", "fmriprep")
  )
  expect_identical(
    provenance$configuration$snapshot_checksum,
    unname(tools::md5sum(provenance$configuration$snapshot_file))
  )
  container_row <- provenance$artifacts[
    provenance$artifacts$role == "fmriprep.fmriprep_container", , drop = FALSE
  ]
  expect_equal(nrow(container_row), 1L)
  expect_identical(container_row$checksum, unname(tools::md5sum(fixture$container)))
  expect_true(nzchar(provenance$software$braingnomes$version))
  expect_true(nzchar(provenance$software$r$version))
  expect_true(nzchar(provenance$host$system$nodename))
})

test_that("provenance links submitted and current scheduler jobs", {
  fixture <- make_provenance_project()
  on.exit(unlink(fixture$root, recursive = TRUE, force = TRUE), add = TRUE)
  execution <- BrainGnomes:::resolve_project_execution(
    fixture$cfg, steps = "fmriprep", subject_filter = "01"
  )
  run_id <- "provenance-run-2"
  BrainGnomes:::record_run_provenance(
    fixture$cfg, run_id, execution, debug = FALSE, log_level = "INFO"
  )
  insert_tracked_job(
    fixture$cfg$metadata$sqlite_db, "99101",
    list(
      job_name = "fmriprep_sub-01", sequence_id = run_id,
      n_nodes = 1, n_cpus = 2, scheduler = "slurm"
    )
  )
  BrainGnomes:::update_run_provenance_submission(
    fixture$cfg, run_id, submitted_ids = "99101"
  )

  provenance <- get_run_provenance(fixture$cfg, "latest")
  expect_identical(provenance$run_id, run_id)
  expect_identical(provenance$state, "submitted")
  expect_identical(provenance$submission$submitted_job_ids, "99101")
  expect_identical(provenance$current_jobs$job_id, "99101")
  expect_identical(provenance$current_jobs$sequence_id, run_id)
})

test_that("deferred submission finalizes the resolved subject scope", {
  fixture <- make_provenance_project()
  on.exit(unlink(fixture$root, recursive = TRUE, force = TRUE), add = TRUE)
  fixture$cfg$flywheel_sync <- list(
    enable = TRUE, source_url = "fw://example/project"
  )
  execution <- BrainGnomes:::resolve_project_execution(
    fixture$cfg, steps = c("flywheel_sync", "fmriprep"),
    subject_filter = "01"
  )
  run_id <- "provenance-run-deferred"
  BrainGnomes:::record_run_provenance(
    fixture$cfg, run_id, execution, debug = FALSE, log_level = "INFO"
  )
  before <- get_run_provenance(fixture$cfg, run_id)
  expect_identical(before$execution$scope_status, "deferred")

  resolved <- BrainGnomes:::discover_project_subjects(
    fixture$cfg, "fmriprep", subject_filter = "01"
  )
  BrainGnomes:::update_run_provenance_submission(
    fixture$cfg, run_id, deferred = TRUE, resolved_subjects = resolved
  )

  after <- get_run_provenance(fixture$cfg, run_id)
  expect_identical(after$execution$scope_status, "resolved")
  expect_identical(after$execution$subjects$sub_id, "01")
  expect_true(nzchar(after$execution$scope_resolved_at))
  subjects <- utils::read.delim(after$files$subjects, stringsAsFactors = FALSE)
  expect_identical(subjects$sub_id, 1L)
})
