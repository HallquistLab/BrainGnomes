test_that("validation evaluation distinguishes pass, skip, failure, and error", {
  make_result <- function(value, message, details = list()) {
    attr(value, "message") <- message
    attr(value, "details") <- details
    value
  }

  passed <- evaluate_postproc_validation(
    "apply_mask", function() make_result(TRUE, "exact replay passed"),
    "pre.nii.gz", "post.nii.gz"
  )
  skipped <- evaluate_postproc_validation(
    "apply_aroma",
    function() make_result(TRUE, "no noise ICs", list(skipped = TRUE)),
    "pre.nii.gz", "post.nii.gz"
  )
  failed <- evaluate_postproc_validation(
    "scrub_timepoints", function() make_result(FALSE, "wrong order"),
    "pre.nii.gz", "post.nii.gz", "reused_destination"
  )
  errored <- evaluate_postproc_validation(
    "temporal_filter",
    function() {
      warning("calibration warning")
      stop("validator exploded")
    },
    "pre.nii.gz", "post.nii.gz"
  )

  expect_identical(passed$status, "passed")
  expect_true(passed$passed)
  expect_identical(skipped$status, "skipped")
  expect_true(skipped$passed)
  expect_identical(failed$status, "failed")
  expect_false(failed$passed)
  expect_identical(failed$output_source, "reused_destination")
  expect_identical(errored$status, "error")
  expect_false(errored$passed)
  expect_match(errored$message, "validator exploded")
  expect_identical(errored$warnings, "calibration warning")
  expect_gte(errored$elapsed_seconds, 0)
})

test_that("validation summary writer preserves nested diagnostic details", {
  summary_file <- tempfile(fileext = ".json")
  on.exit(unlink(summary_file), add = TRUE)
  summary <- list(
    schema_version = "postproc-validation-v1",
    pipeline_completed = FALSE,
    checks = list(list(
      step = "confound_regression", status = "failed",
      details = list(
        sampled_indices = c(2L, 10L),
        normalized_coords = matrix(c(0.1, 0.2, 0.3), nrow = 1L)
      )
    ))
  )

  expect_true(write_postproc_validation_summary(summary_file, summary))
  observed <- jsonlite::fromJSON(summary_file, simplifyVector = FALSE)
  expect_identical(observed$schema_version, "postproc-validation-v1")
  expect_false(observed$pipeline_completed)
  expect_identical(observed$checks[[1L]]$status, "failed")
  expect_length(observed$checks[[1L]]$details$sampled_indices, 2L)
})

.make_validation_orchestration_fixture <- function(stop_on_failure = FALSE,
                                                    overwrite = TRUE,
                                                    two_steps = FALSE) {
  root <- norm_path(tempfile("postproc-validation-"), mustWork = FALSE)
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  log_dir <- file.path(root, "logs", "sub-TEST")
  bold_dir <- file.path(root, "fmriprep", "sub-TEST", "func")
  output_dir <- file.path(root, "postproc", "sub-TEST", "func")
  scratch_dir <- file.path(root, "scratch")
  for (path in c(log_dir, bold_dir, output_dir, scratch_dir)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  bold_file <- file.path(
    bold_dir,
    "sub-TEST_task-rest_space-MNI152NLin6Asym_desc-preproc_bold.nii.gz"
  )
  connection <- gzfile(bold_file, "wb")
  writeChar("fake-bold", connection, eos = NULL)
  close(connection)
  mask_file <- file.path(root, "mask.nii.gz")
  writeLines("fake-mask", mask_file)

  cfg <- list(
    bids_desc = "postproc", keep_intermediates = FALSE,
    overwrite = overwrite, tr = 0.8, output_dir = output_dir,
    scratch_directory = scratch_dir, project_name = "validation_test",
    fsl_img = NULL, force_processing_order = FALSE,
    validate_postproc_steps = TRUE,
    stop_on_failed_validation = stop_on_failure,
    apply_mask = list(enable = TRUE, prefix = "m", mask_file = mask_file),
    spatial_smooth = list(enable = FALSE, prefix = "s", fwhm_mm = 4),
    apply_aroma = list(enable = FALSE, prefix = "a", nonaggressive = TRUE),
    temporal_filter = list(
      enable = FALSE, prefix = "f", method = "fslmaths",
      low_pass_hz = 0.1, high_pass_hz = 0.01
    ),
    intensity_normalize = list(
      enable = two_steps, prefix = "n", mode = "run_scalar",
      target = 10000, global_median = 10000
    ),
    confound_regression = list(enable = FALSE, prefix = "r"),
    confound_calculate = list(
      enable = FALSE, columns = NULL, noproc_columns = NULL, demean = FALSE
    ),
    scrubbing = list(
      enable = FALSE, expression = NULL, interpolate = FALSE,
      interpolate_prefix = "i", apply = FALSE, prefix = "x",
      add_to_confounds = FALSE
    ),
    motion_filter = list(enable = FALSE)
  )
  list(
    root = root, log_dir = log_dir, bold_file = bold_file,
    mask_file = mask_file, output_dir = output_dir,
    scratch_dir = scratch_dir, cfg = cfg
  )
}

.validation_copy_step <- function(in_file, out_file) {
  dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
  file.copy(in_file, out_file, overwrite = TRUE)
  out_file
}

test_that("validator errors obey continue policy and produce a durable audit", {
  fixture <- .make_validation_orchestration_fixture(stop_on_failure = FALSE)
  on.exit(unlink(fixture$root, recursive = TRUE, force = TRUE), add = TRUE)
  old_log <- Sys.getenv("log_file")
  on.exit(Sys.setenv(log_file = old_log), add = TRUE)
  Sys.setenv(log_file = file.path(fixture$log_dir, "postprocess.log"))

  suppressWarnings(
    with_mocked_bindings({
      final_file <- postprocess_subject(fixture$bold_file, fixture$cfg)
    }, automask = function(in_file, outfile, ...) {
      .validation_copy_step(in_file, outfile)
    }, apply_mask = function(in_file, out_file, ...) {
      .validation_copy_step(in_file, out_file)
    }, postprocess_confounds = function(...) NULL,
    validate_apply_mask = function(...) stop("synthetic validator error"))
  )

  expect_true(file.exists(final_file))
  summary_files <- list.files(
    fixture$log_dir, pattern = "postproc-validation\\.json$", full.names = TRUE
  )
  expect_length(summary_files, 1L)
  summary <- jsonlite::fromJSON(summary_files, simplifyVector = FALSE)
  expect_true(summary$pipeline_completed)
  expect_identical(summary$overall_status, "error")
  expect_identical(summary$checks[[1L]]$status, "error")
  expect_identical(summary$checks[[1L]]$output_source, "computed")
  expect_match(summary$checks[[1L]]$message, "synthetic validator error")
})

test_that("stop policy withholds a final output after validator errors", {
  fixture <- .make_validation_orchestration_fixture(stop_on_failure = TRUE)
  on.exit(unlink(fixture$root, recursive = TRUE, force = TRUE), add = TRUE)
  old_log <- Sys.getenv("log_file")
  on.exit(Sys.setenv(log_file = old_log), add = TRUE)
  Sys.setenv(log_file = file.path(fixture$log_dir, "postprocess.log"))

  expect_error(
    suppressWarnings(
      with_mocked_bindings({
        postprocess_subject(fixture$bold_file, fixture$cfg)
      }, automask = function(in_file, outfile, ...) {
        .validation_copy_step(in_file, outfile)
      }, apply_mask = function(in_file, out_file, ...) {
        .validation_copy_step(in_file, out_file)
      }, postprocess_confounds = function(...) NULL,
      validate_apply_mask = function(...) stop("synthetic validator error"))
    ),
    "stop_on_failed_validation is TRUE"
  )

  final_file <- file.path(
    fixture$output_dir,
    "sub-TEST_task-rest_space-MNI152NLin6Asym_desc-postproc_bold.nii.gz"
  )
  expect_false(file.exists(final_file))
  summary_file <- list.files(
    fixture$log_dir, pattern = "postproc-validation\\.json$", full.names = TRUE
  )
  expect_length(summary_file, 1L)
  summary <- jsonlite::fromJSON(summary_file, simplifyVector = FALSE)
  expect_false(summary$pipeline_completed)
  expect_identical(summary$overall_status, "error")
})

test_that("reused intermediate outputs are validated and identified", {
  fixture <- .make_validation_orchestration_fixture(
    stop_on_failure = FALSE, overwrite = FALSE, two_steps = TRUE
  )
  on.exit(unlink(fixture$root, recursive = TRUE, force = TRUE), add = TRUE)
  old_log <- Sys.getenv("log_file")
  on.exit(Sys.setenv(log_file = old_log), add = TRUE)
  Sys.setenv(log_file = file.path(fixture$log_dir, "postprocess.log"))

  reused_mask_file <- file.path(
    fixture$output_dir,
    "sub-TEST_task-rest_space-MNI152NLin6Asym_desc-mPostproc_bold.nii.gz"
  )
  .validation_copy_step(fixture$bold_file, reused_mask_file)
  calls <- new.env(parent = emptyenv())
  calls$apply_mask <- 0L
  calls$validate_mask <- 0L

  with_mocked_bindings({
    final_file <- postprocess_subject(fixture$bold_file, fixture$cfg)
  }, automask = function(in_file, outfile, ...) {
    .validation_copy_step(in_file, outfile)
  }, apply_mask = function(...) {
    calls$apply_mask <- calls$apply_mask + 1L
    stop("apply_mask should not run when its output is reused")
  }, validate_apply_mask = function(...) {
    calls$validate_mask <- calls$validate_mask + 1L
    value <- TRUE
    attr(value, "message") <- "reused mask is valid"
    value
  }, prepare_intensity_reference = function(
      in_file, core_file, sidecar_file, target, scale_file, ...) {
    .validation_copy_step(in_file, core_file)
    writeLines("{}", sidecar_file)
    list(
      reference_location = target / 2, target = target,
      scale_factor = 2, scale_file = scale_file, core_file = core_file,
      include_frames = NULL
    )
  }, intensity_normalize = function(in_file, out_file, ...) {
    .validation_copy_step(in_file, out_file)
  }, validate_intensity_normalize = function(...) {
    value <- TRUE
    attr(value, "message") <- "normalization is valid"
    value
  }, postprocess_confounds = function(...) NULL)

  expect_true(file.exists(final_file))
  expect_identical(calls$apply_mask, 0L)
  expect_identical(calls$validate_mask, 1L)
  summary_file <- list.files(
    fixture$log_dir, pattern = "postproc-validation\\.json$", full.names = TRUE
  )
  summary <- jsonlite::fromJSON(summary_file, simplifyVector = FALSE)
  expect_identical(summary$overall_status, "passed")
  expect_length(summary$checks, 2L)
  expect_identical(
    summary$checks[[1L]]$output_source, "reused_destination"
  )
  expect_identical(summary$checks[[2L]]$output_source, "computed")
})

test_that("an existing final output preserves its prior validation audit", {
  fixture <- .make_validation_orchestration_fixture(
    stop_on_failure = FALSE, overwrite = FALSE
  )
  on.exit(unlink(fixture$root, recursive = TRUE, force = TRUE), add = TRUE)
  old_log <- Sys.getenv("log_file")
  on.exit(Sys.setenv(log_file = old_log), add = TRUE)
  Sys.setenv(log_file = file.path(fixture$log_dir, "postprocess.log"))

  final_file <- file.path(
    fixture$output_dir,
    "sub-TEST_task-rest_space-MNI152NLin6Asym_desc-postproc_bold.nii.gz"
  )
  .validation_copy_step(fixture$bold_file, final_file)
  summary_file <- file.path(
    fixture$log_dir,
    paste0(basename(final_file), "_postproc-validation.json")
  )
  # Match postprocess_subject()'s replacement of the NIfTI extension.
  summary_file <- sub(
    "\\.nii(?:\\.gz)?_postproc-validation", "_postproc-validation",
    summary_file, perl = TRUE
  )
  prior <- list(
    schema_version = "postproc-validation-v1",
    overall_status = "failed", prior_marker = "retain-me"
  )
  expect_true(write_postproc_validation_summary(summary_file, prior))

  observed_file <- suppressWarnings(
    postprocess_subject(fixture$bold_file, fixture$cfg)
  )
  expect_identical(norm_path(observed_file), norm_path(final_file))
  observed_summary <- jsonlite::fromJSON(
    summary_file, simplifyVector = FALSE
  )
  expect_identical(observed_summary$overall_status, "failed")
  expect_identical(observed_summary$prior_marker, "retain-me")
})
