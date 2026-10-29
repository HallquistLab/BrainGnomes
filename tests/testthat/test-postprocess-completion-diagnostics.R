#' Create existing postprocessing inputs and an isolated completion-evidence store
#' @return Configuration, subject row, selected steps, input, and marker paths.
#' @noRd
make_completion_diagnostic_fixture <- function() {
  root <- tempfile("completion-diagnostic-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE), envir = parent.frame())
  cfg <- structure(list(metadata = list(project_directory = root,
    log_directory = file.path(root, "logs"), bids_directory = file.path(root, "bids"),
    fmriprep_directory = file.path(root, "fmriprep"), sqlite_db = file.path(root, "tracking.sqlite")),
    postprocess = list(enable = TRUE, default = list(input_regex = "desc:preproc suffix:bold",
      ncores = 1, nhours = 1, memgb = 1)),
    fmriprep = list(enable = FALSE), compute_environment = list(scheduler = "slurm"),
    force = FALSE), class = "bg_project_cfg")
  subject <- data.frame(sub_id = "01", ses_id = NA_character_,
    dicom_sub_dir = NA_character_, dicom_ses_dir = NA_character_,
    bids_sub_dir = file.path(cfg$metadata$bids_directory, "sub-01"), bids_ses_dir = NA_character_)
  output <- file.path(cfg$metadata$fmriprep_directory, "sub-01")
  logs <- file.path(cfg$metadata$log_directory, "sub-01")
  for (dir in c(subject$bids_sub_dir, output, logs)) dir.create(dir, recursive = TRUE)
  input <- file.path(output, "sub-01_task-rest_desc-preproc_bold.nii.gz")
  writeBin(raw(0), input)
  marker <- file.path(logs, ".fmriprep_sub-01_complete")
  file.create(marker)
  steps <- c(bids_conversion = FALSE, mriqc = FALSE, fmriprep = FALSE,
    aroma = FALSE, postprocess = TRUE, extract_rois = FALSE)
  list(cfg = cfg, subject = subject, steps = steps, input = input, marker = marker)
}

for (evidence in c("failed job", "invalid manifest", "failure marker", "database error", "complete")) {
  test_that(paste("existing fmriprep markers are described accurately for", evidence), {
    fixture <- make_completion_diagnostic_fixture()
    db <- fixture$cfg$metadata$sqlite_db
    if (evidence %in% c("failed job", "invalid manifest", "database error")) {
      create_tracking_db(db)
      insert_tracked_job(db, "1", list(job_name = "fmriprep_sub-01"))
    }
    if (evidence == "failed job") {
      update_tracked_job_status(db, "1", "FAILED", strict = TRUE)
      expected <- "latest fMRIPrep tracking status is FAILED"
    } else if (evidence == "invalid manifest") {
      missing <- file.path(dirname(fixture$input), "missing.nii.gz")
      writeLines("data", missing)
      manifest <- capture_output_manifest(dirname(fixture$input))
      unlink(missing)
      update_tracked_job_status(db, "1", "COMPLETED", output_manifest = manifest, strict = TRUE)
      expected <- "output manifest verification failed"
    } else if (evidence == "failure marker") {
      failed <- sub("_complete$", "_fail", fixture$marker)
      file.create(failed)
      Sys.setFileTime(fixture$marker, Sys.time() - 60)
      expected <- "failure marker is not superseded"
    } else if (evidence == "database error") {
      submit_sqlite_query("DROP TABLE job_tracking", db)
      expected <- "could not query job tracking database"
    }
    buffer <- lgr::AppenderBuffer$new()
    logger <- lgr::get_logger_glue(tempfile("completion-log-"))
    logger$set_threshold("info")
    logger$set_propagate(FALSE)
    logger$add_appender(buffer, "buffer")
    withr::defer(logger$remove_appender("buffer"))
    submissions <- 0L
    local_mocked_bindings(
      get_subject_logger = function(...) logger,
      get_job_script = function(...) "mock_script",
      get_job_sched_args = function(...) "--mock",
      # Exercise real completion/input decisions without submitting cluster jobs.
      submit_postprocess = function(...) {
        submissions <<- submissions + 1L
        "job-1"
      }
    )
    warnings <- character()
    withCallingHandlers(process_subject(fixture$cfg, fixture$subject, fixture$steps),
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      })
    expect_identical(submissions, 1L)
    expect_true(file.exists(fixture$marker))
    expect_false(any(grepl("Exiting process_subject|markers are missing|no fMRIPrep completion marker",
      buffer$data$msg)))
    if (evidence == "complete") {
      expect_length(warnings, 0L)
      expect_false(any(grepl("Proceeding with postprocessing", buffer$data$msg)))
    } else {
      expect_length(warnings, 1L)
      expect_match(warnings, expected)
      expect_true(any(grepl("Proceeding with postprocessing.*using matching NIfTI inputs", buffer$data$msg)))
    }
  })
}
