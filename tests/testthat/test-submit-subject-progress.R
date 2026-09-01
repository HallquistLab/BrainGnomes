make_resolved_subjects <- function(n) {
  data.frame(
    sub_id = sprintf("%04d", seq_len(n)),
    ses_id = NA_character_,
    dicom_sub_dir = NA_character_,
    dicom_ses_dir = NA_character_,
    bids_sub_dir = tempfile("bids-subject-", tmpdir = tempdir()),
    bids_ses_dir = NA_character_,
    stringsAsFactors = FALSE
  )
}

test_that("large subject submissions report bounded progress", {
  seen <- character()
  local_mocked_bindings(
    process_subject = function(scfg, sub_cfg, ...) {
      seen <<- c(seen, sub_cfg$sub_id[[1L]])
      TRUE
    },
    .package = "BrainGnomes"
  )
  subjects <- make_resolved_subjects(100L)
  steps <- stats::setNames(
    rep(FALSE, 6L),
    c("bids_conversion", "mriqc", "fmriprep", "aroma", "postprocess", "extract_rois")
  )

  messages <- capture_messages(BrainGnomes:::submit_subjects(
    scfg = structure(list(), class = "bg_project_cfg"),
    steps = steps,
    resolved_subjects = subjects
  ))

  progress <- grep("Preparing jobs for subject", messages, value = TRUE)
  expect_identical(length(seen), 100L)
  expect_lte(length(progress), 21L)
  expect_true(any(grepl("subject 1 of 100", progress)))
  expect_true(any(grepl("subject 100 of 100", progress)))
  expect_true(any(grepl("Finished checking and submitting jobs", messages)))
})

test_that("large explicit subject lists get a bounded preview", {
  local_mocked_bindings(
    process_subject = function(...) TRUE,
    .package = "BrainGnomes"
  )
  subjects <- make_resolved_subjects(100L)
  steps <- stats::setNames(
    rep(FALSE, 6L),
    c("bids_conversion", "mriqc", "fmriprep", "aroma", "postprocess", "extract_rois")
  )

  output <- capture_output(suppressMessages(BrainGnomes:::submit_subjects(
    scfg = structure(list(), class = "bg_project_cfg"),
    steps = steps,
    subject_filter = subjects$sub_id,
    resolved_subjects = subjects
  )))

  expect_match(output, "and 80 more matching subject/session entries")
  expect_match(output, "sub-0001")
  expect_false(grepl("sub-0100", output, fixed = TRUE))
})
