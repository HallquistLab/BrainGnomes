test_that("censor parsing validates binary values before converting to integers", {
  file <- withr::local_tempfile()
  writeLines(c("1", "0.0", "1e0", " 0 "), file)
  expect_identical(read_censor_file(file), c(1L, 0L, 1L, 0L))
  record <- derivative_provenance_censor(file, 4L)
  expect_equal(unlist(record$RetainedIndices), c(1L, 3L))
  expect_equal(unlist(record$CensoredIndices), c(2L, 4L))

  for (invalid in c("0.9", "1.1", "-0.1", "2", "NA", "NaN", "Inf", "text", "")) {
    writeLines(c("1", invalid, "0"), file)
    expect_error(read_censor_file(file), "only 0 and 1")
    expect_error(derivative_provenance_censor(file), "only 0 and 1")
  }
  writeLines(character(), file)
  expect_error(read_censor_file(file), "nonempty")
})

test_that("fractional censors cannot alter ROI time series or scrubbing outputs", {
  root <- withr::local_tempdir()
  bold <- file.path(root, "sub-01_task-rest_desc-clean_bold.nii.gz")
  atlas <- file.path(root, "atlas.nii.gz")
  censor <- get_censor_file(as.list(extract_bids_info(bold)))
  RNifti::writeNifti(array(rep(seq_len(25L), each = 8L), c(2L, 2L, 2L, 25L)), bold)
  RNifti::writeNifti(array(1L, c(2L, 2L, 2L)), atlas)
  writeLines(c("0.9", rep("1", 24L)), censor)
  out_dir <- file.path(root, "rois")
  dir.create(out_dir)
  expect_warning(
    expect_error(extract_rois(bold, atlas, out_dir, cor_method = "none",
      min_vox_per_roi = 1L), "only 0 and 1"),
    "Error running extract_rois"
  )
  expect_length(list.files(out_dir, pattern = "[.]tsv$", recursive = TRUE), 0L)
  for (scrub in list(scrub_timepoints, scrub_interpolate)) {
    output <- file.path(root, "scrubbed.nii.gz")
    expect_error(scrub(bold, censor, out_file = output), "only 0 and 1")
    expect_false(file.exists(output))
  }

  # Valid numeric binary notation still removes exactly the intended volume.
  writeLines(c("0.0", rep("1.0", 24L)), censor)
  result <- extract_rois(bold, atlas, out_dir, cor_method = "none",
    min_vox_per_roi = 1L)[[1L]]
  timeseries <- read.delim(result$timeseries)
  expect_equal(timeseries$volume, 2:25)
  expect_equal(timeseries$roi1, 2:25)
})
