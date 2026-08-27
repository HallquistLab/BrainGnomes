test_that("get_postproc_output_files targets postprocessed outputs", {
  tmp_dir <- tempfile("bg_postproc_")
  dir.create(tmp_dir, recursive = TRUE)

  postproc <- file.path(
    tmp_dir,
    "sub-01_task-impressions_space-MNI152NLin2009cAsym_desc-postproc_clean_bold.nii.gz"
  )
  preproc <- file.path(
    tmp_dir,
    "sub-01_task-impressions_space-MNI152NLin2009cAsym_desc-preproc_bold.nii.gz"
  )
  file.create(postproc)
  file.create(preproc)

  rx_with_desc <- "desc:preproc task:impressions space:MNI152NLin2009cAsym suffix:bold"
  res_with_desc <- get_postproc_output_files(tmp_dir, rx_with_desc, "postproc_clean")
  expect_true(postproc %in% res_with_desc)
  expect_false(preproc %in% res_with_desc)

  rx_no_desc <- "task:impressions space:MNI152NLin2009cAsym suffix:bold"
  res_no_desc <- get_postproc_output_files(tmp_dir, rx_no_desc, "postproc_clean")
  expect_true(postproc %in% res_no_desc)
  expect_false(preproc %in% res_no_desc)
})

test_that("get_postproc_output_files pairs input specifications with descriptions", {
  tmp_dir <- tempfile("bg_postproc_pairs_")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)

  expected <- file.path(tmp_dir, c(
    "sub-01_task-rest_desc-clean_bold.nii.gz",
    "sub-01_task-nback_desc-denoised_bold.nii.gz"
  ))
  decoys <- file.path(tmp_dir, c(
    "sub-01_task-rest_desc-denoised_bold.nii.gz",
    "sub-01_task-nback_desc-clean_bold.nii.gz"
  ))
  expect_true(all(file.create(c(expected, decoys))))

  specs <- c(
    "task:rest desc:preproc suffix:bold",
    "task:nback desc:preproc suffix:bold"
  )
  matched <- get_postproc_output_files(
    tmp_dir,
    input_regex = specs,
    bids_desc = c("clean", "denoised")
  )

  expect_setequal(matched, expected)
  expect_false(any(decoys %in% matched))

  scalar_desc <- get_postproc_output_files(
    tmp_dir,
    input_regex = specs,
    bids_desc = "clean"
  )
  expect_setequal(scalar_desc, c(expected[[1L]], decoys[[2L]]))
})

test_that("get_postproc_output_files rejects ambiguous vector lengths", {
  tmp_dir <- tempfile("bg_postproc_pair_lengths_")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)

  expect_error(
    get_postproc_output_files(
      tmp_dir,
      input_regex = c("task:rest suffix:bold", "task:nback suffix:bold"),
      bids_desc = c("clean", "denoised", "other")
    ),
    "bids_desc must have length 1 or the same length as input_regex"
  )
})

test_that("get_postproc_output_files retargets raw preproc regexes to stream outputs", {
  tmp_dir <- tempfile("bg_postproc_raw_regex_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)

  wanted <- file.path(
    tmp_dir,
    "sub-540294_task-ridl_run-01_space-MNI152NLin2009cAsym_desc-taskPost_bold.nii.gz"
  )
  wrong_desc <- sub("desc-taskPost", "desc-other", wanted, fixed = TRUE)
  wrong_task <- sub("task-ridl", "task-rest", wanted, fixed = TRUE)
  expect_true(all(file.create(c(wanted, wrong_desc, wrong_task))))

  input_regex <- paste0(
    "regex: .*task-ridl.*space-MNI152NLin2009cAsym.*",
    "_desc-preproc_bold.nii.gz$"
  )
  expect_identical(
    get_postproc_output_files(tmp_dir, input_regex, "taskPost"),
    wanted
  )
})

test_that("get_postproc_output_files constrains desc for raw regexes without one", {
  tmp_dir <- tempfile("bg_postproc_raw_regex_no_desc_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)

  wanted <- file.path(tmp_dir, "sub-01_task-ridl_desc-clean_bold.nii.gz")
  decoy <- file.path(tmp_dir, "sub-01_task-ridl_desc-other_bold.nii.gz")
  expect_true(all(file.create(c(wanted, decoy))))

  expect_identical(
    get_postproc_output_files(
      tmp_dir,
      "regex: .*task-ridl.*_bold.nii.gz$",
      "clean"
    ),
    wanted
  )
})
