test_that("extract_rois creates timeseries and correlations", {
  skip_if_not_installed("RNifti")
  skip_if_not_installed("corpcor")
  skip_if_not_installed("data.table")
  skip_if_not_installed("lgr")
  skip_if_not_installed("checkmate")
  library(RNifti)
  tmpdir <- tempdir()
  arr <- array(rnorm(3*3*3*25), dim = c(3,3,3,25))
  bold_file <- file.path(tmpdir, "sub-01_task-test_desc-test_bold.nii.gz")
  RNifti::writeNifti(RNifti::asNifti(arr), bold_file)

  atlas_arr <- array(0, c(3,3,3))
  atlas_arr[1:2,1:2,1:2] <- 1
  atlas_arr[3,3,3] <- 2
  atlas_file <- file.path(tmpdir, "Schaefer2.nii.gz")
  RNifti::writeNifti(RNifti::asNifti(atlas_arr), atlas_file)

  res <- extract_rois(bold_file, atlas_files = atlas_file, out_dir = tmpdir,
                      cor_method = "cor.shrink", roi_reduce = "median")
  atlas_name <- names(res)[1]
  ts_file <- res[[atlas_name]]$timeseries
  corr_file <- res[[atlas_name]]$correlation[["cor.shrink"]]
  expect_true(file.exists(ts_file))
  expect_true(file.exists(corr_file))
  expect_null(res[[atlas_name]]$diagnostics)
  cmat <- as.matrix(read.delim(corr_file, check.names = FALSE))
  expect_equal(dim(cmat), c(2, 2))
  expect_true(all(is.na(cmat[2, ])) && all(is.na(cmat[, 2])))
})

test_that("extract_rois reports distinct spatial-mask and BOLD-validity losses", {
  skip_if_not_installed("RNifti")
  skip_if_not_installed("data.table")
  skip_if_not_installed("lgr")
  skip_if_not_installed("checkmate")

  tmpdir <- tempfile("extract-roi-diagnostics-")
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE, force = TRUE), add = TRUE)

  set.seed(1401)
  dims <- c(4L, 2L, 2L, 25L)
  bold <- array(rnorm(prod(dims)), dim = dims)
  bold[4, , , ] <- 0

  atlas <- array(0L, dim = dims[1:3])
  for (label in 1:4) atlas[label, , ] <- label

  spatial_mask <- array(1L, dim = dims[1:3])
  spatial_mask[2, 2, ] <- 0L
  spatial_mask[3, , ] <- 0L

  bold_file <- file.path(tmpdir, "sub-14_task-rest_desc-clean_bold.nii.gz")
  atlas_file <- file.path(tmpdir, "DiagnosticAtlas.nii.gz")
  mask_file <- file.path(tmpdir, "analysis-mask.nii.gz")
  RNifti::writeNifti(RNifti::asNifti(bold), bold_file)
  RNifti::writeNifti(RNifti::asNifti(atlas), atlas_file)
  RNifti::writeNifti(RNifti::asNifti(spatial_mask), mask_file)

  result <- extract_rois(
    bold_file = bold_file,
    atlas_files = atlas_file,
    out_dir = tmpdir,
    mask_file = mask_file,
    cor_method = "none",
    min_vox_per_roi = 3L,
    save_diagnostics = TRUE
  )[[1]]

  expect_true(file.exists(result$diagnostics))
  expect_match(basename(result$diagnostics), "_roidiagnostics\\.tsv$")
  diagnostics <- read.delim(result$diagnostics, check.names = FALSE)
  expect_named(diagnostics, c(
    "roi", "atlas_value", "n_vox_atlas", "n_vox_in_mask",
    "n_vox_usable", "min_vox_required", "proportion_in_mask",
    "proportion_usable", "proportion_usable_in_mask", "retained",
    "exclusion_reason"
  ))
  expect_identical(diagnostics$roi, paste0("roi", 1:4))
  expect_identical(diagnostics$n_vox_atlas, rep(4L, 4L))
  expect_identical(diagnostics$n_vox_in_mask, c(4L, 2L, 0L, 4L))
  expect_identical(diagnostics$n_vox_usable, c(4L, 2L, 0L, 0L))
  expect_identical(diagnostics$min_vox_required, rep(3L, 4L))
  expect_equal(diagnostics$proportion_in_mask, c(1, 0.5, 0, 1))
  expect_equal(diagnostics$proportion_usable, c(1, 0.5, 0, 0))
  expect_equal(diagnostics$proportion_usable_in_mask, c(1, 1, NA, 0))
  expect_identical(diagnostics$retained, c(TRUE, FALSE, FALSE, FALSE))
  expect_identical(
    diagnostics$exclusion_reason,
    c(NA_character_, "below_threshold", "outside_mask", "invalid_bold")
  )
})

test_that("extract_rois supports time-series-only extraction without correlations", {
  skip_if_not_installed("RNifti")
  skip_if_not_installed("data.table")
  skip_if_not_installed("lgr")
  skip_if_not_installed("checkmate")

  tmpdir <- tempfile("extract-timeseries-only-")
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE, force = TRUE), add = TRUE)

  dims <- c(2L, 2L, 2L, 12L)
  x <- seq(-1, 1, length.out = dims[[4]])
  bold <- array(0, dim = dims)
  for (volume in seq_along(x)) {
    bold[1, , , volume] <- 100 + x[[volume]]
    bold[2, , , volume] <- 100 + x[[volume]]^3
  }
  atlas <- array(0L, dim = dims[1:3])
  atlas[1, , ] <- 1L
  atlas[2, , ] <- 2L

  bold_file <- file.path(tmpdir, "sub-01_task-rest_desc-clean_bold.nii.gz")
  atlas_file <- file.path(tmpdir, "DemoAtlas.nii.gz")
  RNifti::writeNifti(RNifti::asNifti(bold), bold_file)
  RNifti::writeNifti(RNifti::asNifti(atlas), atlas_file)

  expect_no_warning(result <- extract_rois(
    bold_file = bold_file,
    atlas_files = atlas_file,
    out_dir = tmpdir,
    cor_method = "none",
    min_vox_per_roi = 1L
  )[[1]])

  expect_true(file.exists(result$timeseries))
  expect_null(result$correlation)
  expect_length(list.files(tmpdir, pattern = "_connectivity\\.tsv$", recursive = TRUE), 0L)
  series <- read.delim(result$timeseries, check.names = FALSE)
  expect_equal(dim(series), c(12L, 3L))
  expect_named(series, c("volume", "roi1", "roi2"))

  invalid_dir <- file.path(tmpdir, "invalid")
  dir.create(invalid_dir)
  expect_error(
    extract_rois(
      bold_file = bold_file,
      atlas_files = atlas_file,
      out_dir = invalid_dir,
      cor_method = c("none", "pearson"),
      min_vox_per_roi = 1L
    ),
    "'none' cannot be combined with correlation methods"
  )
  expect_error(
    extract_rois(
      bold_file = bold_file,
      atlas_files = atlas_file,
      out_dir = invalid_dir,
      cor_method = "none",
      min_vox_per_roi = 1L,
      save_ts = FALSE
    ),
    "cor_method = 'none' requires save_ts = TRUE"
  )
  expect_length(list.files(invalid_dir, recursive = TRUE), 0L)
})

test_that("extract_rois writes distinct, accurate files for every correlation method", {
  skip_if_not_installed("RNifti")
  skip_if_not_installed("corpcor")
  skip_if_not_installed("data.table")
  skip_if_not_installed("lgr")
  skip_if_not_installed("checkmate")

  tmpdir <- tempfile("extract-correlation-methods-")
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE, force = TRUE), add = TRUE)

  dims <- c(2L, 2L, 2L, 30L)
  x <- seq(-2, 2, length.out = dims[[4]])
  bold <- array(0, dim = dims)
  for (volume in seq_along(x)) {
    bold[1, , , volume] <- 100 + x[[volume]]
    bold[2, , , volume] <- 100 + x[[volume]]^3
  }

  atlas <- array(0L, dim = dims[1:3])
  atlas[1, , ] <- 1L
  atlas[2, , ] <- 2L

  bold_file <- file.path(tmpdir, "sub-01_task-rest_desc-clean_bold.nii.gz")
  atlas_file <- file.path(tmpdir, "DemoAtlas.nii.gz")
  RNifti::writeNifti(RNifti::asNifti(bold), bold_file)
  RNifti::writeNifti(RNifti::asNifti(atlas), atlas_file)

  for (overwrite in c(FALSE, TRUE)) {
    out_dir <- file.path(tmpdir, paste0("overwrite-", overwrite))
    dir.create(out_dir)

    result <- extract_rois(
      bold_file = bold_file,
      atlas_files = atlas_file,
      out_dir = out_dir,
      cor_method = c("pearson", "spearman"),
      min_vox_per_roi = 1L,
      overwrite = overwrite
    )[[1]]

    paths <- unlist(result$correlation, use.names = FALSE)
    expect_length(unique(paths), 2L)

    series <- read.delim(result$timeseries, check.names = FALSE)
    for (method in names(result$correlation)) {
      path <- result$correlation[[method]]
      expect_match(basename(path), paste0("_cor-", method, "_connectivity\\.tsv$"))
      observed <- as.matrix(read.delim(path, check.names = FALSE))
      expected <- stats::cor(series[, c("roi1", "roi2")], method = method)
      expect_equal(unname(observed), unname(expected), tolerance = 1e-7)
    }
  }

  default_dir <- file.path(tmpdir, "default-methods")
  dir.create(default_dir)
  default_result <- extract_rois(
    bold_file = bold_file,
    atlas_files = atlas_file,
    out_dir = default_dir,
    min_vox_per_roi = 1L
  )[[1]]

  default_paths <- unlist(default_result$correlation, use.names = FALSE)
  expect_named(default_result$correlation, c("pearson", "spearman", "kendall", "cor.shrink"))
  expect_length(unique(default_paths), 4L)
  expect_true(all(file.exists(default_paths)))
  expect_match(
    basename(default_result$correlation[["cor.shrink"]]),
    "_cor-corShrink_connectivity\\.tsv$"
  )
})

test_that("extract_rois rejects duplicate correlation output paths before writing", {
  skip_if_not_installed("RNifti")
  skip_if_not_installed("data.table")
  skip_if_not_installed("lgr")
  skip_if_not_installed("checkmate")

  tmpdir <- tempfile("extract-duplicate-correlations-")
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE, force = TRUE), add = TRUE)

  bold <- array(rep(seq_len(25L), each = 8L), dim = c(2L, 2L, 2L, 25L))
  atlas <- array(1L, dim = c(2L, 2L, 2L))
  bold_file <- file.path(tmpdir, "sub-01_task-rest_bold.nii.gz")
  atlas_file <- file.path(tmpdir, "DemoAtlas.nii.gz")
  out_dir <- file.path(tmpdir, "output")
  dir.create(out_dir)
  RNifti::writeNifti(RNifti::asNifti(bold), bold_file)
  RNifti::writeNifti(RNifti::asNifti(atlas), atlas_file)

  expect_error(
    suppressWarnings(extract_rois(
      bold_file = bold_file,
      atlas_files = atlas_file,
      out_dir = out_dir,
      cor_method = c("pearson", "pearson"),
      min_vox_per_roi = 1L
    )),
    "Correlation methods must produce unique output paths"
  )
  expect_length(list.files(out_dir, recursive = TRUE), 0L)
})

test_that("extract_rois respects percentage-based minimum voxel thresholds", {
  skip_if_not_installed("RNifti")
  skip_if_not_installed("lgr")
  skip_if_not_installed("checkmate")
  skip_if_not_installed("data.table")
  tmpdir <- tempdir()

  dims <- c(4, 4, 4, 25)
  arr <- array(rnorm(prod(dims)), dim = dims)

  # Introduce constant time series for two ROI2 voxels so they fail the usable-voxel check
  arr[3, 3, 3, ] <- 0
  arr[4, 4, 4, ] <- 0

  bold_file <- file.path(tmpdir, "sub-02_task-test_desc-test_bold.nii.gz")
  RNifti::writeNifti(RNifti::asNifti(arr), bold_file)

  atlas_arr <- array(0, dims[1:3])
  atlas_arr[1:2, 1:2, 1:2] <- 1  # ROI1 (8 voxels)
  atlas_arr[3:4, 3:4, 3:4] <- 2  # ROI2 (8 voxels)
  atlas_file <- file.path(tmpdir, "SchaeferPct.nii.gz")
  RNifti::writeNifti(RNifti::asNifti(atlas_arr), atlas_file)

  # Mask excludes three voxels from ROI1, leaving five voxels available
  mask_arr <- array(0L, dims[1:3])
  mask_arr[atlas_arr > 0] <- 1L
  mask_arr[1, 1, 2] <- 0L
  mask_arr[2, 1, 2] <- 0L
  mask_arr[1, 2, 2] <- 0L
  mask_file <- file.path(tmpdir, "brain_mask_pct.nii.gz")
  RNifti::writeNifti(RNifti::asNifti(mask_arr), mask_file)

  out_dir <- file.path(tmpdir, "pct-threshold")
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  expect_warning(
    res_pct <- extract_rois(
      bold_file = bold_file,
      atlas_files = atlas_file,
      out_dir = out_dir,
      mask_file = mask_file,
      min_vox_per_roi = "80%",
      cor_method = "pearson",
      roi_reduce = "mean",
      overwrite = TRUE,
      save_ts = TRUE
    ),
    regexp = "writing an all-NA connectivity matrix"
  )

  atlas_name <- names(res_pct)[1]
  ts_file <- res_pct[[atlas_name]]$timeseries
  ts_pct <- read.delim(ts_file, check.names = FALSE)
  expect_true(all(is.na(ts_pct$roi1)))
  expect_true(all(is.na(ts_pct$roi2)))
  cor_pct <- read.delim(
    res_pct[[atlas_name]]$correlation[["pearson"]],
    check.names = FALSE
  )
  expect_equal(dim(cor_pct), c(2L, 2L))
  expect_named(cor_pct, c("roi1", "roi2"))
  expect_true(all(is.na(cor_pct)))

  res_frac <- extract_rois(
    bold_file = bold_file,
    atlas_files = atlas_file,
    out_dir = out_dir,
    mask_file = mask_file,
    min_vox_per_roi = 0.5,
    cor_method = "pearson",
    roi_reduce = "mean",
    overwrite = TRUE,
    save_ts = TRUE
  )

  ts_file_frac <- res_frac[[atlas_name]]$timeseries
  ts_frac <- read.delim(ts_file_frac, check.names = FALSE)
  expect_true(any(!is.na(ts_frac$roi1)))
  expect_true(any(!is.na(ts_frac$roi2)))
})

test_that("extract_rois preserves atlas labels when brain masks exclude ROIs", {
  skip_if_not_installed("RNifti")
  skip_if_not_installed("lgr")
  skip_if_not_installed("checkmate")
  skip_if_not_installed("data.table")

  tmpdir <- tempfile("extract-mask-")
  dir.create(tmpdir, recursive = TRUE, showWarnings = FALSE)

  dims <- c(3, 3, 3, 20)
  arr <- array(rnorm(prod(dims)), dim = dims)
  bold_file <- file.path(tmpdir, "sub-03_task-test_desc-test_bold.nii.gz")
  RNifti::writeNifti(RNifti::asNifti(arr), bold_file)

  atlas_arr <- array(0L, dims[1:3])
  atlas_arr[2, 2, 2] <- 1L
  atlas_arr[3, 3, 3] <- 2L
  atlas_file <- file.path(tmpdir, "SchaeferMaskPrune.nii.gz")
  RNifti::writeNifti(RNifti::asNifti(atlas_arr), atlas_file)

  mask_arr <- array(0L, dims[1:3])
  mask_arr[2, 2, 2] <- 1L
  mask_file <- file.path(tmpdir, "brain_mask_pruned.nii.gz")
  RNifti::writeNifti(RNifti::asNifti(mask_arr), mask_file)

  out_dir <- file.path(tmpdir, "mask-pruned-output")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  res <- extract_rois(
    bold_file = bold_file,
    atlas_files = atlas_file,
    out_dir = out_dir,
    mask_file = mask_file,
    cor_method = "pearson",
    roi_reduce = "mean",
    min_vox_per_roi = 1,
    overwrite = TRUE,
    save_ts = TRUE
  )

  atlas_name <- names(res)[1]
  ts_file <- res[[atlas_name]]$timeseries
  cor_file <- res[[atlas_name]]$correlation[["pearson"]]

  expect_true(file.exists(ts_file))
  expect_true(file.exists(cor_file))

  ts_df <- read.delim(ts_file, check.names = FALSE)
  expect_named(ts_df, c("volume", "roi1", "roi2"))
  expect_true(all(is.na(ts_df$roi2)))

  cor_mat <- as.matrix(read.delim(cor_file, check.names = FALSE))
  expect_equal(dim(cor_mat), c(2L, 2L))
  expect_identical(colnames(cor_mat), c("roi1", "roi2"))
  expect_equal(unname(cor_mat[1, 1]), 1)
  expect_true(all(is.na(cor_mat[2, ])))
  expect_true(all(is.na(cor_mat[, 2])))
})

test_that("extract_rois keeps atlas dimensions across different and empty masks", {
  skip_if_not_installed("RNifti")
  skip_if_not_installed("lgr")
  skip_if_not_installed("checkmate")
  skip_if_not_installed("data.table")

  tmpdir <- tempfile("extract-mask-dimensions-")
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE, force = TRUE), add = TRUE)

  dims <- c(3L, 2L, 2L, 30L)
  x <- seq(-2, 2, length.out = dims[[4]])
  bold <- array(0, dim = dims)
  for (volume in seq_along(x)) {
    bold[1, , , volume] <- 100 + x[[volume]]
    bold[2, , , volume] <- 100 + x[[volume]]^2
    bold[3, , , volume] <- 100 + x[[volume]]^3
  }

  atlas <- array(0L, dim = dims[1:3])
  atlas[1, , ] <- 1L
  atlas[2, , ] <- 3L
  atlas[3, , ] <- 7L
  atlas_file <- file.path(tmpdir, "StableAtlas.nii.gz")
  RNifti::writeNifti(RNifti::asNifti(atlas), atlas_file)

  masks <- list(
    first = as.integer(atlas %in% c(1L, 7L)),
    second = as.integer(atlas == 3L),
    empty = integer(prod(dims[1:3]))
  )
  mask_files <- vapply(names(masks), function(label) {
    path <- file.path(tmpdir, paste0(label, "-mask.nii.gz"))
    RNifti::writeNifti(
      RNifti::asNifti(array(masks[[label]], dim = dims[1:3])),
      path
    )
    path
  }, FUN.VALUE = character(1))

  extract_subject <- function(subject, mask, minimum = 1L, method = "pearson") {
    bold_file <- file.path(
      tmpdir,
      paste0("sub-", subject, "_task-rest_desc-clean_bold.nii.gz")
    )
    RNifti::writeNifti(RNifti::asNifti(bold), bold_file)
    extract_rois(
      bold_file = bold_file,
      atlas_files = atlas_file,
      out_dir = tmpdir,
      mask_file = mask_files[[mask]],
      cor_method = method,
      min_vox_per_roi = minimum
    )[[1]]
  }

  first <- extract_subject("01", "first", minimum = "80%")
  second <- extract_subject("02", "second")
  expect_warning(
    empty <- extract_subject("03", "empty"),
    regexp = "writing an all-NA connectivity matrix"
  )
  expect_no_warning(timeseries_only <- extract_subject("04", "empty", method = "none"))

  expected_names <- c("volume", "roi1", "roi3", "roi7")
  expected_roi_names <- expected_names[-1]
  for (result in list(first, second, empty, timeseries_only)) {
    series <- read.delim(result$timeseries, check.names = FALSE)
    expect_named(series, expected_names)
    expect_equal(dim(series), c(30L, 4L))
  }

  first_series <- read.delim(first$timeseries, check.names = FALSE)
  second_series <- read.delim(second$timeseries, check.names = FALSE)
  empty_series <- read.delim(empty$timeseries, check.names = FALSE)
  expect_true(all(is.na(first_series$roi3)))
  expect_true(all(is.na(second_series$roi1)))
  expect_true(all(is.na(second_series$roi7)))
  expect_true(all(is.na(empty_series[, expected_roi_names])))
  expect_null(timeseries_only$correlation)

  for (result in list(first, second, empty)) {
    matrix <- as.matrix(read.delim(
      result$correlation[["pearson"]],
      check.names = FALSE
    ))
    expect_equal(dim(matrix), c(3L, 3L))
    expect_identical(colnames(matrix), expected_roi_names)
  }

  first_matrix <- as.matrix(read.delim(first$correlation[["pearson"]]))
  second_matrix <- as.matrix(read.delim(second$correlation[["pearson"]]))
  empty_matrix <- as.matrix(read.delim(empty$correlation[["pearson"]]))
  expect_true(all(is.na(first_matrix[2, ])))
  expect_true(all(is.na(first_matrix[, 2])))
  expect_true(all(is.na(second_matrix[c(1, 3), ])))
  expect_true(all(is.na(second_matrix[, c(1, 3)])))
  expect_true(all(is.na(empty_matrix)))

  single_atlas <- array(0L, dim = dims[1:3])
  single_atlas[1, , ] <- 7L
  single_atlas_file <- file.path(tmpdir, "SingleAtlas.nii.gz")
  RNifti::writeNifti(RNifti::asNifti(single_atlas), single_atlas_file)
  single_bold <- file.path(tmpdir, "sub-05_task-rest_desc-clean_bold.nii.gz")
  RNifti::writeNifti(RNifti::asNifti(bold), single_bold)
  expect_warning(
    single <- extract_rois(
      bold_file = single_bold,
      atlas_files = single_atlas_file,
      out_dir = tmpdir,
      mask_file = mask_files[["empty"]],
      cor_method = "pearson",
      min_vox_per_roi = 1L
    )[[1]],
    regexp = "writing an all-NA connectivity matrix"
  )
  single_matrix <- read.delim(single$correlation[["pearson"]], check.names = FALSE)
  expect_equal(dim(single_matrix), c(1L, 1L))
  expect_named(single_matrix, "roi7")
  expect_true(is.na(single_matrix[[1]][[1]]))

  zero_atlas_file <- file.path(tmpdir, "ZeroAtlas.nii.gz")
  RNifti::writeNifti(
    RNifti::asNifti(array(0L, dim = dims[1:3])),
    zero_atlas_file
  )
  expect_error(
    suppressWarnings(extract_rois(
      bold_file = single_bold,
      atlas_files = zero_atlas_file,
      out_dir = tmpdir,
      cor_method = "pearson",
      min_vox_per_roi = 1L
    )),
    "contains no positive ROI labels"
  )
})

test_that("extract_rois does not apply censoring twice to already scrubbed BOLD", {
  skip_if_not_installed("RNifti")
  skip_if_not_installed("data.table")
  skip_if_not_installed("lgr")
  skip_if_not_installed("checkmate")

  tmpdir <- tempfile("extract-already-scrubbed-")
  dir.create(tmpdir, recursive = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE, force = TRUE), add = TRUE)

  original_n <- 30L
  censor <- rep(1L, original_n)
  censor[c(4L, 12L, 21L)] <- 0L
  retained_n <- sum(censor)
  bold <- array(
    rep(seq_len(retained_n), each = 8L),
    dim = c(2L, 2L, 2L, retained_n)
  )
  atlas <- array(1L, dim = c(2L, 2L, 2L))
  bold_file <- file.path(tmpdir, "sub-01_task-rest_desc-scrubbed_bold.nii.gz")
  atlas_file <- file.path(tmpdir, "DemoAtlas.nii.gz")
  out_dir <- file.path(tmpdir, "rois")
  dir.create(out_dir)
  RNifti::writeNifti(RNifti::asNifti(bold), bold_file)
  RNifti::writeNifti(RNifti::asNifti(atlas), atlas_file)
  writeLines(
    as.character(censor),
    get_censor_file(as.list(extract_bids_info(bold_file)))
  )

  result <- extract_rois(
    bold_file, atlas_file, out_dir,
    cor_method = "pearson", min_vox_per_roi = 1L
  )[[1L]]
  timeseries <- data.table::fread(result$timeseries)

  expect_equal(nrow(timeseries), retained_n)
  expect_equal(timeseries$volume, seq_len(retained_n))
  expect_true(file.exists(result$correlation[["pearson"]]))
})

test_that("extract_rois rejects a censor vector incompatible with BOLD length", {
  skip_if_not_installed("RNifti")
  skip_if_not_installed("data.table")
  skip_if_not_installed("lgr")
  skip_if_not_installed("checkmate")

  tmpdir <- tempfile("extract-bad-censor-")
  dir.create(tmpdir, recursive = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE, force = TRUE), add = TRUE)

  bold <- array(rnorm(2L * 2L * 2L * 25L), dim = c(2L, 2L, 2L, 25L))
  atlas <- array(1L, dim = c(2L, 2L, 2L))
  bold_file <- file.path(tmpdir, "sub-01_task-rest_desc-clean_bold.nii.gz")
  atlas_file <- file.path(tmpdir, "DemoAtlas.nii.gz")
  out_dir <- file.path(tmpdir, "rois")
  dir.create(out_dir)
  RNifti::writeNifti(RNifti::asNifti(bold), bold_file)
  RNifti::writeNifti(RNifti::asNifti(atlas), atlas_file)
  writeLines(
    as.character(c(rep(1L, 20L), rep(0L, 10L))),
    get_censor_file(as.list(extract_bids_info(bold_file)))
  )

  expect_warning(
    expect_error(
      extract_rois(
        bold_file, atlas_file, out_dir,
        cor_method = "none", min_vox_per_roi = 1L
      ),
      "incompatible with the BOLD time dimension"
    ),
    "Error running extract_rois"
  )
})
