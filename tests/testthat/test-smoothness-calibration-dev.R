test_that("resolution-aware quadrature calibration recovers known coefficients", {
  helper_path <- testthat::test_path(
    "..", "..", "inst", "dev", "smoothness_calibration", "calibration_helpers.R"
  )
  calibration_env <- new.env(parent = globalenv())
  sys.source(helper_path, envir = calibration_env)

  design <- expand.grid(
    pre_fwhm_mm = c(2, 3, 4),
    kernel_fwhm_mm = c(3, 5, 8),
    voxel_geom_mm = c(2.4, 2.7, 3.1)
  )
  truth <- c(1.15, -0.35)
  design$post_fwhm_mm <- calibration_env$predict_post_ratio_linear(
    design$pre_fwhm_mm, design$kernel_fwhm_mm, design$voxel_geom_mm, truth
  )

  fitted <- calibration_env$fit_ratio_linear(design)
  expect_equal(fitted, truth, tolerance = 1e-3)
})

test_that("smoothness calibration scoring reports prediction error", {
  helper_path <- testthat::test_path(
    "..", "..", "inst", "dev", "smoothness_calibration", "calibration_helpers.R"
  )
  calibration_env <- new.env(parent = globalenv())
  sys.source(helper_path, envir = calibration_env)

  score <- calibration_env$score_predictions(c(1, 2, 3), c(1, 2.5, 2.5))
  expect_equal(score$n, 3)
  expect_equal(score$bias_mm, 0)
  expect_equal(score$mae_mm, 1 / 3)
  expect_gt(score$rmse_mm, 0)
})

test_that("postprocessing automask helper preserves the production settings", {
  captured <- NULL
  testthat::local_mocked_bindings(
    automask = function(img, outfile, ...) {
      captured <<- c(list(img = img, outfile = outfile), list(...))
      invisible(outfile)
    },
    .package = "BrainGnomes"
  )

  expect_equal(.postprocess_automask("input.nii.gz", "mask.nii.gz"), "mask.nii.gz")
  expect_equal(captured$img, "input.nii.gz")
  expect_equal(captured$outfile, "mask.nii.gz")
  expect_equal(captured$clfrac, 0.5)
  expect_equal(captured$NN, 1L)
  expect_equal(captured$SIhh, 0)
  expect_equal(captured$peels, 1L)
  expect_true(captured$fill_holes)
  expect_equal(captured$dilate_steps, 1L)
})
