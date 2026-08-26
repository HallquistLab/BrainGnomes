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
