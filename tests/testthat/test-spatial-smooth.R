test_that("spatial_smooth obtains both SUSAN quantiles in one call", {
  quantile_calls <- list()
  commands <- character()

  result <- with_mocked_bindings(
    spatial_smooth(
      in_file = "input.nii.gz",
      out_file = "output.nii.gz",
      fwhm_mm = 6,
      brain_mask = "mask.nii.gz"
    ),
    image_quantile = function(in_file, brain_mask = NULL, quantiles, ...) {
      quantile_calls[[length(quantile_calls) + 1L]] <<- list(
        in_file = in_file,
        brain_mask = brain_mask,
        quantiles = quantiles
      )
      setNames(c(100, 500), c("2.00%", "50.00%"))
    },
    run_fsl_command = function(cmd, ...) {
      commands <<- c(commands, as.character(cmd))
      invisible(NULL)
    },
    rm_niftis = function(...) invisible(NULL)
  )

  expect_identical(result, "output.nii.gz")
  expect_length(quantile_calls, 1L)
  expect_identical(quantile_calls[[1L]]$in_file, "input.nii.gz")
  expect_identical(quantile_calls[[1L]]$brain_mask, "mask.nii.gz")
  expect_equal(quantile_calls[[1L]]$quantiles, c(0.02, 0.5))

  susan_command <- commands[grepl("^susan ", commands)]
  expect_length(susan_command, 1L)
  susan_tokens <- strsplit(susan_command, " ", fixed = TRUE)[[1L]]
  expect_identical(susan_tokens[[1L]], "susan")
  expect_identical(susan_tokens[[2L]], "input")
  expect_equal(as.numeric(susan_tokens[[3L]]), 300)
})
