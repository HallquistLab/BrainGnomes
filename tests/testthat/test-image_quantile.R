test_that("image_quantile computes correct quantiles in 3D and 4D with masking and zero exclusion", {
  skip_if_not_installed("RNifti")
  
  # --- Create 3D image with known values and some zeros
  arr3d <- array(1:125, dim = c(5, 5, 5))
  arr3d[1:10] <- 0  # insert zeros
  nifti3d <- RNifti::asNifti(arr3d)
  file3d <- tempfile(fileext = ".nii.gz")
  RNifti::writeNifti(nifti3d, file3d)
  
  # --- Create mask (only include voxels > 60)
  mask_arr <- array(as.numeric(arr3d > 60), dim = dim(arr3d))
  mask_nifti <- RNifti::asNifti(mask_arr)
  mask_file <- tempfile(fileext = ".nii.gz")
  RNifti::writeNifti(mask_nifti, mask_file)
  
  # --- Test all combinations of exclude_zero and use_mask for 3D
  combinations <- expand.grid(exclude_zero = c(TRUE, FALSE), use_mask = c(TRUE, FALSE))
  
  for (i in seq_len(nrow(combinations))) {
    ez <- combinations$exclude_zero[i]
    um <- combinations$use_mask[i]
    label <- paste0("3D - exclude_zero=", ez, ", use_mask=", um)
    
    expected <- arr3d
    if (um) expected <- expected[mask_arr == 1]
    if (ez) expected <- expected[expected != 0]
    
    q_val <- image_quantile(
      in_file = file3d,
      brain_mask = if (um) mask_file else NULL,
      quantiles = 0.5,
      exclude_zero = ez
    )
    
    expect_equal(as.numeric(q_val), as.numeric(quantile(expected, 0.5, names = FALSE)),
                 tolerance = 1e-6, label = label)
  }
  
  # --- Create 4D image (replicate 3D twice with an offset)
  arr4d <- array(c(arr3d, arr3d + 100), dim = c(5, 5, 5, 2))
  nifti4d <- RNifti::asNifti(arr4d)
  file4d <- tempfile(fileext = ".nii.gz")
  RNifti::writeNifti(nifti4d, file4d)
  
  for (i in seq_len(nrow(combinations))) {
    ez <- combinations$exclude_zero[i]
    um <- combinations$use_mask[i]
    label <- paste0("4D - exclude_zero=", ez, ", use_mask=", um)
    
    expected <- as.vector(arr4d)
    if (um) {
      mask_vec <- rep(as.numeric(mask_arr > 0), 2)  # repeat for 2 volumes
      expected <- expected[mask_vec == 1]
    }
    if (ez) expected <- expected[expected != 0]
    
    q_val <- image_quantile(
      in_file = file4d,
      brain_mask = if (um) mask_file else NULL,
      quantiles = 0.5,
      exclude_zero = ez
    )
    
    expect_equal(as.numeric(q_val), as.numeric(quantile(expected, 0.5, names = FALSE)),
                 tolerance = 1e-6, label = label)
  }
})

test_that("image_quantile returns combined quantiles identically to separate calls", {
  skip_if_not_installed("RNifti")

  dims <- c(7L, 6L, 5L, 9L)
  values <- ((seq_len(prod(dims)) * 37L) %% 997L) - 400L
  values[seq.int(3L, length(values), by = 19L)] <- 0L
  image <- array(values, dim = dims)
  mask <- array((seq_len(prod(dims[1:3])) %% 4L) != 0L, dim = dims[1:3])
  image_file <- tempfile(fileext = ".nii.gz")
  mask_file <- tempfile(fileext = ".nii.gz")
  on.exit(unlink(c(image_file, mask_file)), add = TRUE)
  RNifti::writeNifti(RNifti::asNifti(image), image_file)
  RNifti::writeNifti(RNifti::asNifti(mask), mask_file)

  probabilities <- c(0.5, 0.02, 0.98, 0.25, 0.5)
  combined <- image_quantile(
    image_file, mask_file, probabilities, exclude_zero = TRUE
  )
  separate <- vapply(
    probabilities,
    function(probability) {
      unname(image_quantile(
        image_file, mask_file, probability, exclude_zero = TRUE
      ))
    },
    numeric(1L)
  )
  retained <- as.vector(image)[rep(as.vector(mask), dims[4])]
  retained <- retained[retained != 0]
  expected <- unname(stats::quantile(
    retained, probabilities, names = FALSE, type = 7
  ))

  expect_equal(unname(combined), separate, tolerance = 0)
  expect_equal(unname(combined), expected, tolerance = 1e-6)
  expect_identical(
    names(combined), sprintf("%.2f%%", probabilities * 100)
  )
})

test_that("image_quantile rejects empty, missing, and non-finite probabilities", {
  skip_if_not_installed("RNifti")
  image_file <- tempfile(fileext = ".nii.gz")
  RNifti::writeNifti(RNifti::asNifti(array(1:8, dim = c(2, 2, 2))), image_file)

  expect_error(
    image_quantile(image_file, quantiles = numeric()),
    "quantiles must contain at least one probability",
    fixed = TRUE
  )
  expect_error(
    image_quantile(image_file, quantiles = NA_real_),
    "quantiles must not contain NA values",
    fixed = TRUE
  )
  expect_error(
    image_quantile(image_file, quantiles = NaN),
    "quantiles must not contain NaN values",
    fixed = TRUE
  )
  for (probability in c(Inf, -Inf)) {
    expect_error(
      image_quantile(image_file, quantiles = probability),
      "quantiles must contain only finite values; Inf and -Inf are not allowed",
      fixed = TRUE
    )
  }
})
