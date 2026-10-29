#' Write a NIfTI fixture with explicit spatial transforms and units
#' @param values Image array.
#' @param path Destination NIfTI path.
#' @param qform Qform affine; defaults to an identity grid in millimeters.
#' @param sform Sform affine; defaults to the qform.
#' @param units NIfTI spatial/temporal unit code.
#' @return Written path, invisibly.
#' @noRd
write_roi_mask_grid_fixture <- function(values, path, qform = diag(4),
                                        sform = qform, units = 2L) {
  image <- RNifti::asNifti(values)
  attr(qform, "code") <- 4L
  attr(sform, "code") <- 4L
  image <- RNifti::`qform<-`(image, qform)
  image <- RNifti::`sform<-`(image, sform)
  image <- RNifti::updateNifti(image, list(xyzt_units = units))
  RNifti::writeNifti(image, path)
  invisible(path)
}

test_that("ROI extraction rejects spatially incompatible masks before writing derivatives", {
  root <- tempfile("roi-mask-grid-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE))
  bold <- file.path(root, "sub-01_task-rest_desc-clean_bold.nii.gz")
  atlas <- file.path(root, "atlas.nii.gz")
  mask <- file.path(root, "mask.nii.gz")
  write_roi_mask_grid_fixture(array(rep(seq_len(30), each = 8), c(2, 2, 2, 30)), bold)
  write_roi_mask_grid_fixture(array(rep(c(1L, 2L), 4), c(2, 2, 2)), atlas)
  translated <- diag(4)
  translated[1, 4] <- 100
  flipped <- diag(c(-1, 1, 1, 1))
  enlarged <- diag(c(2, 1, 1, 1))
  variants <- list(
    translated = list(qform = translated),
    flipped = list(qform = flipped),
    voxel_size = list(qform = enlarged),
    sform_only = list(sform = translated),
    qform_only = list(qform = translated, sform = diag(4)),
    spatial_units = list(units = 1L)
  )
  for (name in names(variants)) {
    do.call(write_roi_mask_grid_fixture, c(list(
      values = array(1L, c(2, 2, 2)), path = mask), variants[[name]]))
    output <- file.path(root, name)
    dir.create(output)
    expect_error(extract_rois(bold, atlas, output, mask_file = mask,
      cor_method = "none", min_vox_per_roi = 1L, allow_atlas_resampling = TRUE),
      "BOLD image/ROI mask spatial NIfTI grid mismatch", info = name)
    expect_length(list.files(output, recursive = TRUE, all.files = TRUE, no.. = TRUE), 0L)
  }

  # Temporal units need not match a spatial mask, and small serialization
  # differences in affine coordinates are allowed by the shared grid tolerance.
  close <- diag(4)
  close[1, 4] <- 1e-6
  write_roi_mask_grid_fixture(array(rep(c(1L, 0L), 4), c(2, 2, 2)),
    mask, qform = close, units = 10L)
  output <- file.path(root, "matched")
  dir.create(output)
  result <- extract_rois(bold, atlas, output, mask_file = mask,
    cor_method = "none", min_vox_per_roi = 1L)[[1L]]
  series <- read.delim(result$timeseries)
  expect_equal(series$roi1, seq_len(30))
  expect_true(all(is.na(series$roi2)))
})
