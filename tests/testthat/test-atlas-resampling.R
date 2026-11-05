write_atlas_resampling_nifti <- function(values, path,
                                         voxel_sizes = c(2, 2, 2),
                                         offset = c(0, 0, 0)) {
  image <- RNifti::asNifti(values)
  pixdim(image) <- if (length(dim(values)) == 4L) {
    c(voxel_sizes, 1)
  } else {
    voxel_sizes
  }
  affine <- diag(c(voxel_sizes, 1))
  affine[1:3, 4] <- offset
  attr(affine, "code") <- 4L
  image <- RNifti::`qform<-`(image, affine)
  image <- RNifti::`sform<-`(image, affine)
  RNifti::writeNifti(image, path)
  invisible(path)
}

test_that("matching atlas grids are never resampled", {
  skip_if_not_installed("RNifti")

  root <- tempfile("atlas-grid-match-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  bold_file <- file.path(
    root,
    "sub-01_task-rest_space-MNI152NLin2009cAsym_desc-clean_bold.nii.gz"
  )
  atlas_file <- file.path(root, "atlas.nii.gz")
  write_atlas_resampling_nifti(
    array(rnorm(3L * 4L * 2L * 5L), c(3L, 4L, 2L, 5L)),
    bold_file,
    voxel_sizes = c(2.2, 2.3, 2.4),
    offset = c(-8, -10, -12)
  )
  write_atlas_resampling_nifti(
    array(rep(1:2, each = 12L), c(3L, 4L, 2L)),
    atlas_file,
    voxel_sizes = c(2.2, 2.3, 2.4),
    offset = c(-8, -10, -12)
  )

  local_mocked_bindings(
    resample_atlas_to_bold = function(...) {
      stop("resampling must not be called for a matching grid")
    },
    .package = "BrainGnomes"
  )
  cache_dir <- file.path(root, "cache")
  observed <- prepare_atlas_for_extraction(
    atlas_file,
    bold_file,
    allow_atlas_resampling = TRUE,
    atlas_space = NULL,
    cache_dir = cache_dir
  )

  expect_identical(observed, atlas_file)
  expect_false(dir.exists(cache_dir))
})

test_that("atlas spaces are inferred only from formal BIDS entities", {
  observed <- bids_space_from_filename(c(
    "space-MNI152NLin2009cAsym_atlas-Schaefer444_dseg.nii.gz",
    "sub-01_space-MNI152NLin6Asym_atlas-Demo_dseg.nii.gz",
    "Schaefer_444_final_2009c_1.0mm.nii.gz",
    "notspace-MNI152NLin6Asym_atlas-Demo_dseg.nii.gz",
    "notspace-Wrong_space-MNI152NLin2009cAsym_atlas-Demo_dseg.nii.gz"
  ))
  expect_identical(
    observed,
    c(
      "MNI152NLin2009cAsym", "MNI152NLin6Asym", NA, NA,
      "MNI152NLin2009cAsym"
    )
  )
})

test_that("atlas resampling is opt-in and restricted to a declared matching space", {
  skip_if_not_installed("RNifti")

  root <- tempfile("atlas-space-check-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  bold_file <- file.path(
    root,
    "sub-01_task-rest_space-MNI152NLin2009cAsym_desc-clean_bold.nii.gz"
  )
  atlas_file <- file.path(root, "atlas.nii.gz")
  write_atlas_resampling_nifti(
    array(rnorm(2L * 2L * 2L * 5L), c(2L, 2L, 2L, 5L)),
    bold_file,
    offset = c(-6, -8, -10)
  )
  write_atlas_resampling_nifti(
    array(rep(1:2, each = 4L), c(2L, 2L, 2L)),
    atlas_file,
    offset = c(-4, -8, -10)
  )

  expect_error(
    prepare_atlas_for_extraction(
      atlas_file, bold_file,
      allow_atlas_resampling = FALSE,
      cache_dir = file.path(root, "cache")
    ),
    "spatial NIfTI grid mismatch.*allow_atlas_resampling = TRUE.*space-<label>"
  )
  expect_error(
    prepare_atlas_for_extraction(
      atlas_file, bold_file,
      allow_atlas_resampling = TRUE,
      cache_dir = file.path(root, "cache")
    ),
    "atlas_space is required"
  )
  expect_error(
    prepare_atlas_for_extraction(
      atlas_file, bold_file,
      allow_atlas_resampling = TRUE,
      atlas_space = "MNI152NLin6Asym",
      cache_dir = file.path(root, "cache")
    ),
    "Registration between spaces is not performed"
  )

  calls <- 0L
  local_mocked_bindings(
    resample_atlas_to_bold = function(atlas_file, bold_file, cache_dir,
                                      lg = NULL) {
      calls <<- calls + 1L
      atlas_file
    },
    .package = "BrainGnomes"
  )
  observed <- prepare_atlas_for_extraction(
    atlas_file, bold_file,
    allow_atlas_resampling = TRUE,
    atlas_space = "MNI152NLin2009cAsym",
    cache_dir = file.path(root, "cache")
  )
  expect_identical(observed, atlas_file)
  expect_identical(calls, 1L)
})

test_that("atlas filename space takes precedence over the YAML fallback", {
  skip_if_not_installed("RNifti")

  root <- tempfile("atlas-filename-space-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  bold_file <- file.path(
    root,
    "sub-01_task-rest_space-MNI152NLin2009cAsym_desc-clean_bold.nii.gz"
  )
  atlas_file <- file.path(
    root,
    "space-MNI152NLin2009cAsym_atlas-Demo_dseg.nii.gz"
  )
  wrong_atlas_file <- file.path(
    root,
    "space-MNI152NLin6Asym_atlas-Demo_dseg.nii.gz"
  )
  write_atlas_resampling_nifti(
    array(rnorm(2L * 2L * 2L * 5L), c(2L, 2L, 2L, 5L)),
    bold_file,
    offset = c(-6, -8, -10)
  )
  atlas_values <- array(rep(1:2, each = 4L), c(2L, 2L, 2L))
  write_atlas_resampling_nifti(
    atlas_values, atlas_file, offset = c(-4, -8, -10)
  )
  write_atlas_resampling_nifti(
    atlas_values, wrong_atlas_file, offset = c(-4, -8, -10)
  )

  calls <- 0L
  local_mocked_bindings(
    resample_atlas_to_bold = function(atlas_file, bold_file, cache_dir,
                                      lg = NULL) {
      calls <<- calls + 1L
      atlas_file
    },
    .package = "BrainGnomes"
  )

  inferred <- prepare_atlas_for_extraction(
    atlas_file, bold_file,
    allow_atlas_resampling = TRUE,
    atlas_space = NULL,
    cache_dir = file.path(root, "cache")
  )
  expect_identical(inferred, atlas_file)
  expect_identical(calls, 1L)

  expect_error(
    prepare_atlas_for_extraction(
      atlas_file, bold_file,
      allow_atlas_resampling = TRUE,
      atlas_space = "MNI152NLin6Asym",
      cache_dir = file.path(root, "cache")
    ),
    "filename space-MNI152NLin2009cAsym conflicts.*atlas_space"
  )
  expect_error(
    prepare_atlas_for_extraction(
      wrong_atlas_file, bold_file,
      allow_atlas_resampling = TRUE,
      atlas_space = NULL,
      cache_dir = file.path(root, "cache")
    ),
    "atlas declares space-MNI152NLin6Asym.*BOLD image declares space-MNI152NLin2009cAsym"
  )
  expect_identical(calls, 1L)
})

test_that("resampled atlases are cached by source content and target grid", {
  skip_if_not_installed("RNifti")
  skip_if_not_installed("filelock")

  root <- tempfile("atlas-cache-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  bold_file <- file.path(
    root,
    "sub-01_task-rest_space-MNI152NLin2009cAsym_desc-clean_bold.nii.gz"
  )
  atlas_file <- file.path(root, "source-atlas.nii.gz")
  cache_dir <- file.path(root, "cache")
  voxel_sizes <- c(2.5, 2.6, 2.7)
  offset <- c(-7, -9, -11)
  write_atlas_resampling_nifti(
    array(rnorm(2L * 2L * 2L * 6L), c(2L, 2L, 2L, 6L)),
    bold_file,
    voxel_sizes = voxel_sizes,
    offset = offset
  )
  write_atlas_resampling_nifti(
    array(rep(1:2, each = 32L), c(4L, 4L, 4L)),
    atlas_file,
    voxel_sizes = c(1.25, 1.3, 1.35),
    offset = offset
  )

  backend_calls <- 0L
  target_labels <- array(rep(1:2, each = 4L), c(2L, 2L, 2L))
  local_mocked_bindings(
    load_image_resampling_backend = function(...) {
      list(resample_image_to_reference = function(
          source_file, reference_file, output, interpolation) {
        backend_calls <<- backend_calls + 1L
        expect_identical(interpolation, "nearest")
        write_atlas_resampling_nifti(
          target_labels, output,
          voxel_sizes = voxel_sizes,
          offset = offset
        )
        output
      })
    },
    .package = "BrainGnomes"
  )

  first <- resample_atlas_to_bold(
    atlas_file, bold_file, cache_dir = cache_dir
  )
  second <- resample_atlas_to_bold(
    atlas_file, bold_file, cache_dir = cache_dir
  )

  expect_identical(first, second)
  expect_true(file.exists(first))
  expect_true(pp_compare_nifti_grid(bold_file, first)$passed)
  expect_identical(backend_calls, 1L)
  expect_setequal(
    sort(unique(as.vector(RNifti::readNifti(first)))),
    c(1, 2)
  )
})

test_that("atlas resampling rejects label loss", {
  skip_if_not_installed("RNifti")

  root <- tempfile("atlas-label-loss-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  source_file <- file.path(root, "source.nii.gz")
  output_file <- file.path(root, "output.nii.gz")
  write_atlas_resampling_nifti(
    array(c(rep(1L, 7L), 2L), c(2L, 2L, 2L)), source_file
  )
  write_atlas_resampling_nifti(
    array(1L, c(2L, 2L, 2L)), output_file
  )

  expect_error(
    validate_resampled_atlas_labels(source_file, output_file),
    "lost labels: 2"
  )
})

test_that("atlas lock timeout stops before cache validation or backend access", {
  root <- withr::local_tempdir()
  bold <- file.path(root, "bold.nii.gz")
  atlas <- file.path(root, "atlas.nii.gz")
  RNifti::writeNifti(array(1, c(2L, 2L, 2L, 3L)), bold)
  RNifti::writeNifti(array(1L, c(2L, 2L, 2L)), atlas)
  withr::local_options(BrainGnomes.atlas_resample_lock_timeout = 0)
  local_mocked_bindings(lock = function(...) NULL,
    unlock = function(...) stop("Must not unlock an unacquired lock"), .package = "filelock")
  local_mocked_bindings(
    pp_compare_nifti_grid = function(...) stop("Must not inspect cached outputs without a lock"),
    load_image_resampling_backend = function(...) stop("Must not load backend without a lock")
  )
  expect_error(resample_atlas_to_bold(atlas, bold, file.path(root, "cache")),
    "Timed out waiting for atlas-resampling cache lock.*milliseconds")
  expect_length(list.files(file.path(root, "cache"), pattern = "[.]nii"), 0L)
})

test_that("a real atlas lock excludes another process and is released on errors", {
  skip_on_os("windows")
  root <- withr::local_tempdir()
  bold <- file.path(root, "bold.nii.gz")
  atlas <- file.path(root, "atlas.nii.gz")
  cache <- file.path(root, "cache")
  ready <- file.path(root, "ready")
  release <- file.path(root, "release")
  RNifti::writeNifti(array(1, c(2L, 2L, 2L, 3L)), bold)
  RNifti::writeNifti(array(1L, c(2L, 2L, 2L)), atlas)
  dir.create(cache)
  lock_file <- file.path(cache, paste0("atlas_source-",
    substr(unname(tools::md5sum(atlas)), 1L, 12L), "_grid-",
    substr(nifti_grid_checksum(bold), 1L, 12L), ".nii.gz.lock"))
  # Hold the same publication lock from an independent worker until released.
  worker <- parallel::mcparallel({
    handle <- filelock::lock(lock_file, timeout = 0)
    if (is.null(handle)) stop("Worker could not acquire lock")
    file.create(ready)
    deadline <- Sys.time() + 20
    while (!file.exists(release) && Sys.time() < deadline) Sys.sleep(0.01)
    filelock::unlock(handle)
  }, silent = TRUE)
  collected <- FALSE
  withr::defer({
    if (!collected) {
      file.create(release)
      parallel::mccollect(worker)
    }
  })
  deadline <- Sys.time() + 10
  while (!file.exists(ready) && Sys.time() < deadline) Sys.sleep(0.01)
  expect_true(file.exists(ready))
  if (!file.exists(ready)) stop("Worker did not acquire lock")
  withr::local_options(BrainGnomes.atlas_resample_lock_timeout = 0)
  backend_calls <- 0L
  local_mocked_bindings(load_image_resampling_backend = function(...) {
    backend_calls <<- backend_calls + 1L
    stop("simulated backend failure")
  })
  expect_error(resample_atlas_to_bold(atlas, bold, cache), "Timed out")
  expect_identical(backend_calls, 0L)
  file.create(release)
  parallel::mccollect(worker)
  collected <- TRUE
  expect_error(resample_atlas_to_bold(atlas, bold, cache), "simulated backend failure")
  expect_identical(backend_calls, 1L)
  # A fresh process can acquire the lock after the failing invocation exits.
  probe <- parallel::mcparallel({
    handle <- filelock::lock(lock_file, timeout = 0)
    available <- !is.null(handle)
    if (available) filelock::unlock(handle)
    available
  }, silent = TRUE)
  expect_true(parallel::mccollect(probe)[[1L]])
})
