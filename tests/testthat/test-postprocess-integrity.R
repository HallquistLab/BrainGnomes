#' Build a small deterministic postprocessing input and isolated destinations
#' @param root Temporary directory to populate.
#' @return BOLD, atlas, mask, configuration, and root paths for local tests.
#' @noRd
make_postprocess_integrity_fixture <- function(root = tempfile("postprocess-integrity-")) {
  dir.create(root, recursive = TRUE)
  output <- file.path(root, "output")
  scratch <- file.path(root, "scratch")
  dir.create(output)
  dir.create(scratch)
  bold <- file.path(root, "sub-01_task-rest_desc-preproc_bold.nii.gz")
  values <- outer(seq_len(8L), seq_len(30L), function(voxel, time) {
    100 + voxel + sin(time / (voxel + 1))
  })
  RNifti::writeNifti(array(values, c(2L, 2L, 2L, 30L)), bold)
  mask <- file.path(root, "mask.nii.gz")
  atlas <- file.path(root, "atlas.nii.gz")
  RNifti::writeNifti(array(1L, c(2L, 2L, 2L)), mask)
  file.copy(mask, atlas)
  data.table::fwrite(data.frame(
    framewise_displacement = c(1, rep(0, 29)), trans_x = seq_len(30L)
  ), file.path(root, "sub-01_task-rest_desc-confounds_timeseries.tsv"), sep = "\t")
  cfg <- list(bids_desc = "clean", tr = 1, output_dir = output,
    scratch_directory = scratch, project_name = "integrity", overwrite = TRUE,
    keep_intermediates = FALSE, apply_mask = list(enable = FALSE),
    spatial_smooth = list(enable = FALSE), intensity_normalize = list(enable = FALSE),
    apply_aroma = list(enable = FALSE), temporal_filter = list(enable = FALSE),
    confound_regression = list(enable = FALSE), confound_calculate = list(enable = FALSE),
    motion_filter = list(enable = FALSE), scrubbing = list(enable = FALSE,
      expression = "framewise_displacement > 0.5", interpolate = FALSE,
      apply = FALSE, add_to_confounds = FALSE))
  list(root = root, bold = bold, mask = mask, atlas = atlas, cfg = cfg)
}

for (peer_state in c("completed", "failed", "reused")) {
  test_that(paste("scratch survives an overlapping", peer_state, "stream"), {
    fixture <- make_postprocess_integrity_fixture()
    withr::defer(unlink(fixture$root, recursive = TRUE))
    withr::local_envvar(log_file = file.path(fixture$root, "worker.log"))
    cfg <- fixture$cfg
    cfg$apply_mask <- list(enable = TRUE, prefix = "m", mask_file = fixture$mask)
    peer <- fixture$cfg
    peer$bids_desc <- "other"
    peer_file <- file.path(peer$output_dir, "sub-01_task-rest_desc-other_bold.nii.gz")
    if (peer_state == "reused") {
      file.copy(fixture$bold, peer_file)
      peer$overwrite <- FALSE
    }
    if (peer_state == "failed") peer$tr <- NULL
    staged <- NULL
    local_mocked_bindings(
      # Mask generation is irrelevant to the publication/cleanup interleaving.
      # Return the fixture mask without invoking any external imaging runtime.
      postprocess_automask = function(in_file, outfile) fixture$mask,
      # Pause the outer stream after staging, finish another invocation, then
      # resume. This deterministic interleaving also runs on Windows.
      apply_mask = function(in_file, mask_file, out_file, ...) {
        file.copy(in_file, out_file)
        staged <<- out_file
        if (peer_state == "failed") {
          expect_error(postprocess_subject(fixture$bold, peer), "tr field")
        } else {
          expect_identical(postprocess_subject(fixture$bold, peer), peer_file)
        }
        expect_true(file.exists(out_file))
        expect_equal(unname(tools::md5sum(out_file)), unname(tools::md5sum(in_file)))
        out_file
      }
    )
    result <- postprocess_subject(fixture$bold, cfg)
    expect_true(file.exists(result))
    expect_false(dir.exists(dirname(staged)))
    expect_equal(unname(tools::md5sum(result)), unname(tools::md5sum(fixture$bold)))
  })
}

test_that("separate workers retain their own staged outputs on shared scratch", {
  skip_on_os("windows")
  fixture <- make_postprocess_integrity_fixture()
  withr::defer(unlink(fixture$root, recursive = TRUE))
  withr::local_envvar(log_file = file.path(fixture$root, "worker.log"))
  cfg <- fixture$cfg
  cfg$apply_mask <- list(enable = TRUE, prefix = "m", mask_file = fixture$mask)
  ready <- file.path(fixture$root, "ready")
  resume <- file.path(fixture$root, "resume")
  local_mocked_bindings(postprocess_automask = function(in_file, outfile) fixture$mask)
  worker <- parallel::mcparallel({
    local_mocked_bindings(
      # Publish the staged path as a barrier; the parent completes its own
      # stream before releasing this worker. Bound the wait on any failure.
      apply_mask = function(in_file, mask_file, out_file, ...) {
        file.copy(in_file, out_file)
        writeLines(out_file, paste0(ready, ".tmp"))
        if (!file.rename(paste0(ready, ".tmp"), ready)) stop("Unable to signal staged output")
        deadline <- Sys.time() + 30
        while (!file.exists(resume) && Sys.time() < deadline) Sys.sleep(0.01)
        if (!file.exists(resume)) stop("Parent did not release the staged worker")
        out_file
      }
    )
    postprocess_subject(fixture$bold, cfg)
  }, silent = TRUE)
  collected <- FALSE
  withr::defer({
    if (!collected && is.null(parallel::mccollect(worker, wait = FALSE))) {
      tools::pskill(worker$pid)
      parallel::mccollect(worker)
    }
  })
  deadline <- Sys.time() + 30
  while (!file.exists(ready) && Sys.time() < deadline) Sys.sleep(0.01)
  expect_true(file.exists(ready))
  if (!file.exists(ready)) stop("Worker did not stage its output")
  staged <- readLines(ready)
  peer <- fixture$cfg
  peer$bids_desc <- "other"
  peer_output <- postprocess_subject(fixture$bold, peer)
  expect_true(file.exists(staged))
  writeLines("resume", resume)
  result <- parallel::mccollect(worker)
  collected <- TRUE
  expect_false(inherits(result[[1L]], "try-error"))
  expect_true(file.exists(result[[1L]]))
  expect_true(file.exists(peer_output))
  expect_false(dir.exists(dirname(staged)))
})

test_that("recomputation retires obsolete temporal companions before ROI extraction", {
  fixture <- make_postprocess_integrity_fixture()
  withr::defer(unlink(fixture$root, recursive = TRUE))
  withr::local_envvar(log_file = file.path(fixture$root, "worker.log"))
  local_mocked_bindings(postprocess_automask = function(in_file, outfile) fixture$mask)
  cfg <- fixture$cfg
  cfg$scrubbing$enable <- TRUE
  cfg$confound_calculate <- list(enable = TRUE, noproc_columns = "framewise_displacement")
  cfg$confound_regression <- list(enable = TRUE, noproc_columns = "trans_x")
  # Generate real temporal companions without requiring image denoising.
  cfg$force_processing_order <- TRUE
  cfg$processing_steps <- character()
  first <- postprocess_subject(fixture$bold, cfg)
  ids <- as.list(extract_bids_info(first))
  companions <- c(get_censor_file(ids), vapply(c("scrub", "confounds", "regressors"),
    function(suffix) construct_bids_filename(modifyList(ids,
      list(suffix = suffix, ext = ".tsv")), full.names = TRUE), character(1)))
  expect_true(all(file.exists(companions)))
  expect_equal(readLines(companions[1]), as.character(c(0, rep(1, 29))))
  other_stream <- sub("desc-clean", "desc-other", companions, fixed = TRUE)
  expect_true(all(file.copy(companions, other_stream)))
  other_hashes <- tools::md5sum(other_stream)

  cfg$scrubbing$enable <- cfg$confound_calculate$enable <- cfg$confound_regression$enable <- FALSE
  cfg$overwrite <- FALSE
  expect_identical(postprocess_subject(fixture$bold, cfg), first)
  expect_true(all(file.exists(companions))) # Reuse preserves the original producer.
  cfg$overwrite <- TRUE
  final <- postprocess_subject(fixture$bold, cfg)
  expect_false(any(file.exists(companions)))
  expect_identical(tools::md5sum(other_stream), other_hashes)
  expect_equal(dim(RNifti::readNifti(final))[4], 30L)
  provenance <- read_derivative_provenance(final)
  expect_false(provenance$BrainGnomes$Resolved$Censor$Available)
  result <- extract_rois(final, fixture$atlas, fixture$cfg$output_dir,
    cor_method = "none", min_vox_per_roi = 1L)[[1L]]
  timeseries <- read.delim(result$timeseries)
  expect_identical(timeseries$volume, seq_len(30L))
  expect_equal(timeseries$roi1,
    colMeans(matrix(RNifti::readNifti(fixture$bold), nrow = 8L)), tolerance = 1e-7)
})
