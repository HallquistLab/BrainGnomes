get_topic_rd <- function(topic) {
  source_file <- testthat::test_path("..", "..", "man", paste0(topic, ".Rd"))
  if (file.exists(source_file)) {
    return(tools::parse_Rd(source_file))
  }

  help_file <- utils::help(topic, package = "BrainGnomes")
  if (!length(help_file)) stop("Missing installed help topic: ", topic)
  utils:::.getHelpFile(help_file)
}

rd_tag_text <- function(rd, tag) {
  matching <- vapply(rd, function(x) identical(attr(x, "Rd_tag"), tag), logical(1))
  paste(unlist(rd[matching]), collapse = "")
}

test_that("exported native-backed help topics contain real usage signatures", {
  expected_formals <- list(
    automask = c(
      "img", "outfile", "clfrac", "NN", "erode_steps", "dilate_steps",
      "SIhh", "peels", "fill_holes"
    ),
    filtfilt_cpp = c("x", "b", "a", "padlen", "padtype", "use_zi"),
    image_quantile = c("in_file", "brain_mask", "quantiles", "exclude_zero"),
    lmfit_residuals_4d = c(
      "infile", "X", "include_rows", "add_intercept", "outfile", "internal",
      "preserve_mean", "set_mean", "regress_cols", "exclusive"
    ),
    natural_spline_4d = c(
      "infile", "t_interpolate", "edge_nn", "outfile", "internal"
    ),
    natural_spline_interp = c("x", "y", "xout"),
    remove_nifti_volumes = c("infile", "remove_tpts", "outfile")
  )

  namespace <- asNamespace("BrainGnomes")
  for (topic in names(expected_formals)) {
    rd <- get_topic_rd(topic)
    usage <- rd_tag_text(rd, "\\usage")
    expect_true(nzchar(usage), info = paste(topic, "must have a usage section"))
    expect_match(usage, paste0("\\b", topic, "\\("), info = topic)
    expect_identical(
      names(formals(get(topic, envir = namespace))),
      expected_formals[[topic]],
      info = topic
    )
  }

  expect_identical(formals(automask)$outfile, "")
})

test_that("corrected public examples and scientific output contracts stay documented", {
  automask_rd <- paste(unlist(get_topic_rd("automask")), collapse = "")
  quantile_rd <- paste(unlist(get_topic_rd("image_quantile")), collapse = "")
  run_project_topic <- get_topic_rd("run_project")
  run_project_rd <- paste(unlist(run_project_topic), collapse = "")
  extract_rd <- paste(unlist(get_topic_rd("extract_rois")), collapse = "")

  expect_match(automask_rd, "mask <- automask\\(")
  expect_false(grepl("automask_rcpp", automask_rd, fixed = TRUE))
  expect_match(quantile_rd, "quantiles = 0.5", fixed = TRUE)
  expect_false(grepl('image_quantile("bold.nii.gz", 0.5)', quantile_rd, fixed = TRUE))
  expect_false(grepl("prompt = TRUE", run_project_rd, fixed = TRUE))
  expect_match(run_project_rd, 'steps = "fmriprep"', fixed = TRUE)
  supported_stages <- c(
    "flywheel_sync", "bids_conversion", "mriqc", "fmriprep", "aroma",
    "postprocess", "extract_rois"
  )
  for (stage in supported_stages) {
    expect_match(run_project_rd, stage, fixed = TRUE)
  }
  run_project_seealso <- rd_tag_text(run_project_topic, "\\seealso")
  expect_match(run_project_seealso, "run_bids_validation", fixed = TRUE)
  expect_match(extract_rd, "range from -1 to 1", fixed = TRUE)
  expect_match(extract_rd, "use NA on the diagonal", fixed = TRUE)
  expect_false(grepl("becomes 15", extract_rd, fixed = TRUE))
})

test_that("Quickstart renders CLI help from the installed command", {
  quickstart_path <- testthat::test_path("..", "..", "vignettes", "braingnomes_quickstart.Rmd")
  if (!file.exists(quickstart_path)) {
    quickstart_path <- system.file("doc", "braingnomes_quickstart.Rmd", package = "BrainGnomes")
  }
  expect_true(nzchar(quickstart_path) && file.exists(quickstart_path))
  quickstart <- readLines(quickstart_path, warn = FALSE)
  quickstart_text <- paste(quickstart, collapse = "\n")

  expect_match(quickstart_text, 'system.file("BrainGnomes", package = "BrainGnomes")', fixed = TRUE)
  expect_match(quickstart_text, "system2(", fixed = TRUE)
  expect_false(grepl("run_project <project_directory|config.yaml> -steps", quickstart_text, fixed = TRUE))
})
