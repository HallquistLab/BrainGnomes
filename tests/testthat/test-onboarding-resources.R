test_that("DESCRIPTION declares stage-dependent external requirements", {
  description_file <- system.file("DESCRIPTION", package = "BrainGnomes")
  if (!nzchar(description_file) || !file.exists(description_file)) {
    description_file <- testthat::test_path("..", "..", "DESCRIPTION")
  }
  description <- read.dcf(description_file)

  expect_true("SystemRequirements" %in% colnames(description))
  requirements <- description[1L, "SystemRequirements"]
  for (term in c("SLURM", "TORQUE/PBS", "Singularity", "Python 3", "FreeSurfer")) {
    expect_match(requirements, term, fixed = TRUE)
  }

  package_description <- description[1L, "Description"]
  expect_match(package_description, "high-performance computing", fixed = TRUE)
  expect_match(package_description, "without\\s+a cluster")
  expect_match(package_description, "TemplateFlow", fixed = TRUE)
})

test_that("installed onboarding configuration is loadable without validation", {
  config_file <- system.file(
    "extdata", "example_project_config.yaml",
    package = "BrainGnomes"
  )
  if (!nzchar(config_file)) {
    config_file <- testthat::test_path(
      "..", "..", "inst", "extdata", "example_project_config.yaml"
    )
  }

  expect_true(file.exists(config_file))
  config <- load_project(config_file, validate = FALSE)
  expect_s3_class(config, "bg_project_cfg")
  expect_identical(config$metadata$project_name, "example_project")
  expect_identical(config$compute_environment$scheduler, "slurm")

  stages <- c(
    "flywheel_sync", "bids_conversion", "bids_validation", "mriqc",
    "fmriprep", "aroma", "postprocess", "extract_rois"
  )
  expect_true(all(vapply(stages, function(stage) identical(config[[stage]]$enable, FALSE), logical(1))))
})

test_that("local onboarding documents executable and HPC boundaries", {
  vignette_file <- testthat::test_path("..", "..", "vignettes", "local_onboarding.Rmd")
  if (!file.exists(vignette_file)) {
    vignette_file <- system.file("doc", "local_onboarding.Rmd", package = "BrainGnomes")
  }
  expect_true(nzchar(vignette_file) && file.exists(vignette_file))
  contents <- paste(readLines(vignette_file, warn = FALSE), collapse = "\n")

  expect_match(contents, "load_project(example_config_file, validate = FALSE)", fixed = TRUE)
  expect_match(contents, "extract_bids_info(filenames, drop_unused = TRUE)", fixed = TRUE)
  expect_match(contents, "image_quantile(image_file", fixed = TRUE)
  expect_match(contents, "dry_run = TRUE", fixed = TRUE)
  expect_match(contents, "not a substitute for configuration", fixed = TRUE)
  expect_match(contents, "submitted separately through `run_bids_validation()`", fixed = TRUE)
})
