test_that("fsaverage setup supports fMRIPrep containers without rsync", {
  skip_if(Sys.which("bash") == "", "bash is required for shell syntax validation")
  skip_on_os("windows")

  source_path <- testthat::test_path(
    "..", "..", "inst", "hpc_scripts", "fsaverage_setup.sbatch"
  )
  script_path <- if (file.exists(source_path)) {
    normalizePath(source_path, mustWork = TRUE)
  } else {
    normalizePath(
      system.file("hpc_scripts", "fsaverage_setup.sbatch", package = "BrainGnomes"),
      mustWork = TRUE
    )
  }

  script <- paste(readLines(script_path, warn = FALSE), collapse = "\n")
  expect_match(script, "command -v rsync")
  expect_match(script, "rsync is unavailable; copying fsaverage with cp", fixed = TRUE)
  expect_match(
    script,
    "cp -a --no-preserve=ownership,timestamps,mode",
    fixed = TRUE
  )
  expect_equal(system2("bash", c("-n", script_path)), 0L)
})
