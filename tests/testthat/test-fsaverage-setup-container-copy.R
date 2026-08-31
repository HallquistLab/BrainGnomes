test_that("Slurm and PBS fsaverage setup support containers without rsync", {
  skip_if(Sys.which("bash") == "", "bash is required for shell syntax validation")
  skip_on_os("windows")

  for (extension in c("sbatch", "pbs")) {
    script_name <- paste0("fsaverage_setup.", extension)
    source_path <- testthat::test_path(
      "..", "..", "inst", "hpc_scripts", script_name
    )
    script_path <- if (file.exists(source_path)) {
      normalizePath(source_path, mustWork = TRUE)
    } else {
      normalizePath(
        system.file("hpc_scripts", script_name, package = "BrainGnomes"),
        mustWork = TRUE
      )
    }

    script <- paste(readLines(script_path, warn = FALSE), collapse = "\n")
    expect_match(script, "command -v rsync", info = extension)
    expect_match(
      script, "rsync is unavailable; copying fsaverage with cp",
      fixed = TRUE, info = extension
    )
    expect_match(
      script, "cp -a --no-preserve=ownership,timestamps,mode",
      fixed = TRUE, info = extension
    )
    expect_equal(system2("bash", c("-n", script_path)), 0L, info = extension)
  }
})
