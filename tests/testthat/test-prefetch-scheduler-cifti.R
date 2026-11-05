test_that("PBS and Slurm prefetch forward the effective CIFTI-defaults option", {
  skip_on_os("windows")
  bash <- Sys.which("bash")
  skip_if(!nzchar(bash), "Bash is required to exercise scheduler command arguments")
  root <- withr::local_tempdir()
  capture <- file.path(root, "arguments.txt")
  withr::local_envvar(
    capture = capture, prefetch_container = "container with spaces.sif",
    prefetch_script = "prefetch helper.py", prefetch_spaces = "MNI152NLin6Asym:res-2",
    prefetch_summary_file = "summary.json", prefetch_manifest_file = "manifest.json",
    prefetch_state_file = "state.json", PBS_JOBID = "123.pbs", SLURM_JOB_ID = "456"
  )
  for (extension in c("pbs", "sbatch")) {
    script <- system.file("hpc_scripts", paste0("prefetch_templates.", extension), package = "BrainGnomes")
    expect_true(file.exists(script))
    lines <- readLines(script)
    start <- match("# run the TemplateFlow prefetch helper inside the fMRIPrep container", lines)
    end <- match("cmd_pid=$!", lines)
    expect_true(!is.na(start) && !is.na(end) && end > start)
    harness <- file.path(root, paste0("command-", extension, ".sh"))
    # Run the production command block, replacing only the execution wrapper.
    # Capturing argv exercises array expansion and quoting without a scheduler
    # or a container runtime, including both enabled and disabled settings.
    writeLines(c("set -e", "bind_args=('-B' 'cache with spaces:cache with spaces')",
      "rel() { printf '%s\\n' \"$@\" > \"$capture\"; }",
      lines[start:end], "wait \"$cmd_pid\""), harness)
    for (setting in c("TRUE", "1", "FALSE", "0", "")) {
      withr::local_envvar(prefetch_include_cifti_defaults = setting)
      output <- system2(bash, shQuote(harness), stdout = TRUE, stderr = TRUE)
      expect_null(attr(output, "status"))
      args <- readLines(capture)
      expect_equal(sum(args == "--include-cifti-defaults"), as.integer(setting %in% c("TRUE", "1")))
      expect_true("container with spaces.sif" %in% args)
      expect_identical(args[match("--output-spaces", args) + 1L], "MNI152NLin6Asym:res-2")
      expect_identical(args[match("--scheduler-job-id", args) + 1L],
        if (extension == "pbs") "123.pbs" else "456")
    }
  }
})
