test_that("real Slurm accounting recovers failed and cancelled tracked allocations", {
  skip_if(Sys.getenv("BG_TEST_RECOVERY_SLURM") != "1", "Set BG_TEST_RECOVERY_SLURM=1 to submit small Slurm test jobs")
  skip_if(!nzchar(Sys.getenv("SLURM_JOB_ID")), "Run inside an interactive srun allocation")
  shared_root <- Sys.getenv("BG_RECOVERY_TEST_ROOT")
  skip_if(!nzchar(shared_root) || !dir.exists(shared_root),
    "Set BG_RECOVERY_TEST_ROOT to a writable directory shared with compute nodes")
  for (command in c("sbatch", "sacct", "scancel", "bash", "md5sum")) {
    skip_if(!nzchar(Sys.which(command)), paste("Required command is unavailable:", command))
  }
  root <- tempfile("recovery-slurm-", tmpdir = normalizePath(shared_root, mustWork = TRUE))
  dir.create(root)
  submitted <- character()
  # Request cleanup on every exit; retain evidence if a runtime assertion fails.
  on.exit({
    for (job_id in submitted) {
      try(suppressWarnings(system2("scancel", shQuote(job_id), stdout = TRUE,
        stderr = TRUE, timeout = 15L)), silent = TRUE)
    }
    message("Slurm recovery test evidence retained at ", root)
  }, add = TRUE)
  db <- file.path(root, "tracking.sqlite")
  create_tracking_db(db)
  cfg <- structure(list(metadata = list(project_directory = root, project_name = "recovery-test",
    sqlite_db = db), compute_environment = list(scheduler = "slurm")), class = "bg_project_cfg")
  tracking <- list(job_name = "bg_recovery_test", sequence_id = "recovery-integration",
    stage = "fmriprep", sub_id = "01", job_role = "subject")
  args <- c("--parsable", "--nodes=1", "--ntasks=1", "--mem=1G", "--time=00:02:00",
    paste0("--output=", shQuote(file.path(root, "worker-%j.out"))),
    paste0("--error=", shQuote(file.path(root, "worker-%j.err"))))
  withr::local_envvar(c(SLURM_ARRAY_JOB_ID = NA, SLURM_ARRAY_TASK_ID = NA,
    PBS_JOBID = NA, PBS_ARRAYID = NA, PBS_ARRAY_INDEX = NA))
  # Poll a specific allocation with a bounded accounting-lag allowance.
  # Input is a confirmed ID and expected state; output is its observation.
  await_state <- function(job_id, expected) {
    deadline <- Sys.time() + 180
    repeat {
      result <- scheduler_job_status(job_id, "slurm")
      if (result$scheduler_status == expected) return(result)
      if (Sys.time() >= deadline) {
        stop("Timed out waiting for ", expected, " for ", job_id, ": ",
          result$scheduler_status, ". See ", root)
      }
      Sys.sleep(2)
    }
  }
  failed <- cluster_job_submit("exit 7", scheduler = "slurm", sched_args = args,
    echo = FALSE, fail_on_error = TRUE, tracking_sqlite_db = db, tracking_args = tracking)
  submitted <- c(submitted, as.character(failed))
  await_state(as.character(failed), "FAILED")
  accounting <- slurm_job_status(as.character(failed), sacct_format = "jobid%100,state%40,exitcode")
  expect_identical(accounting$ExitCode, "7:0")
  expect_identical(get_tracked_job_status(as.character(failed), sqlite_db = db)$status, "QUEUED")
  preview <- reconcile_project_jobs(cfg)
  expect_identical(preview$action, "UPDATE_TERMINAL_FAILURE")
  expect_false(preview$applied)
  expect_true(reconcile_project_jobs(cfg, dry_run = FALSE)$applied)
  expect_identical(get_tracked_job_status(as.character(failed), sqlite_db = db)$status, "FAILED")

  held <- cluster_job_submit("sleep 120", scheduler = "slurm", sched_args = c(args, "--hold"),
    echo = FALSE, fail_on_error = TRUE, tracking_sqlite_db = db, tracking_args = tracking)
  submitted <- c(submitted, as.character(held))
  cancellation <- cancel_project_run(cfg, "recovery-integration", dry_run = FALSE)
  expect_identical(cancellation$job_id, as.character(held))
  expect_identical(cancellation$status, "cancellation_requested")
  expect_identical(get_tracked_job_status(as.character(held), sqlite_db = db)$status, "QUEUED")
  await_state(as.character(held), "CANCELLED")
  expect_true(reconcile_project_jobs(cfg, dry_run = FALSE)$applied)
  expect_identical(get_tracked_job_status(as.character(held), sqlite_db = db)$status, "CANCELLED")
})
