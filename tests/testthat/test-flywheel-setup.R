test_that("normalize_flag_value coerces common YAML boolean strings", {
  expect_true(isTRUE(normalize_flag_value("yes")))
  expect_true(isTRUE(normalize_flag_value("TRUE")))
  expect_false(isTRUE(normalize_flag_value("no")))
  expect_identical(normalize_flag_value("maybe"), "maybe")
})

test_that("validate_project accepts quoted Flywheel boolean settings", {
  root <- tempfile("validate_fw_flags_")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  dirs <- c("project", "logs", "scratch", "templateflow", "fw_sync", "fw_tmp")
  dir_paths <- file.path(root, dirs)
  vapply(dir_paths, dir.create, recursive = TRUE, showWarnings = FALSE, FUN.VALUE = logical(1))

  fw_path <- file.path(root, "fw")
  writeLines(c("#!/bin/sh", "exit 0"), fw_path)
  Sys.chmod(fw_path, mode = "0755")

  scfg <- structure(list(
    metadata = list(
      project_name = "quoted_fw_flags",
      project_directory = file.path(root, "project"),
      log_directory = file.path(root, "logs"),
      scratch_directory = file.path(root, "scratch"),
      templateflow_home = file.path(root, "templateflow"),
      flywheel_sync_directory = file.path(root, "fw_sync"),
      flywheel_temp_directory = file.path(root, "fw_tmp")
    ),
    flywheel_sync = list(
      enable = "yes",
      memgb = 16,
      nhours = 4,
      ncores = 1,
      cli_options = "",
      sched_args = "",
      source_url = "fw://server/group/project",
      save_audit_logs = "no"
    ),
    bids_conversion = list(enable = FALSE),
    bids_validation = list(enable = FALSE),
    fmriprep = list(enable = FALSE),
    mriqc = list(enable = FALSE),
    aroma = list(enable = FALSE),
    postprocess = list(enable = FALSE),
    extract_rois = list(enable = FALSE),
    compute_environment = list(
      scheduler = "slurm",
      flywheel = fw_path
    )
  ), class = "bg_project_cfg")

  expect_true(isTRUE(validate_project(scfg, correct_problems = FALSE)))
})

test_that("setup_compute_environment prompts for Flywheel path when CLI is absent", {
  root <- tempfile("setup_fw_cli_")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  fw_path <- file.path(root, "fw")
  writeLines(c("#!/bin/sh", "exit 0"), fw_path)
  Sys.chmod(fw_path, mode = "0755")

  scfg <- structure(list(
    compute_environment = list(flywheel = NULL),
    flywheel_sync = list(enable = TRUE),
    bids_conversion = list(enable = FALSE),
    bids_validation = list(enable = FALSE),
    fmriprep = list(enable = FALSE),
    mriqc = list(enable = FALSE),
    aroma = list(enable = FALSE),
    postprocess = list(enable = FALSE)
  ), class = "bg_project_cfg")

  prompts <- character()

  local_mocked_bindings(
    discover_cli_path = function(command) "",
    prompt_input = function(instruct = NULL, ...) {
      prompts <<- c(prompts, instruct)
      fw_path
    },
    .package = "BrainGnomes"
  )

  result <- setup_compute_environment(scfg, fields = "compute_environment/flywheel")

  expect_path_identical(result$compute_environment$flywheel, fw_path)
  expect_true(any(grepl("not found on your PATH", prompts, fixed = TRUE)))
})

test_that("Flywheel setup prefers flyw and preserves the name of a symlink", {
  skip_on_os("windows")
  root <- normalizePath(withr::local_tempdir(), mustWork = TRUE)
  withr::local_envvar(c(PATH = root))
  cli <- file.path(root, "cli-v23")
  writeLines(c("#!/bin/sh", "exit 0"), cli)
  Sys.chmod(cli, "0755")
  fw <- file.path(root, "fw")
  flyw <- file.path(root, "flyw")
  expect_true(file.symlink(cli, fw))
  choose_discovered <- TRUE
  local_mocked_bindings(prompt_input = function(type, ...) {
    if (type == "flag") choose_discovered else flyw
  })
  cfg <- structure(list(metadata = list(project_directory = root)), class = "bg_project_cfg")
  legacy <- setup_compute_environment(cfg, fields = "compute_environment/flywheel")
  expect_identical(legacy$compute_environment$flywheel, fw)
  expect_true(file.symlink(cli, flyw))
  modern <- setup_compute_environment(cfg, fields = "compute_environment/flywheel")
  modern <- resolve_project_paths(modern)
  expect_identical(modern$compute_environment$flywheel, flyw)
  expect_true(uses_flywheel_export(modern))
  payload <- project_config_payload(modern, file.path(root, "project_config.yaml"))
  expect_identical(basename(payload$compute_environment$flywheel), "flyw")
  choose_discovered <- FALSE
  edited <- setup_compute_environment(legacy, fields = "compute_environment/flywheel")
  expect_identical(edited$compute_environment$flywheel, flyw)
})

test_that("Slurm and PBS workers run the selected Flywheel CLI with literal arguments", {
  skip_on_os("windows")
  root <- withr::local_tempdir()
  withr::local_dir(root)
  helpers <- dirname(system.file("shell_functions", package = "BrainGnomes", mustWork = TRUE))
  captured <- NULL
  local_mocked_bindings(
    get_job_sched_args = function(...) "",
    cluster_job_submit = function(env_variables, ...) {
      captured <<- env_variables
      structure("123", cmd = "mock scheduler")
    }
  )
  cfg <- list(
    metadata = list(log_directory = root, flywheel_sync_directory = file.path(root, "Dicom files"),
                    flywheel_temp_directory = file.path(root, "Temp files")),
    flywheel_sync = list(source_url = "fw://group/Study A, B/", save_audit_logs = TRUE,
                         nhours = 1, memgb = 4, ncores = 1)
  )
  # Capture status updates from the production worker without touching a tracking database.
  dir.create("bin")
  writeLines(c("#!/bin/sh", 'echo "$@" >> "$job_updates"'), "bin/Rscript")
  Sys.chmod("bin/Rscript", "0755")
  for (scheduler in c("slurm", "torque")) {
    for (cli in c("fw", "flyw")) {
      cfg$compute_environment <- list(flywheel = file.path(root, cli), scheduler = scheduler)
      writeLines(c("#!/bin/sh", 'printf "%s\\n" "$@" > "$cli_args"', 'exit "$cli_status"'), cli)
      Sys.chmod(cli, "0755")
      custom_rules <- cli == "flyw" && scheduler == "torque"
      cfg$flywheel_sync$cli_options <- if (custom_rules) '--rule-set "/custom rules/dicom.yaml"' else ""
      submit_flywheel_sync(cfg, lg = lgr::get_logger("flywheel_test"))
      expect_false(grepl("[,\n]", captured[["flywheel_argv"]]))
      env <- as.list(captured)
      env$pkg_dir <- helpers
      env$R_HOME <- root
      env$PBS_O_WORKDIR <- root
      env$job_updates <- file.path(root, "status")
      env$cli_args <- file.path(root, "args")
      env$cli_status <- if (custom_rules) "7" else "0"
      script <- file.path(helpers, "hpc_scripts", if (scheduler == "slurm") "flywheel_sync.sbatch" else "flywheel_sync.pbs")
      status <- withr::with_envvar(unlist(env), system2("bash", shQuote(script), stdout = FALSE, stderr = FALSE))
      expect_equal(status, as.integer(env$cli_status))
      updates <- readLines(env$job_updates)
      expect_match(tail(updates, 1L), if (custom_rules) "FAILED --cascade" else "COMPLETED")
      actual <- readLines(env$cli_args)
      if (cli == "fw") {
        expect_match(actual[7], "--save-audit-logs", fixed = TRUE)
        expect_match(actual[8], "flywheel_sync_audit_.*[.]csv$")
        expect_identical(actual[-c(7, 8)], c("sync", "--include", "dicom", "-y", "--tmp-path",
          cfg$metadata$flywheel_temp_directory, cfg$flywheel_sync$source_url, cfg$metadata$flywheel_sync_directory))
      } else {
        rule <- if (custom_rules) "/custom rules/dicom.yaml" else file.path(helpers, "extdata", "flywheel_dicom.yaml")
        # Custom and packaged rules can appear at different positions in the command.
        pos <- match("--rule-set", actual)
        expect_identical(actual[pos + 1L], rule)
        expect_identical(actual[-c(pos, pos + 1L)], c("--no-interactive", "export", "run", "--wait", "-p", "fw://group/Study A, B",
                                                      "-s", cfg$metadata$flywheel_sync_directory))
      }
    }
  }
})
