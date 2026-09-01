# Shared lifecycle APIs used by the R interface and installed command-line tool.

supported_project_steps <- function() {
  c(
    "flywheel_sync", "bids_conversion", "mriqc", "fmriprep",
    "aroma", "postprocess", "extract_rois"
  )
}

project_config_from_input <- function(input) {
  if (inherits(input, "bg_project_cfg")) return(input)
  if (checkmate::test_string(input)) return(load_project(input, validate = FALSE))
  stop("input must be a bg_project_cfg object, YAML file, or project directory", call. = FALSE)
}

empty_issue_df <- function() {
  data.frame(
    severity = character(), code = character(), field = character(),
    message = character(), stringsAsFactors = FALSE
  )
}

#' Validate a BrainGnomes project configuration without changing it
#'
#' This optional inspection entry point is useful for scripts, continuous
#' integration, and configuration review. It is not required before
#' [run_project()], which retains its selected-stage checks. Unlike the historical
#' repair path in `validate_project()`, this function never opens the setup wizard
#' and never writes the configuration.
#'
#' @param input A project configuration object, YAML file, or project directory.
#' @param quiet Suppress the printed validation summary.
#' @return A `bg_project_validation` object containing `valid`, `issues`,
#'   `messages`, and the parsed `config`.
#' @export
validate_project_config <- function(input, quiet = FALSE) {
  checkmate::assert_flag(quiet)

  config_error <- NULL
  scfg <- tryCatch(
    project_config_from_input(input),
    error = function(e) {
      config_error <<- conditionMessage(e)
      NULL
    }
  )

  if (is.null(scfg)) {
    result <- structure(list(
      valid = FALSE,
      issues = data.frame(
        severity = "error", code = "config_unreadable", field = NA_character_,
        message = config_error, stringsAsFactors = FALSE
      ),
      messages = config_error,
      config = NULL
    ), class = "bg_project_validation")
    if (!quiet) print(result)
    return(result)
  }

  validation_error <- NULL
  messages <- utils::capture.output(
    valid <- tryCatch(
      validate_project(scfg, quiet = TRUE, correct_problems = FALSE),
      error = function(e) {
        validation_error <<- conditionMessage(e)
        FALSE
      }
    ),
    type = "message"
  )

  gaps <- unique(attr(valid, "gaps"))
  gaps <- gaps[!is.na(gaps) & nzchar(gaps)]
  issues <- empty_issue_df()
  if (length(gaps) > 0L) {
    issues <- data.frame(
      severity = rep("error", length(gaps)),
      code = rep("missing_or_invalid", length(gaps)),
      field = gaps,
      message = paste0("Missing or invalid configuration field: ", gaps),
      stringsAsFactors = FALSE
    )
  }
  if (!is.null(validation_error)) {
    issues <- rbind(issues, data.frame(
      severity = "error", code = "validation_error", field = NA_character_,
      message = validation_error, stringsAsFactors = FALSE
    ))
  }

  result <- structure(list(
    valid = isTRUE(valid) && nrow(issues) == 0L,
    issues = issues,
    messages = unique(messages[nzchar(messages)]),
    config = scfg
  ), class = "bg_project_validation")
  if (!quiet) print(result)
  result
}

#' @export
print.bg_project_validation <- function(x, ...) {
  if (isTRUE(x$valid)) {
    cli::cli_alert_success("Project configuration is valid.")
  } else {
    cli::cli_alert_danger("Project configuration is invalid ({nrow(x$issues)} issue{?s}).")
    if (nrow(x$issues) > 0L) print(x$issues, row.names = FALSE)
  }
  invisible(x)
}

doctor_check_df <- function() {
  data.frame(
    category = character(), check = character(), status = character(),
    detail = character(), remedy = character(), stringsAsFactors = FALSE
  )
}

#' Run non-mutating project and runtime preflight checks
#'
#' This optional comprehensive preflight is useful on a new cluster, after the
#' submission environment changes, or before an expensive run. `doctor_project()`
#' checks configuration, scheduler commands, container runtime, enabled-stage
#' files, project storage, and the job-tracking database. It is not required
#' before [run_project()] and does not submit work, create directories, or modify
#' the configuration.
#'
#' @param input A project configuration object, YAML file, or project directory.
#' @param steps Optional stages to check. By default all enabled stages are used.
#' @param deep Also initialize Python and check optional postprocessing modules.
#' @param quiet Suppress the printed report.
#' @return A `bg_project_doctor` object with an `ok` flag and a checks data frame.
#' @export
doctor_project <- function(input, steps = NULL, deep = FALSE, quiet = FALSE) {
  checkmate::assert_character(steps, null.ok = TRUE)
  checkmate::assert_flag(deep)
  checkmate::assert_flag(quiet)

  validation <- validate_project_config(input, quiet = TRUE)
  scfg <- validation$config
  checks <- doctor_check_df()
  add_check <- function(category, check, status, detail, remedy = "") {
    checks[nrow(checks) + 1L, ] <<- list(category, check, status, detail, remedy)
  }

  add_check(
    "configuration", "project_config", if (validation$valid) "pass" else "fail",
    if (validation$valid) "Configuration schema and enabled-stage requirements are valid." else
      paste(nrow(validation$issues), "configuration issue(s) found."),
    if (validation$valid) "" else "Run `BrainGnomes config edit <project>` and validate again."
  )

  if (is.null(scfg)) {
    result <- structure(list(ok = FALSE, checks = checks, validation = validation),
      class = "bg_project_doctor")
    if (!quiet) print(result)
    return(result)
  }

  requested_error <- NULL
  if (is.null(steps)) {
    requested <- supported_project_steps()
    requested <- requested[vapply(
      requested, function(step) isTRUE(scfg[[step]]$enable), logical(1)
    )]
    if (length(requested) == 0L) {
      add_check(
        "configuration", "enabled_steps", "warn",
        "No processing stages are enabled; stage-specific runtime checks were skipped.",
        "Enable and configure at least one stage before planning a run."
      )
    }
  } else {
    requested <- tryCatch(
      resolve_project_steps(scfg, steps),
      error = function(e) {
        requested_error <<- conditionMessage(e)
        character()
      }
    )
  }
  if (!is.null(requested_error)) {
    add_check(
      "configuration", "requested_steps", "fail", requested_error,
      "Enable the requested stages or choose from the stages enabled in the project configuration."
    )
  }
  scheduler <- scfg$compute_environment$scheduler
  scheduler_commands <- switch(
    scheduler,
    slurm = c("sbatch", "squeue", "sacct", "scancel"),
    torque = c("qsub", "qstat", "qselect", "qdel"),
    sh = character(), local = character(), character()
  )
  if (length(scheduler_commands) == 0L && !scheduler %in% c("sh", "local")) {
    add_check("scheduler", "scheduler", "fail", paste0("Unsupported scheduler: ", scheduler),
      "Set compute_environment/scheduler to slurm or torque.")
  }
  for (command in scheduler_commands) {
    command_path <- Sys.which(command)
    add_check(
      "scheduler", command, if (nzchar(command_path)) "pass" else "fail",
      if (nzchar(command_path)) unname(command_path) else paste(command, "was not found on PATH."),
      if (nzchar(command_path)) "" else paste("Load or install the scheduler client providing", command, "on the submission host.")
    )
  }

  container_steps <- intersect(requested, c("bids_conversion", "mriqc", "fmriprep", "aroma", "postprocess"))
  if (length(container_steps) > 0L) {
    singularity <- Sys.which("singularity")
    apptainer <- Sys.which("apptainer")
    runtime_status <- if (nzchar(singularity)) "pass" else if (nzchar(apptainer)) "warn" else "fail"
    runtime_detail <- if (nzchar(singularity)) unname(singularity) else if (nzchar(apptainer)) {
      paste0(unname(apptainer), " is available, but BrainGnomes workers invoke `singularity`.")
    } else "Neither singularity nor apptainer was found on PATH."
    add_check(
      "container", "runtime", runtime_status, runtime_detail,
      if (runtime_status == "pass") "" else "Provide a `singularity` compatibility command on compute nodes."
    )
  }

  path_checks <- list(
    project_directory = scfg$metadata$project_directory,
    log_directory = scfg$metadata$log_directory,
    scratch_directory = scfg$metadata$scratch_directory,
    bids_directory = scfg$metadata$bids_directory
  )
  for (label in names(path_checks)) {
    path <- path_checks[[label]]
    exists <- checkmate::test_directory_exists(path)
    writable <- exists && file.access(path, 2L) == 0L
    status <- if (!exists) "fail" else if (!writable) "fail" else "pass"
    detail <- if (!exists) {
      paste0(value_or_default(path, "<unset>"), " does not exist.")
    } else if (!writable) {
      paste0(path, " is not writable.")
    } else normalizePath(path, winslash = "/", mustWork = TRUE)
    add_check("storage", label, status, detail,
      if (status == "pass") "" else "Create the directory or correct ownership/permissions before submission.")
  }

  stage_files <- list(
    flywheel_sync = c(flywheel = scfg$compute_environment$flywheel),
    bids_conversion = c(
      heudiconv_container = scfg$compute_environment$heudiconv_container,
      heuristic_file = scfg$bids_conversion$heuristic_file
    ),
    mriqc = c(mriqc_container = scfg$compute_environment$mriqc_container),
    fmriprep = c(
      fmriprep_container = scfg$compute_environment$fmriprep_container,
      fs_license_file = scfg$fmriprep$fs_license_file
    ),
    aroma = c(aroma_container = scfg$compute_environment$aroma_container),
    postprocess = c(fsl_container = scfg$compute_environment$fsl_container)
  )
  for (stage in intersect(names(stage_files), requested)) {
    for (label in names(stage_files[[stage]])) {
      path <- unname(stage_files[[stage]][label])
      readable <- checkmate::test_file_exists(path) && file.access(path, 4L) == 0L
      add_check(
        stage, label, if (readable) "pass" else "fail",
        if (readable) {
          normalizePath(path, winslash = "/", mustWork = TRUE)
        } else {
          paste0(value_or_default(path, "<unset>"), " is missing or unreadable.")
        },
        if (readable) "" else paste("Configure a readable", label, "for", stage, ".")
      )
    }
  }

  sqlite_db <- scfg$metadata$sqlite_db
  if (checkmate::test_string(sqlite_db)) {
    db_parent <- dirname(sqlite_db)
    parent_ok <- dir.exists(db_parent) && file.access(db_parent, 2L) == 0L
    db_ok <- if (file.exists(sqlite_db)) {
      tryCatch({
        con <- DBI::dbConnect(RSQLite::SQLite(), sqlite_db)
        on.exit(DBI::dbDisconnect(con), add = TRUE)
        DBI::dbGetQuery(con, "PRAGMA quick_check")[[1L]][1L] == "ok"
      }, error = function(e) FALSE)
    } else parent_ok
    add_check(
      "tracking", "sqlite", if (db_ok) "pass" else "fail",
      if (file.exists(sqlite_db)) sqlite_db else paste(sqlite_db, "will be created when jobs are submitted."),
      if (db_ok) "" else "Correct the database path, parent permissions, or SQLite integrity problem."
    )
  } else {
    add_check("tracking", "sqlite", "fail", "metadata/sqlite_db is not configured.",
      "Set metadata/sqlite_db to a writable project path.")
  }

  if (deep && "postprocess" %in% requested) {
    for (module in c("nibabel", "nilearn", "templateflow")) {
      available <- tryCatch(reticulate::py_module_available(module), error = function(e) FALSE)
      add_check(
        "python", module, if (available) "pass" else "warn",
        if (available) paste(module, "is importable.") else paste(module, "is not importable in the active reticulate Python."),
        if (available) "" else paste("Install", module, "when using template-dependent postprocessing.")
      )
    }
  }

  result <- structure(list(
    ok = !any(checks$status == "fail"), checks = checks,
    validation = validation, checked_steps = requested
  ), class = "bg_project_doctor")
  if (!quiet) print(result)
  result
}

#' Project preflight shorthand
#'
#' @inheritParams doctor_project
#' @return A `bg_project_doctor` object.
#' @export
doctor <- function(input, steps = NULL, deep = FALSE, quiet = FALSE) {
  doctor_project(input = input, steps = steps, deep = deep, quiet = quiet)
}

#' @export
print.bg_project_doctor <- function(x, ...) {
  print(x$checks, row.names = FALSE)
  counts <- table(factor(x$checks$status, levels = c("pass", "warn", "fail")))
  if (isTRUE(x$ok)) {
    cli::cli_alert_success("Preflight passed: {counts[['pass']]} passed, {counts[['warn']]} warning{?s}.")
  } else {
    cli::cli_alert_danger("Preflight failed: {counts[['fail']]} failed, {counts[['warn']]} warning{?s}.")
  }
  invisible(x)
}

value_or_default <- function(x, y) {
  if (is.null(x) || length(x) == 0L || is.na(x[1L]) || !nzchar(as.character(x[1L]))) y else x
}

#' Initialize a BrainGnomes project interactively or from portable defaults
#'
#' @param project_name Project label.
#' @param project_directory Project root directory.
#' @param template Optional configuration object or YAML file to use as a base.
#' @param interactive Launch the existing guided setup. When false, missing paths
#'   are populated beneath `project_directory` and all pipeline stages default to
#'   disabled.
#' @param overwrite Replace an existing `project_config.yaml` in non-interactive mode.
#' @return A `bg_project_cfg` object.
#' @export
initialize_project <- function(project_name, project_directory, template = NULL,
                               interactive = base::interactive(), overwrite = FALSE) {
  checkmate::assert_string(project_name)
  checkmate::assert_string(project_directory)
  checkmate::assert_flag(interactive)
  checkmate::assert_flag(overwrite)
  project_directory <- normalizePath(path.expand(project_directory), winslash = "/", mustWork = FALSE)

  if (interactive) {
    scfg <- if (is.null(template)) list(metadata = list()) else project_config_from_input(template)
    scfg$metadata$project_name <- project_name
    scfg$metadata$project_directory <- project_directory
    class(scfg) <- unique(c("bg_project_cfg", class(scfg)))
    return(setup_project(scfg))
  }

  dir.create(project_directory, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(project_directory)) stop("Failed to create project directory: ", project_directory, call. = FALSE)
  scfg <- if (is.null(template)) list() else as.list(project_config_from_input(template))
  scfg$schema_version <- value_or_default(scfg$schema_version, 1L)
  if (is.null(scfg$metadata)) scfg$metadata <- list()
  scfg$metadata$project_name <- project_name
  scfg$metadata$project_directory <- project_directory
  default_dirs <- c(
    dicom_directory = "data_dicoms", bids_directory = "data_bids",
    fmriprep_directory = "data_fmriprep", mriqc_directory = "data_mriqc",
    postproc_directory = "data_postproc", rois_directory = "data_rois",
    log_directory = "logs", scratch_directory = "scratch",
    templateflow_home = "templateflow"
  )
  for (field in names(default_dirs)) {
    if (!checkmate::test_string(scfg$metadata[[field]])) {
      scfg$metadata[[field]] <- file.path(project_directory, default_dirs[[field]])
    }
    dir.create(scfg$metadata[[field]], recursive = TRUE, showWarnings = FALSE)
  }
  scfg$metadata$sqlite_db <- value_or_default(
    scfg$metadata$sqlite_db,
    file.path(project_directory, paste0(project_name, ".sqlite"))
  )
  if (is.null(scfg$compute_environment)) scfg$compute_environment <- list()
  if (!checkmate::test_string(scfg$compute_environment$scheduler)) {
    scfg$compute_environment$scheduler <- if (nzchar(Sys.which("qsub")) && !nzchar(Sys.which("sbatch"))) "torque" else "slurm"
  }
  for (stage in c(supported_project_steps(), "bids_validation")) {
    if (is.null(scfg[[stage]])) scfg[[stage]] <- list()
    if (!checkmate::test_flag(scfg[[stage]]$enable)) scfg[[stage]]$enable <- FALSE
  }
  class(scfg) <- unique(c("bg_project_cfg", class(scfg)))
  scfg <- write_project_config(scfg, overwrite = overwrite)
  scfg
}

#' Write a project configuration without interactive prompts
#'
#' @param input A `bg_project_cfg` object.
#' @param file Destination YAML path. Defaults to `project_config.yaml` beneath
#'   the configured project directory.
#' @param overwrite Replace an existing file.
#' @return The configuration, invisibly, with its `yaml_file` attribute set.
#' @export
write_project_config <- function(input, file = NULL, overwrite = FALSE) {
  scfg <- project_config_from_input(input)
  checkmate::assert_flag(overwrite)
  if (is.null(file)) file <- file.path(scfg$metadata$project_directory, "project_config.yaml")
  checkmate::assert_string(file)
  file <- path.expand(file)
  if (file.exists(file) && !overwrite) stop("Configuration file already exists: ", file, call. = FALSE)
  if (!dir.exists(dirname(file))) stop("Configuration directory does not exist: ", dirname(file), call. = FALSE)
  temp <- tempfile("project-config-", tmpdir = dirname(file), fileext = ".yaml")
  on.exit(if (file.exists(temp)) unlink(temp), add = TRUE)
  payload <- as.list(scfg)
  payload$schema_version <- value_or_default(payload$schema_version, 1L)
  yaml::write_yaml(payload, temp)
  if (!file.rename(temp, file)) stop("Failed to atomically write configuration: ", file, call. = FALSE)
  attr(scfg, "yaml_file") <- normalizePath(file, winslash = "/", mustWork = TRUE)
  invisible(scfg)
}

resolve_project_steps <- function(scfg, steps = NULL) {
  supported <- supported_project_steps()
  if (is.null(steps)) {
    steps <- supported[vapply(supported, function(step) isTRUE(scfg[[step]]$enable), logical(1))]
  }
  steps <- unique(tolower(trimws(as.character(steps))))
  steps <- steps[!is.na(steps) & nzchar(steps)]
  if ("all" %in% steps) {
    steps <- supported[vapply(supported, function(step) isTRUE(scfg[[step]]$enable), logical(1))]
  }
  unknown <- setdiff(steps, supported)
  if (length(unknown) > 0L) {
    stop("Unknown processing step(s): ", paste(unknown, collapse = ", "), call. = FALSE)
  }
  disabled <- steps[!vapply(steps, function(step) isTRUE(scfg[[step]]$enable), logical(1))]
  if (length(disabled) > 0L) {
    stop("Requested step(s) are disabled: ", paste(disabled, collapse = ", "), call. = FALSE)
  }
  if (length(steps) == 0L) stop("No enabled processing steps were requested.", call. = FALSE)
  steps
}

discover_project_subjects <- function(scfg, steps, subject_filter = NULL, allow_empty = FALSE) {
  requested_subjects <- if (is.data.frame(subject_filter)) {
    as.character(subject_filter$sub_id)
  } else if (!is.null(subject_filter)) {
    as.character(subject_filter)
  } else {
    NULL
  }
  dicom <- data.frame(
    sub_id = character(), ses_id = character(), dicom_sub_dir = character(),
    dicom_ses_dir = character(), stringsAsFactors = FALSE
  )
  if ("bids_conversion" %in% steps && dir.exists(scfg$metadata$dicom_directory)) {
    dicom <- get_subject_dirs(
      scfg$metadata$dicom_directory,
      sub_regex = scfg$bids_conversion$sub_regex,
      sub_id_match = scfg$bids_conversion$sub_id_match,
      ses_regex = scfg$bids_conversion$ses_regex,
      ses_id_match = scfg$bids_conversion$ses_id_match,
      full.names = TRUE,
      subject_filter = requested_subjects
    )
    names(dicom) <- sub("(sub|ses)_dir", "dicom_\\1_dir", names(dicom))
  }

  bids <- data.frame(
    sub_id = character(), ses_id = character(), bids_sub_dir = character(),
    bids_ses_dir = character(), stringsAsFactors = FALSE
  )
  if (dir.exists(scfg$metadata$bids_directory)) {
    bids <- get_subject_dirs(
      scfg$metadata$bids_directory,
      sub_regex = "^sub-.+", ses_regex = "^ses-.+",
      sub_id_match = "sub-(.*)", ses_id_match = "ses-(.*)", full.names = TRUE,
      subject_filter = requested_subjects
    )
    names(bids) <- sub("(sub|ses)_dir", "bids_\\1_dir", names(bids))
  }

  subjects <- merge(dicom, bids, by = c("sub_id", "ses_id"), all = TRUE)
  if (!is.null(subject_filter)) {
    if (is.data.frame(subject_filter)) {
      checkmate::assert_names(names(subject_filter), must.include = "sub_id")
      by_cols <- intersect(c("sub_id", "ses_id"), names(subject_filter))
      subjects <- merge(subjects, subject_filter[, by_cols, drop = FALSE], by = by_cols)
    } else {
      subjects <- subjects[subjects$sub_id %in% as.character(subject_filter), , drop = FALSE]
    }
  }
  if (nrow(subjects) == 0L && !allow_empty) {
    stop("No subject/session inputs match the requested run.", call. = FALSE)
  }
  subjects
}

resolve_project_selection <- function(scfg, steps, postprocess_streams = NULL,
                                      extract_streams = NULL, force = FALSE) {
  checkmate::assert_class(scfg, "bg_project_cfg")
  checkmate::assert_character(steps, null.ok = TRUE)
  checkmate::assert_character(postprocess_streams, null.ok = TRUE)
  checkmate::assert_character(extract_streams, null.ok = TRUE)
  checkmate::assert_flag(force)

  resolved_steps <- resolve_project_steps(scfg, steps)
  available_postprocess <- get_postprocess_stream_names(scfg)
  available_extract <- get_extract_stream_names(scfg)
  checkmate::assert_subset(
    postprocess_streams, available_postprocess, empty.ok = TRUE
  )
  checkmate::assert_subset(
    extract_streams, available_extract, empty.ok = TRUE
  )

  if ("postprocess" %in% resolved_steps) {
    if (length(available_postprocess) == 0L) {
      stop(
        "Cannot run postprocessing without at least one postprocess configuration.",
        call. = FALSE
      )
    }
    if (is.null(postprocess_streams)) {
      postprocess_streams <- available_postprocess
    }
  } else {
    postprocess_streams <- character()
  }

  if ("extract_rois" %in% resolved_steps) {
    if (length(available_extract) == 0L) {
      stop(
        "Cannot run extraction without at least one extract_rois configuration.",
        call. = FALSE
      )
    }
    if (is.null(extract_streams)) extract_streams <- available_extract
  } else {
    extract_streams <- character()
  }

  step_flags <- stats::setNames(
    supported_project_steps() %in% resolved_steps,
    supported_project_steps()
  )
  structure(list(
    steps = resolved_steps,
    step_flags = step_flags,
    postprocess_streams = postprocess_streams,
    extract_streams = extract_streams,
    force = force
  ), class = "bg_project_selection")
}

resolve_project_execution <- function(scfg, steps, subject_filter = NULL,
                                      postprocess_streams = NULL,
                                      extract_streams = NULL, force = FALSE) {
  checkmate::assert(
    checkmate::check_character(
      subject_filter, any.missing = FALSE, null.ok = TRUE
    ),
    checkmate::check_data_frame(subject_filter, null.ok = TRUE)
  )
  selection <- resolve_project_selection(
    scfg, steps, postprocess_streams, extract_streams, force
  )
  subject_steps <- setdiff(selection$steps, "flywheel_sync")
  scope_deferred <-
    "flywheel_sync" %in% selection$steps && length(subject_steps) > 0L
  subjects <- if (length(subject_steps) > 0L) {
    discover_project_subjects(
      scfg, selection$steps, subject_filter,
      allow_empty = scope_deferred
    )
  } else {
    data.frame(
      sub_id = character(), ses_id = character(),
      dicom_sub_dir = character(), dicom_ses_dir = character(),
      bids_sub_dir = character(), bids_ses_dir = character(),
      stringsAsFactors = FALSE
    )
  }

  structure(c(unclass(selection), list(
    subject_filter = subject_filter,
    subjects = subjects,
    scope_deferred = scope_deferred
  )), class = "bg_project_execution")
}

stage_resource <- function(scfg, stage, stream = NA_character_) {
  cfg <- if (stage == "fsaverage_setup") {
    list(ncores = 1, memgb = 8, nhours = 0.15)
  } else if (stage == "prefetch_templates") {
    list(ncores = 1, memgb = 16, nhours = 0.5)
  } else if (stage == "postprocess" && !is.na(stream)) {
    scfg$postprocess[[stream]]
  } else if (stage == "extract_rois" && !is.na(stream)) {
    scfg$extract_rois[[stream]]
  } else scfg[[stage]]
  c(
    ncores = as.numeric(value_or_default(cfg$ncores, NA_real_)),
    memgb = as.numeric(value_or_default(cfg$memgb, NA_real_)),
    nhours = as.numeric(value_or_default(cfg$nhours, NA_real_))
  )
}

build_project_jobs <- function(scfg, execution) {
  checkmate::assert_class(scfg, "bg_project_cfg")
  checkmate::assert_class(execution, "bg_project_execution")
  resolved_steps <- execution$steps
  postprocess_streams <- execution$postprocess_streams
  extract_streams <- execution$extract_streams
  subjects <- execution$subjects
  n_subjects <- length(unique(subjects$sub_id))
  n_sessions <- nrow(subjects)
  scope_count <- function(stage) {
    if (execution$scope_deferred && stage != "flywheel_sync") {
      return(NA_integer_)
    }
    if (stage %in% c("mriqc", "fmriprep", "aroma")) n_subjects else n_sessions
  }

  jobs <- data.frame(
    stage = character(), stream = character(), scope = character(),
    n_jobs = integer(), depends_on = character(), ncores = numeric(),
    memgb = numeric(), nhours = numeric(), stringsAsFactors = FALSE
  )
  add_job <- function(stage, stream = NA_character_, scope, n_jobs,
                      depends_on = "") {
    resource <- stage_resource(scfg, stage, stream)
    jobs[nrow(jobs) + 1L, ] <<- list(
      stage, stream, scope, as.integer(n_jobs), depends_on,
      resource[["ncores"]], resource[["memgb"]], resource[["nhours"]]
    )
  }
  if ("flywheel_sync" %in% resolved_steps) {
    add_job("flywheel_sync", scope = "project", n_jobs = 1L)
  }
  if ("fmriprep" %in% resolved_steps) {
    add_job("fsaverage_setup", scope = "project", n_jobs = 1L)
  }
  if (length(intersect(
    resolved_steps, c("mriqc", "fmriprep", "aroma")
  )) > 0L) {
    add_job("prefetch_templates", scope = "project", n_jobs = 1L)
  }
  for (stage in intersect(
    c("bids_conversion", "mriqc", "fmriprep", "aroma"), resolved_steps
  )) {
    dependency_candidates <- switch(stage,
      bids_conversion = if ("flywheel_sync" %in% resolved_steps) {
        "flywheel_sync"
      } else "",
      mriqc = "bids_conversion",
      fmriprep = "bids_conversion",
      aroma = "fmriprep"
    )
    dependency <- paste(
      intersect(dependency_candidates, resolved_steps), collapse = ","
    )
    if (stage %in% c("mriqc", "fmriprep", "aroma")) {
      dependency <- paste(
        c(dependency, "prefetch_templates")[
          nzchar(c(dependency, "prefetch_templates"))
        ],
        collapse = ","
      )
    }
    if (stage == "fmriprep") {
      dependency <- paste(
        c(dependency, "fsaverage_setup")[
          nzchar(c(dependency, "fsaverage_setup"))
        ],
        collapse = ","
      )
    }
    add_job(
      stage,
      scope = if (stage == "bids_conversion") "session" else "subject",
      n_jobs = scope_count(stage),
      depends_on = dependency
    )
  }
  if ("postprocess" %in% resolved_steps) {
    for (stream in postprocess_streams) {
      deps <- intersect(c("fmriprep", "aroma"), resolved_steps)
      add_job(
        "postprocess", stream, "session", scope_count("postprocess"),
        paste(deps, collapse = ",")
      )
    }
  }
  if ("extract_rois" %in% resolved_steps) {
    for (stream in extract_streams) {
      deps <- intersect("postprocess", resolved_steps)
      add_job(
        "extract_rois", stream, "session", scope_count("extract_rois"),
        paste(deps, collapse = ",")
      )
    }
  }
  jobs
}

#' Inspect or persist the resolved project execution model
#'
#' `plan_project()` is optional inspection and automation tooling. It exposes the
#' stages, streams, subject/session scope, resources, dependencies, and implicit
#' setup work resolved for a request. [run_project()] resolves the same execution
#' model internally, so creating or submitting a plan is not required for a
#' direct run.
#'
#' @param input A project configuration object, YAML file, or project directory.
#' @param steps Pipeline stages or `"all"`.
#' @param subject_filter Optional subject IDs or a data frame with `sub_id` and
#'   optionally `ses_id`.
#' @param postprocess_streams Optional postprocessing streams.
#' @param extract_streams Optional ROI-extraction streams.
#' @param force Include work whose completion markers would otherwise skip it.
#' @param allow_invalid Build the plan despite configuration validation errors.
#' @param quiet Suppress the printed plan.
#' @return A serializable `bg_project_plan` object.
#' @seealso [run_project()] for the standard direct execution path.
#' @export
plan_project <- function(input, steps = "all", subject_filter = NULL,
                         postprocess_streams = NULL, extract_streams = NULL,
                         force = FALSE, allow_invalid = FALSE, quiet = FALSE) {
  checkmate::assert_flag(force)
  checkmate::assert_flag(allow_invalid)
  checkmate::assert_flag(quiet)
  validation <- validate_project_config(input, quiet = TRUE)
  if (!validation$valid && !allow_invalid) {
    stop("Project configuration is invalid. Run validate_project_config() or `BrainGnomes config validate` for details.", call. = FALSE)
  }
  scfg <- validation$config
  execution <- resolve_project_execution(
    scfg, steps, subject_filter, postprocess_streams, extract_streams, force
  )
  resolved_steps <- execution$steps
  postprocess_streams <- execution$postprocess_streams
  extract_streams <- execution$extract_streams
  subjects <- execution$subjects
  jobs <- build_project_jobs(scfg, execution)

  result <- structure(list(
    schema_version = "brain-gnomes-plan-v1",
    plan_id = uuid::UUIDgenerate(),
    created_at = as.character(Sys.time()),
    config_file = attr(scfg, "yaml_file"),
    config = scfg,
    validation = validation[c("valid", "issues", "messages")],
    request = list(
      steps = resolved_steps, subject_filter = execution$subject_filter,
      postprocess_streams = postprocess_streams, extract_streams = extract_streams,
      force = force
    ),
    subjects = subjects,
    scope_deferred = execution$scope_deferred,
    jobs = jobs
  ), class = "bg_project_plan")
  if (!quiet) print(result)
  result
}

#' @export
print.bg_project_plan <- function(x, ...) {
  cli::cli_h2("BrainGnomes execution plan {.val {x$plan_id}}")
  cli::cli_text("Steps: {paste(x$request$steps, collapse = ', ')}")
  if (isTRUE(x$scope_deferred)) {
    cli::cli_alert_info("Subject discovery is deferred until Flywheel synchronization completes.")
  } else {
    cli::cli_text("Scope: {length(unique(x$subjects$sub_id))} subject{?s}, {nrow(x$subjects)} subject/session row{?s}.")
  }
  print(x$jobs, row.names = FALSE)
  invisible(x)
}

#' Save an execution plan to YAML
#' @param plan A `bg_project_plan` object.
#' @param file Destination YAML path.
#' @param overwrite Replace an existing file.
#' @return The normalized output path, invisibly.
#' @export
write_project_plan <- function(plan, file, overwrite = FALSE) {
  checkmate::assert_class(plan, "bg_project_plan")
  checkmate::assert_string(file)
  checkmate::assert_flag(overwrite)
  if (file.exists(file) && !overwrite) stop("Plan file already exists: ", file, call. = FALSE)
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  temp <- tempfile("project-plan-", tmpdir = dirname(file), fileext = ".yaml")
  on.exit(if (file.exists(temp)) unlink(temp), add = TRUE)
  payload <- unclass(plan)
  payload$config <- as.list(plan$config)
  yaml::write_yaml(payload, temp)
  if (!file.rename(temp, file)) stop("Failed to atomically write plan file: ", file, call. = FALSE)
  invisible(normalizePath(file, winslash = "/", mustWork = TRUE))
}

#' Read a saved execution plan
#' @param file YAML plan path.
#' @return A `bg_project_plan` object.
#' @export
read_project_plan <- function(file) {
  checkmate::assert_file_exists(file)
  source_file <- normalizePath(file, winslash = "/", mustWork = TRUE)
  plan <- yaml::read_yaml(file)
  if (!identical(plan$schema_version, "brain-gnomes-plan-v1")) {
    stop("Unsupported project plan schema: ", value_or_default(plan$schema_version, "<missing>"), call. = FALSE)
  }
  class(plan$config) <- unique(c("bg_project_cfg", class(plan$config)))
  plan$subjects <- as.data.frame(plan$subjects, stringsAsFactors = FALSE)
  plan$jobs <- as.data.frame(plan$jobs, stringsAsFactors = FALSE)
  class(plan) <- "bg_project_plan"
  attr(plan, "source_file") <- source_file
  plan
}

#' Submit a saved or in-memory execution plan
#' @param plan A `bg_project_plan` object or YAML plan path.
#' @param debug Enable debug submission mode.
#' @param log_level Pipeline log threshold.
#' @return A `bg_project_run` object returned by [run_project()].
#' @export
submit_project_plan <- function(plan, debug = FALSE, log_level = "INFO") {
  if (checkmate::test_string(plan)) plan <- read_project_plan(plan)
  checkmate::assert_class(plan, "bg_project_plan")
  request <- plan$request
  planned_subjects <- request$subject_filter
  if (is.list(planned_subjects) && !is.data.frame(planned_subjects)) {
    planned_subjects <- if ("sub_id" %in% names(planned_subjects)) {
      as.data.frame(planned_subjects, stringsAsFactors = FALSE)
    } else {
      unlist(planned_subjects, use.names = FALSE)
    }
  }
  if (!isTRUE(plan$scope_deferred) && nrow(plan$subjects) > 0L) {
    subject_columns <- intersect(c("sub_id", "ses_id"), names(plan$subjects))
    planned_subjects <- plan$subjects[, subject_columns, drop = FALSE]
  }
  scfg <- plan$config
  attr(scfg, "provenance_context") <- list(
    interface = if (checkmate::test_string(attr(plan, "source_file"))) {
      "saved_plan"
    } else {
      "in_memory_plan"
    },
    plan_id = plan$plan_id,
    plan_created_at = plan$created_at,
    plan_file = attr(plan, "source_file", exact = TRUE)
  )
  run_project(
    scfg,
    steps = unlist(request$steps, use.names = FALSE),
    subject_filter = planned_subjects,
    postprocess_streams = unlist(request$postprocess_streams, use.names = FALSE),
    extract_streams = unlist(request$extract_streams, use.names = FALSE),
    force = isTRUE(request$force), debug = debug, log_level = log_level
  )
}

new_project_run <- function(scfg, run_id, submitted_ids = NULL, deferred = FALSE,
                            provenance_file = NULL) {
  tracked_ids <- character()
  sqlite_db <- scfg$metadata$sqlite_db
  if (checkmate::test_file_exists(sqlite_db) && sqlite_table_exists(sqlite_db, "job_tracking")) {
    tracked <- tryCatch(
      get_tracked_job_status(sequence_id = run_id, sqlite_db = sqlite_db),
      error = function(e) NULL
    )
    if (is.data.frame(tracked) && nrow(tracked) > 0L) {
      tracked_ids <- as.character(tracked$job_id)
    }
  }
  run <- structure(list(
    run_id = run_id,
    job_ids = unique(c(as.character(submitted_ids), tracked_ids)),
    submitted_at = as.character(Sys.time()),
    deferred_subject_submission = isTRUE(deferred),
    project_directory = scfg$metadata$project_directory,
    sqlite_db = sqlite_db,
    provenance_file = provenance_file
  ), class = "bg_project_run")
  tryCatch(
    update_run_provenance_submission(
      scfg, run_id, submitted_ids = run$job_ids, deferred = deferred
    ),
    error = function(e) warning(
      "Run was submitted, but its provenance record could not be finalized: ",
      conditionMessage(e), call. = FALSE
    )
  )
  run
}

#' @export
print.bg_project_run <- function(x, ...) {
  cli::cli_alert_success("Submitted BrainGnomes run {.val {x$run_id}}.")
  cli::cli_text("Tracked scheduler jobs: {length(x$job_ids)}")
  if (isTRUE(x$deferred_subject_submission)) {
    cli::cli_alert_info("Subject discovery and submission are deferred until Flywheel synchronization completes.")
  }
  if (checkmate::test_string(x$provenance_file)) {
    cli::cli_text("Provenance: {.file {x$provenance_file}}")
  }
  invisible(x)
}

resolve_run_id <- function(scfg, run_id = "latest") {
  if (!identical(run_id, "latest")) return(as.character(run_id))
  provenance_id <- latest_run_provenance_id(scfg)
  if (checkmate::test_string(provenance_id)) return(provenance_id)
  sqlite_db <- scfg$metadata$sqlite_db
  if (!checkmate::test_file_exists(sqlite_db) || !sqlite_table_exists(sqlite_db, "job_tracking")) {
    stop("No job-tracking database is available for this project.", call. = FALSE)
  }
  con <- DBI::dbConnect(RSQLite::SQLite(), sqlite_db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  latest <- DBI::dbGetQuery(con,
    "SELECT sequence_id FROM job_tracking WHERE sequence_id IS NOT NULL ORDER BY datetime(time_submitted) DESC, id DESC LIMIT 1")
  if (nrow(latest) == 0L) stop("No tracked runs were found.", call. = FALSE)
  latest$sequence_id[[1L]]
}

#' List submitted runs for a project
#'
#' A run is one submission from [run_project()] or [retry_project_run()]. Use
#' this function to find the run ID needed by the status, log, diagnosis,
#' provenance, retry, and cancellation functions. The newest run is listed
#' first.
#'
#' @param input A project configuration object, YAML file, or project directory.
#' @return A data frame with one row per run, including its ID, submission and
#'   end times, number of tracked jobs, and overall status.
#' @examples
#' \dontrun{
#' runs <- get_project_runs(scfg)
#' run_id <- runs$run_id[[1L]]
#' }
#' @seealso [get_run_jobs()] to inspect the jobs in one run.
#' @export
get_project_runs <- function(input) {
  scfg <- project_config_from_input(input)
  db <- scfg$metadata$sqlite_db
  if (!checkmate::test_file_exists(db) || !sqlite_table_exists(db, "job_tracking")) {
    return(data.frame(
      run_id = character(), submitted = character(), ended = character(),
      n_jobs = integer(), status = character(), stringsAsFactors = FALSE
    ))
  }
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  jobs <- DBI::dbGetQuery(con, "SELECT sequence_id, status, time_submitted, time_ended FROM job_tracking WHERE sequence_id IS NOT NULL")
  if (nrow(jobs) == 0L) return(data.frame(
    run_id = character(), submitted = character(), ended = character(),
    n_jobs = integer(), status = character(), stringsAsFactors = FALSE
  ))
  split_jobs <- split(jobs, jobs$sequence_id)
  rows <- lapply(split_jobs, function(run_jobs) {
    statuses <- toupper(run_jobs$status)
    overall <- if (any(statuses %in% c("FAILED", "FAILED_BY_EXT"))) "FAILED" else if (any(statuses == "CANCELLED")) {
      "CANCELLED"
    } else if (any(statuses == "STARTED")) "RUNNING" else if (any(statuses == "QUEUED")) {
      "QUEUED"
    } else if (length(statuses) > 0L && all(statuses == "COMPLETED")) "COMPLETED" else "UNKNOWN"
    submitted_values <- run_jobs$time_submitted[!is.na(run_jobs$time_submitted)]
    ended_values <- run_jobs$time_ended[!is.na(run_jobs$time_ended)]
    data.frame(
      run_id = run_jobs$sequence_id[[1L]],
      submitted = if (length(submitted_values) == 0L) NA_character_ else min(submitted_values),
      ended = if (length(ended_values) == 0L) NA_character_ else max(ended_values),
      n_jobs = nrow(run_jobs), status = overall, stringsAsFactors = FALSE
    )
  })
  result <- do.call(rbind, rows)
  result[order(result$submitted, decreasing = TRUE), , drop = FALSE]
}

#' Inspect the jobs submitted for one project run
#'
#' Returns the job-tracking rows for one run. This is useful for checking which
#' processing step and subject each scheduler job belongs to and whether it is
#' queued, running, completed, failed, cancelled, or blocked by an earlier
#' failure.
#'
#' @param input A project configuration object, YAML file, or project directory.
#' @param run_id Run ID returned by [run_project()] or [get_project_runs()]. Use
#'   `"latest"` for the most recently recorded run.
#' @return The tracking rows for the run.
#' @examples
#' \dontrun{
#' jobs <- get_run_jobs(scfg, run$run_id)
#' jobs[, c("job_id", "job_name", "status")]
#' }
#' @seealso [diagnose_project()] for a failure-focused summary and
#'   [find_run_logs()] for log-file locations.
#' @export
get_run_jobs <- function(input, run_id = "latest") {
  scfg <- project_config_from_input(input)
  run_id <- resolve_run_id(scfg, run_id)
  jobs <- get_tracked_job_status(sequence_id = run_id, sqlite_db = scfg$metadata$sqlite_db)
  if (nrow(jobs) > 0L && "job_obj" %in% names(jobs)) jobs$job_obj <- NULL
  jobs
}

#' Find output and error logs for one run
#'
#' Matches tracked job IDs to scheduler output (`.out`) and error (`.err`) files
#' beneath the configured log directory. It returns file locations without
#' opening or changing the logs.
#'
#' @param input A project configuration object, YAML file, or project directory.
#' @param run_id Run ID returned by [run_project()] or [get_project_runs()]. Use
#'   `"latest"` for the most recently recorded run.
#' @param failed_only If `TRUE`, include only failed, cancelled, or downstream
#'   jobs that could not run because an earlier job failed.
#' @return A data frame mapping jobs to stdout/stderr log files.
#' @examples
#' \dontrun{
#' failed_logs <- find_run_logs(scfg, run$run_id, failed_only = TRUE)
#' failed_logs[, c("job_name", "status", "type", "path")]
#' }
#' @seealso [diagnose_project()] for a run summary that includes these logs.
#' @export
find_run_logs <- function(input, run_id = "latest", failed_only = FALSE) {
  checkmate::assert_flag(failed_only)
  scfg <- project_config_from_input(input)
  jobs <- get_run_jobs(scfg, run_id)
  if (failed_only) jobs <- jobs[jobs$status %in% c("FAILED", "FAILED_BY_EXT", "CANCELLED"), , drop = FALSE]
  files <- if (dir.exists(scfg$metadata$log_directory)) {
    list.files(scfg$metadata$log_directory, recursive = TRUE, full.names = TRUE, pattern = "\\.(out|err)$")
  } else character()
  rows <- lapply(seq_len(nrow(jobs)), function(i) {
    job_id <- as.character(jobs$job_id[[i]])
    matches <- files[grepl(job_id, basename(files), fixed = TRUE)]
    if (length(matches) == 0L) return(NULL)
    data.frame(
      run_id = jobs$sequence_id[[i]], job_id = job_id,
      job_name = jobs$job_name[[i]], status = jobs$status[[i]],
      type = ifelse(grepl("\\.err$", matches), "stderr", "stdout"),
      path = matches, stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0L) return(data.frame(
    run_id = character(), job_id = character(), job_name = character(),
    status = character(), type = character(), path = character(), stringsAsFactors = FALSE
  ))
  do.call(rbind, rows)
}

#' Summarize failures and logs for one run without prompts
#'
#' Use this function in scripts, reports, or an R session when you already know
#' which run to inspect. It reports all tracked jobs, separates failed,
#' cancelled, and blocked jobs, and locates their logs. For a guided interactive
#' browser, continue to use [diagnose_pipeline()].
#'
#' @param input A project configuration object, YAML file, or project directory.
#' @param run_id Run ID returned by [run_project()] or [get_project_runs()]. Use
#'   `"latest"` for the most recently recorded run.
#' @return A `bg_project_diagnosis` object containing `jobs`, `failures`, and
#'   matching `logs` for the selected run.
#' @examples
#' \dontrun{
#' diagnosis <- diagnose_project(scfg, run$run_id)
#' diagnosis$failures
#' diagnosis$logs
#' }
#' @seealso [diagnose_pipeline()] for interactive investigation and
#'   [retry_project_run()] to preview a new run after correcting a failure.
#' @export
diagnose_project <- function(input, run_id = "latest") {
  scfg <- project_config_from_input(input)
  resolved <- resolve_run_id(scfg, run_id)
  jobs <- get_run_jobs(scfg, resolved)
  failures <- jobs[jobs$status %in% c("FAILED", "FAILED_BY_EXT", "CANCELLED"), , drop = FALSE]
  structure(list(
    run_id = resolved,
    jobs = jobs,
    failures = failures,
    logs = find_run_logs(scfg, resolved, failed_only = TRUE)
  ), class = "bg_project_diagnosis")
}

#' @export
print.bg_project_diagnosis <- function(x, ...) {
  cli::cli_h2("Run diagnosis {.val {x$run_id}}")
  counts <- as.data.frame(table(x$jobs$status), stringsAsFactors = FALSE)
  names(counts) <- c("status", "n_jobs")
  print(counts, row.names = FALSE)
  if (nrow(x$failures) > 0L) {
    cli::cli_alert_danger("{nrow(x$failures)} failed, blocked, or cancelled job{?s}.")
    print(x$failures[, intersect(c("job_id", "job_name", "status", "time_ended"), names(x$failures)), drop = FALSE], row.names = FALSE)
  } else {
    cli::cli_alert_success("No failed jobs were found.")
  }
  invisible(x)
}

#' Cancel queued or running jobs from one run
#'
#' Cancellation affects only tracked jobs that are still queued or running. It
#' does not delete project data, logs, or completed outputs. Calling this
#' function with `dry_run = FALSE` immediately sends cancellation requests to
#' the configured scheduler, so preview the commands first.
#'
#' @param input A project configuration object, YAML file, or project directory.
#' @param run_id Run ID returned by [run_project()] or [get_project_runs()]. An
#'   explicit ID is recommended for cancellation.
#' @param dry_run If `TRUE`, report the commands without contacting the
#'   scheduler. If `FALSE`, request cancellation immediately.
#' @return A data frame describing each cancellation attempt.
#' @examples
#' \dontrun{
#' cancel_project_run(scfg, run$run_id, dry_run = TRUE)
#' cancel_project_run(scfg, run$run_id, dry_run = FALSE)
#' }
#' @seealso [get_run_jobs()] to inspect current job states before cancellation.
#' @export
cancel_project_run <- function(input, run_id = "latest", dry_run = TRUE) {
  checkmate::assert_flag(dry_run)
  scfg <- project_config_from_input(input)
  resolved <- resolve_run_id(scfg, run_id)
  jobs <- get_run_jobs(scfg, resolved)
  jobs <- jobs[jobs$status %in% c("QUEUED", "STARTED"), , drop = FALSE]
  command <- switch(scfg$compute_environment$scheduler,
    slurm = "scancel", torque = "qdel",
    stop("Cancellation is supported only for slurm and torque projects.", call. = FALSE)
  )
  if (nrow(jobs) == 0L) return(data.frame(
    run_id = character(), job_id = character(), command = character(),
    status = character(), stringsAsFactors = FALSE
  ))
  rows <- lapply(seq_len(nrow(jobs)), function(i) {
    job_id <- as.character(jobs$job_id[[i]])
    exit <- if (dry_run) 0L else system2(command, job_id)
    status <- if (dry_run) "would_cancel" else if (identical(as.integer(exit), 0L)) "cancelled" else "failed"
    if (!dry_run && status == "cancelled") {
      update_tracked_job_status(scfg$metadata$sqlite_db, job_id, "CANCELLED")
    }
    data.frame(run_id = resolved, job_id = job_id,
      command = paste(command, job_id), status = status, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

retry_request_from_jobs <- function(jobs, include_blocked = FALSE) {
  statuses <- c("FAILED", "CANCELLED", if (include_blocked) "FAILED_BY_EXT")
  failed <- jobs[jobs$status %in% statuses, , drop = FALSE]
  names_ <- failed$job_name
  stage <- vapply(names_, function(name) {
    if (grepl("^flywheel_sync", name)) "flywheel_sync"
    else if (grepl("^bids_conversion", name)) "bids_conversion"
    else if (grepl("^mriqc", name)) "mriqc"
    else if (grepl("^fmriprep", name)) "fmriprep"
    else if (grepl("^aroma", name)) "aroma"
    else if (grepl("^postprocess_", name)) "postprocess"
    else if (grepl("^extract_rois_", name)) "extract_rois"
    else NA_character_
  }, character(1))
  keep <- !is.na(stage)
  failed <- failed[keep, , drop = FALSE]
  stage <- stage[keep]
  subjects <- regmatches(failed$job_name, regexpr("(?<=sub-)[^_]+", failed$job_name, perl = TRUE))
  subjects[subjects == ""] <- NA_character_
  stream_from_job_name <- function(job_name, prefix) {
    marker <- paste0(prefix, "_")
    if (!startsWith(job_name, marker)) return("")
    value <- substring(job_name, nchar(marker) + 1L)
    value <- sub("_sub-.*$", "", value)
    if (identical(prefix, "postprocess")) {
      value <- sub("_(sentinel|array)$", "", value)
    }
    if (!nzchar(value)) "" else value
  }
  pp <- vapply(
    failed$job_name, stream_from_job_name, character(1),
    prefix = "postprocess"
  )
  ex <- vapply(
    failed$job_name, stream_from_job_name, character(1),
    prefix = "extract_rois"
  )
  list(
    steps = unique(stage),
    subject_filter = unique(subjects[!is.na(subjects)]),
    postprocess_streams = unique(pp[nzchar(pp)]),
    extract_streams = unique(ex[nzchar(ex)]),
    jobs = failed
  )
}

#' Create a new run for failed work
#'
#' A retry does not resume scheduler jobs in place and does not change the
#' original run. It creates a new [run_project()] submission containing the
#' failed or cancelled stages and subjects found in the source run. The selected
#' work is rerun even if old completion markers would normally skip it, and the
#' new provenance record identifies the source run.
#'
#' Preview with `dry_run = TRUE` before submitting. The preview returns a plan
#' and contacts no scheduler. With `dry_run = FALSE`, submission begins
#' immediately and the function returns the new run handle.
#'
#' @param input A project configuration object, YAML file, or project directory.
#' @param run_id Source run ID returned by [run_project()] or
#'   [get_project_runs()]. An explicit ID is recommended for retry.
#' @param include_blocked Also include downstream jobs marked `FAILED_BY_EXT`.
#'   These jobs did not fail themselves; they could not run because an earlier
#'   required job failed. They are excluded by default.
#' @param dry_run If `TRUE`, return a plan without submitting jobs. If `FALSE`,
#'   submit the new run immediately.
#' @return A `bg_project_plan` for a dry run or `bg_project_run` after submission.
#' @examples
#' \dontrun{
#' # Inspect and correct the failure before retrying.
#' diagnosis <- diagnose_project(scfg, run$run_id)
#'
#' # Preview only; no jobs are submitted.
#' retry_plan <- retry_project_run(scfg, run$run_id, dry_run = TRUE)
#'
#' # Submit a separate new run after reviewing the plan.
#' retry_run <- retry_project_run(scfg, run$run_id, dry_run = FALSE)
#' }
#' @seealso [diagnose_project()] to inspect the source failure and
#'   [get_run_provenance()] to compare the original and retry runs.
#' @export
retry_project_run <- function(input, run_id = "latest", include_blocked = FALSE,
                              dry_run = TRUE) {
  checkmate::assert_flag(include_blocked)
  checkmate::assert_flag(dry_run)
  scfg <- project_config_from_input(input)
  source_run_id <- resolve_run_id(scfg, run_id)
  jobs <- get_run_jobs(scfg, source_run_id)
  request <- retry_request_from_jobs(jobs, include_blocked)
  if (length(request$steps) == 0L) stop("No retryable failed jobs were found in this run.", call. = FALSE)
  if (dry_run) {
    return(plan_project(
      scfg, steps = request$steps,
      subject_filter = if (length(request$subject_filter)) request$subject_filter else NULL,
      postprocess_streams = if (length(request$postprocess_streams)) request$postprocess_streams else NULL,
      extract_streams = if (length(request$extract_streams)) request$extract_streams else NULL,
      force = TRUE
    ))
  }
  attr(scfg, "provenance_context") <- list(
    interface = "retry",
    parent_run_id = source_run_id,
    include_blocked = include_blocked
  )
  run_project(
    scfg, steps = request$steps,
    subject_filter = if (length(request$subject_filter)) request$subject_filter else NULL,
    postprocess_streams = if (length(request$postprocess_streams)) request$postprocess_streams else NULL,
    extract_streams = if (length(request$extract_streams)) request$extract_streams else NULL,
    force = TRUE
  )
}
