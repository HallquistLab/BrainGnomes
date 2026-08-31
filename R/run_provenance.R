run_provenance_timestamp <- function(time = Sys.time()) {
  format(time, "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
}

run_provenance_directory <- function(scfg, run_id) {
  file.path(scfg$metadata$log_directory, "runs", as.character(run_id))
}

run_provenance_file <- function(scfg, run_id) {
  file.path(run_provenance_directory(scfg, run_id), "provenance.json")
}

write_json_atomic <- function(value, file) {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  temp <- tempfile("provenance-", tmpdir = dirname(file), fileext = ".json")
  on.exit(if (file.exists(temp)) unlink(temp), add = TRUE)
  jsonlite::write_json(
    value, temp, pretty = TRUE, auto_unbox = TRUE, na = "null",
    null = "null", digits = NA
  )
  if (!file.rename(temp, file)) {
    if (!file.copy(temp, file, overwrite = TRUE)) {
      stop("Failed to write run provenance: ", file, call. = FALSE)
    }
    unlink(temp)
  }
  invisible(normalizePath(file, winslash = "/", mustWork = TRUE))
}

write_yaml_atomic <- function(value, file) {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  temp <- tempfile("run-config-", tmpdir = dirname(file), fileext = ".yaml")
  on.exit(if (file.exists(temp)) unlink(temp), add = TRUE)
  yaml::write_yaml(value, temp)
  if (!file.rename(temp, file)) {
    if (!file.copy(temp, file, overwrite = TRUE)) {
      stop("Failed to write run configuration snapshot: ", file, call. = FALSE)
    }
    unlink(temp)
  }
  invisible(normalizePath(file, winslash = "/", mustWork = TRUE))
}

write_table_atomic <- function(value, file) {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  temp <- tempfile("run-subjects-", tmpdir = dirname(file), fileext = ".tsv")
  on.exit(if (file.exists(temp)) unlink(temp), add = TRUE)
  utils::write.table(
    value, temp, sep = "\t", quote = FALSE, row.names = FALSE, na = ""
  )
  if (!file.rename(temp, file)) {
    if (!file.copy(temp, file, overwrite = TRUE)) {
      stop("Failed to write resolved run scope: ", file, call. = FALSE)
    }
    unlink(temp)
  }
  invisible(normalizePath(file, winslash = "/", mustWork = TRUE))
}

cached_artifact_checksum <- function(path, cache_file = NULL) {
  if (!checkmate::test_string(cache_file)) {
    return(unname(tools::md5sum(path)))
  }
  dir.create(dirname(cache_file), recursive = TRUE, showWarnings = FALSE)
  lock_file <- paste0(cache_file, ".lock")
  lock <- tryCatch(
    filelock::lock(lock_file, timeout = 10000),
    error = function(e) NULL
  )
  if (is.null(lock)) return(unname(tools::md5sum(path)))
  on.exit(filelock::unlock(lock), add = TRUE)

  resolved <- normalizePath(path, winslash = "/", mustWork = TRUE)
  info <- file.info(path)
  size <- as.numeric(info$size[[1L]])
  modified <- as.numeric(info$mtime[[1L]])
  changed <- as.numeric(info$ctime[[1L]])
  cache <- suppressWarnings(tryCatch(
    readRDS(cache_file), error = function(e) NULL
  ))
  if (!is.data.frame(cache) || !all(c(
    "path", "size_bytes", "modified", "changed", "checksum"
  ) %in% names(cache))) {
    cache <- data.frame(
      path = character(), size_bytes = numeric(), modified = numeric(),
      changed = numeric(), checksum = character(), stringsAsFactors = FALSE
    )
  }
  match <- cache$path == resolved & cache$size_bytes == size &
    cache$modified == modified & cache$changed == changed
  if (any(match)) return(cache$checksum[which(match)[[1L]]])

  checksum <- unname(tools::md5sum(path))
  cache <- cache[cache$path != resolved, , drop = FALSE]
  cache <- rbind(cache, data.frame(
    path = resolved, size_bytes = size, modified = modified, changed = changed,
    checksum = checksum, stringsAsFactors = FALSE
  ))
  temp <- tempfile("artifact-checksums-", tmpdir = dirname(cache_file))
  on.exit(if (file.exists(temp)) unlink(temp), add = TRUE)
  saveRDS(cache, temp)
  if (!file.rename(temp, cache_file)) {
    file.copy(temp, cache_file, overwrite = TRUE)
    unlink(temp)
  }
  checksum
}

fingerprint_run_artifact <- function(role, path, checksum_cache = NULL) {
  configured_path <- if (length(path) == 0L || is.na(path[[1L]]) ||
    !nzchar(path[[1L]])) NA_character_ else as.character(path[[1L]])
  exists <- !is.na(configured_path) && file.exists(configured_path)
  is_file <- exists && !dir.exists(configured_path)
  resolved_path <- if (exists) {
    normalizePath(configured_path, winslash = "/", mustWork = TRUE)
  } else if (!is.na(configured_path)) {
    normalizePath(configured_path, winslash = "/", mustWork = FALSE)
  } else {
    NA_character_
  }
  info <- if (exists) file.info(configured_path) else NULL
  data.frame(
    role = as.character(role),
    configured_path = configured_path,
    path = resolved_path,
    exists = exists,
    size_bytes = if (is_file) as.numeric(info$size[[1L]]) else NA_real_,
    modified_at = if (exists) {
      run_provenance_timestamp(info$mtime[[1L]])
    } else NA_character_,
    checksum_algorithm = if (is_file) "md5" else NA_character_,
    checksum = if (is_file) {
      cached_artifact_checksum(configured_path, checksum_cache)
    } else NA_character_,
    stringsAsFactors = FALSE
  )
}

collect_nested_artifact_paths <- function(value, prefix) {
  paths <- list()
  recurse <- function(x, field) {
    if (is.list(x) && !is.data.frame(x)) {
      nms <- names(x)
      if (is.null(nms)) nms <- as.character(seq_along(x))
      for (i in seq_along(x)) recurse(x[[i]], paste(field, nms[[i]], sep = "."))
      return(invisible(NULL))
    }
    leaf <- sub("^.*\\.", "", field)
    file_field <- grepl(
      "(file|files|path|paths|container|atlas|atlases|license|executable)$",
      leaf, ignore.case = TRUE
    )
    if (!file_field || !is.character(x) || length(x) == 0L) {
      return(invisible(NULL))
    }
    for (i in seq_along(x)) {
      candidate <- x[[i]]
      if (!is.na(candidate) && nzchar(candidate) && candidate != "template") {
        paths[[paste0(field, if (length(x) > 1L) paste0(".", i) else "")]] <<-
          candidate
      }
    }
    invisible(NULL)
  }
  recurse(value, prefix)
  paths
}

collect_run_artifacts <- function(scfg, execution, config_snapshot) {
  paths <- list(project_config_snapshot = config_snapshot)
  config_source <- attr(scfg, "yaml_file", exact = TRUE)
  if (checkmate::test_string(config_source)) paths$project_config_source <- config_source
  context <- attr(scfg, "provenance_context", exact = TRUE)
  if (is.list(context) && checkmate::test_string(context$plan_file)) {
    paths$submitted_plan <- context$plan_file
  }

  package_root <- tryCatch(find.package("BrainGnomes"), error = function(e) "")
  if (nzchar(package_root)) {
    package_files <- c(
      file.path(package_root, "DESCRIPTION"),
      file.path(package_root, "R", "BrainGnomes.rdb"),
      file.path(package_root, "R", "BrainGnomes.rdx"),
      file.path(package_root, "shell_functions"),
      file.path(package_root, c(
        "insert_tracked_job.R", "upd_job_status.R", "add_parent.R"
      ))
    )
    library_files <- list.files(
      file.path(package_root, "libs"), recursive = TRUE, full.names = TRUE
    )
    operational_directories <- c(
      file.path(package_root, "R"),
      file.path(package_root, "src"),
      file.path(package_root, "hpc_scripts"),
      file.path(package_root, "inst", "hpc_scripts")
    )
    operational_files <- unlist(lapply(
      operational_directories,
      function(directory) list.files(
        directory, recursive = TRUE, full.names = TRUE
      )
    ), use.names = FALSE)
    package_files <- unique(c(
      package_files, library_files, operational_files
    ))
    package_files <- package_files[file.exists(package_files) &
      !dir.exists(package_files)]
    normalized_root <- normalizePath(
      package_root, winslash = "/", mustWork = TRUE
    )
    for (i in seq_along(package_files)) {
      normalized_file <- normalizePath(
        package_files[[i]], winslash = "/", mustWork = TRUE
      )
      prefix <- paste0(normalized_root, "/")
      relative <- if (startsWith(normalized_file, prefix)) {
        substring(normalized_file, nchar(prefix) + 1L)
      } else basename(normalized_file)
      paths[[paste0("braingnomes_package.", gsub("/", ".", relative))]] <-
        package_files[[i]]
    }
  }

  scheduler_command <- switch(scfg$compute_environment$scheduler,
    slurm = "sbatch", torque = "qsub", sh = "sh", local = "sh",
    as.character(scfg$compute_environment$scheduler)
  )
  scheduler_path <- Sys.which(scheduler_command)
  if (nzchar(scheduler_path)) paths$scheduler_executable <- unname(scheduler_path)

  container_steps <- intersect(
    execution$steps,
    c("bids_conversion", "mriqc", "fmriprep", "aroma", "postprocess")
  )
  if (length(container_steps) > 0L) {
    runtime <- Sys.which("singularity")
    if (!nzchar(runtime)) runtime <- Sys.which("apptainer")
    if (nzchar(runtime)) paths$container_runtime <- unname(runtime)
  }

  explicit <- list(
    flywheel_sync = list(
      flywheel_executable = scfg$compute_environment$flywheel
    ),
    bids_conversion = list(
      heudiconv_container = scfg$compute_environment$heudiconv_container,
      heuristic_file = scfg$bids_conversion$heuristic_file
    ),
    mriqc = list(
      mriqc_container = scfg$compute_environment$mriqc_container
    ),
    fmriprep = list(
      fmriprep_container = scfg$compute_environment$fmriprep_container,
      freesurfer_license = scfg$fmriprep$fs_license_file
    ),
    aroma = list(
      aroma_container = scfg$compute_environment$aroma_container
    ),
    postprocess = list(
      fsl_container = scfg$compute_environment$fsl_container
    )
  )
  for (stage in intersect(execution$steps, names(explicit))) {
    for (label in names(explicit[[stage]])) {
      value <- explicit[[stage]][[label]]
      if (checkmate::test_string(value)) {
        paths[[paste(stage, label, sep = ".")]] <- value
      }
    }
  }

  if ("postprocess" %in% execution$steps) {
    for (stream in execution$postprocess_streams) {
      paths <- c(paths, collect_nested_artifact_paths(
        scfg$postprocess[[stream]], paste("postprocess", stream, sep = ".")
      ))
    }
  }
  if ("extract_rois" %in% execution$steps) {
    for (stream in execution$extract_streams) {
      paths <- c(paths, collect_nested_artifact_paths(
        scfg$extract_rois[[stream]], paste("extract_rois", stream, sep = ".")
      ))
    }
  }

  roles <- names(paths)
  checksum_cache <- file.path(
    scfg$metadata$log_directory, "runs", ".artifact_checksums.rds"
  )
  rows <- lapply(seq_along(paths), function(i) {
    fingerprint_run_artifact(roles[[i]], paths[[i]], checksum_cache)
  })
  artifacts <- if (length(rows) == 0L) {
    data.frame(
      role = character(), configured_path = character(), path = character(),
      exists = logical(), size_bytes = numeric(), modified_at = character(),
      checksum_algorithm = character(), checksum = character(),
      stringsAsFactors = FALSE
    )
  } else {
    do.call(rbind, rows)
  }
  rownames(artifacts) <- NULL
  artifacts[!duplicated(artifacts[c("role", "configured_path")]), , drop = FALSE]
}

run_package_identity <- function() {
  description <- utils::packageDescription("BrainGnomes")
  package_path <- tryCatch(find.package("BrainGnomes"), error = function(e) "")
  git_commit <- value_or_default(
    description$RemoteSha,
    value_or_default(description$GithubSHA1, NA_character_)
  )
  git_dirty <- NA
  if (nzchar(package_path) && dir.exists(file.path(package_path, ".git")) &&
      nzchar(Sys.which("git"))) {
    commit <- suppressWarnings(system2(
      "git", c("-C", shQuote(package_path), "rev-parse", "HEAD"),
      stdout = TRUE, stderr = FALSE
    ))
    if (length(commit) > 0L && identical(attr(commit, "status"), NULL)) {
      git_commit <- commit[[1L]]
    }
    dirty <- suppressWarnings(system2(
      "git", c("-C", shQuote(package_path), "status", "--porcelain"),
      stdout = TRUE, stderr = FALSE
    ))
    git_dirty <- length(dirty) > 0L
  }
  list(
    package = "BrainGnomes",
    version = as.character(utils::packageVersion("BrainGnomes")),
    library_path = if (nzchar(package_path)) {
      normalizePath(package_path, winslash = "/", mustWork = TRUE)
    } else NA_character_,
    built = value_or_default(description$Built, NA_character_),
    repository = value_or_default(description$RemoteRepo, NA_character_),
    git_commit = git_commit,
    git_dirty = git_dirty
  )
}

run_software_identity <- function() {
  namespaces <- sort(loadedNamespaces())
  versions <- vapply(namespaces, function(package) {
    tryCatch(
      as.character(utils::packageVersion(package)),
      error = function(e) NA_character_
    )
  }, character(1))
  list(
    braingnomes = run_package_identity(),
    r = list(
      version = R.version.string,
      platform = R.version$platform,
      architecture = R.version$arch,
      r_home = R.home(),
      library_paths = .libPaths()
    ),
    loaded_packages = as.list(versions)
  )
}

run_host_identity <- function() {
  info <- Sys.info()
  environment_names <- c(
    "LOADEDMODULES", "CONDA_PREFIX", "VIRTUAL_ENV", "RETICULATE_PYTHON",
    "SLURM_CLUSTER_NAME", "PBS_SERVER", "SINGULARITY_NAME", "APPTAINER_NAME"
  )
  environment <- Sys.getenv(environment_names, unset = NA_character_)
  names(environment) <- environment_names
  list(
    system = as.list(info),
    working_directory = normalizePath(getwd(), winslash = "/", mustWork = TRUE),
    timezone = Sys.timezone(),
    locale = Sys.getlocale(),
    selected_environment = as.list(environment)
  )
}

run_invocation_context <- function(scfg) {
  context <- attr(scfg, "provenance_context", exact = TRUE)
  if (is.null(context)) context <- list()
  if (is.null(context$interface)) {
    context$interface <- if (interactive()) "r_interactive" else "r"
  }
  context$command <- commandArgs(trailingOnly = FALSE)
  context
}

record_run_provenance <- function(scfg, run_id, execution, debug = FALSE,
                                  log_level = "INFO") {
  checkmate::assert_class(scfg, "bg_project_cfg")
  checkmate::assert_string(run_id)
  checkmate::assert_class(execution, "bg_project_execution")
  checkmate::assert_flag(debug)
  checkmate::assert_string(log_level)

  run_dir <- run_provenance_directory(scfg, run_id)
  dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
  config_file <- file.path(run_dir, "project_config.yaml")
  subjects_file <- file.path(run_dir, "subjects.tsv")
  write_yaml_atomic(unclass(scfg), config_file)
  write_table_atomic(execution$subjects, subjects_file)
  artifacts <- collect_run_artifacts(scfg, execution, config_file)
  config_row <- artifacts[artifacts$role == "project_config_snapshot", , drop = FALSE]
  context <- run_invocation_context(scfg)
  recorded_at <- run_provenance_timestamp()

  record <- list(
    schema_version = "brain-gnomes-run-provenance-v1",
    run_id = run_id,
    recorded_at = recorded_at,
    state = "submission_started",
    invocation = context,
    request = list(
      steps = execution$steps,
      step_flags = as.list(execution$step_flags),
      subject_filter = execution$subject_filter,
      postprocess_streams = execution$postprocess_streams,
      extract_streams = execution$extract_streams,
      force = execution$force,
      debug = debug,
      log_level = log_level
    ),
    execution = list(
      scope_deferred = execution$scope_deferred,
      scope_status = if (execution$scope_deferred) "deferred" else "resolved",
      scope_resolved_at = if (execution$scope_deferred) {
        NA_character_
      } else recorded_at,
      subjects = execution$subjects,
      job_plan = build_project_jobs(scfg, execution)
    ),
    configuration = list(
      source_file = attr(scfg, "yaml_file", exact = TRUE),
      snapshot_file = normalizePath(
        config_file, winslash = "/", mustWork = TRUE
      ),
      snapshot_checksum_algorithm = if (nrow(config_row)) {
        config_row$checksum_algorithm[[1L]]
      } else NA_character_,
      snapshot_checksum = if (nrow(config_row)) {
        config_row$checksum[[1L]]
      } else NA_character_,
      values = unclass(scfg)
    ),
    software = run_software_identity(),
    host = run_host_identity(),
    scheduler = list(
      configured = scfg$compute_environment$scheduler,
      executable = artifacts$path[artifacts$role == "scheduler_executable"]
    ),
    artifacts = artifacts,
    files = list(
      provenance = normalizePath(
        run_provenance_file(scfg, run_id), winslash = "/", mustWork = FALSE
      ),
      configuration = normalizePath(config_file, winslash = "/", mustWork = TRUE),
      subjects = normalizePath(subjects_file, winslash = "/", mustWork = TRUE)
    ),
    submission = list(
      updated_at = NA_character_,
      submitted_job_ids = character(),
      deferred_subject_submission = execution$scope_deferred,
      tracked_jobs = data.frame()
    )
  )
  write_json_atomic(record, run_provenance_file(scfg, run_id))
}

tracked_jobs_for_provenance <- function(scfg, run_id) {
  db <- scfg$metadata$sqlite_db
  if (!checkmate::test_file_exists(db) ||
      !sqlite_table_exists(db, "job_tracking")) {
    return(data.frame())
  }
  jobs <- tryCatch(
    get_tracked_job_status(sequence_id = run_id, sqlite_db = db),
    error = function(e) data.frame()
  )
  if (nrow(jobs) > 0L && "job_obj" %in% names(jobs)) jobs$job_obj <- NULL
  jobs
}

update_run_provenance_submission <- function(scfg, run_id,
                                             submitted_ids = NULL,
                                             deferred = FALSE,
                                             resolved_subjects = NULL) {
  file <- run_provenance_file(scfg, run_id)
  if (!file.exists(file)) return(invisible(NULL))
  record <- jsonlite::read_json(file, simplifyVector = FALSE)
  previous_ids <- unlist(record$submission$submitted_job_ids, use.names = FALSE)
  tracked_jobs <- tracked_jobs_for_provenance(scfg, run_id)
  tracked_ids <- if (nrow(tracked_jobs) > 0L && "job_id" %in% names(tracked_jobs)) {
    as.character(tracked_jobs$job_id)
  } else character()
  if (!is.null(resolved_subjects)) {
    checkmate::assert_data_frame(resolved_subjects)
    subjects_file <- file.path(dirname(file), "subjects.tsv")
    write_table_atomic(resolved_subjects, subjects_file)
    record$execution$subjects <- resolved_subjects
    record$execution$scope_status <- "resolved"
    record$execution$scope_resolved_at <- run_provenance_timestamp()
    record$files$subjects <- normalizePath(
      subjects_file, winslash = "/", mustWork = TRUE
    )
  }
  record$state <- "submitted"
  record$submission <- list(
    updated_at = run_provenance_timestamp(),
    submitted_job_ids = unique(c(
      as.character(previous_ids), as.character(submitted_ids), tracked_ids
    )),
    deferred_subject_submission = isTRUE(deferred) ||
      isTRUE(record$execution$scope_deferred),
    tracked_jobs = tracked_jobs
  )
  write_json_atomic(record, file)
}

latest_run_provenance_id <- function(scfg) {
  root <- file.path(scfg$metadata$log_directory, "runs")
  if (!dir.exists(root)) return(NULL)
  files <- list.files(
    root, pattern = "^provenance\\.json$", recursive = TRUE,
    full.names = TRUE
  )
  if (length(files) == 0L) return(NULL)
  records <- lapply(files, function(file) {
    tryCatch(
      jsonlite::read_json(file, simplifyVector = TRUE),
      error = function(e) NULL
    )
  })
  valid <- vapply(records, function(x) {
    is.list(x) && checkmate::test_string(x$run_id) &&
      checkmate::test_string(x$recorded_at)
  }, logical(1))
  if (!any(valid)) return(NULL)
  records <- records[valid]
  times <- as.POSIXct(
    vapply(records, `[[`, character(1), "recorded_at"),
    format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"
  )
  records[[which.max(times)]]$run_id
}

#' Read the complete provenance record for a project run
#'
#' Each submitted run records the selected stages, streams, and subjects; an
#' exact copy of the project configuration; requested computing resources and
#' job order; BrainGnomes, R, software, and submission-computer details; the
#' scheduler; and checksums that identify containers and other files that
#' controlled the run. The returned object also includes the currently tracked
#' jobs when available. Use it to confirm exactly what BrainGnomes submitted or
#' to compare an original run with a later retry.
#'
#' @param input A project configuration object, YAML file, or project directory.
#' @param run_id Run ID returned by [run_project()] or [get_project_runs()]. Use
#'   `"latest"` for the most recently recorded run.
#' @return A `bg_run_provenance` object.
#' @examples
#' \dontrun{
#' provenance <- get_run_provenance(scfg, run$run_id)
#' provenance$request
#' provenance$execution$subjects
#' provenance$configuration$snapshot_file
#' }
#' @seealso [diagnose_project()] to inspect failures and [retry_project_run()]
#'   to create a new run from failed work.
#' @export
get_run_provenance <- function(input, run_id = "latest") {
  scfg <- project_config_from_input(input)
  resolved <- if (identical(run_id, "latest")) {
    value_or_default(latest_run_provenance_id(scfg), resolve_run_id(scfg, run_id))
  } else {
    as.character(run_id)
  }
  file <- run_provenance_file(scfg, resolved)
  if (!file.exists(file)) {
    stop("No provenance record was found for run ", resolved, ".", call. = FALSE)
  }
  record <- jsonlite::read_json(file, simplifyVector = TRUE)
  if (!identical(
    record$schema_version, "brain-gnomes-run-provenance-v1"
  )) {
    stop(
      "Unsupported run provenance schema: ",
      value_or_default(record$schema_version, "<missing>"),
      call. = FALSE
    )
  }
  record$current_jobs <- tracked_jobs_for_provenance(scfg, resolved)
  record$provenance_file <- normalizePath(file, winslash = "/", mustWork = TRUE)
  class(record) <- c("bg_run_provenance", class(record))
  record
}

#' @export
print.bg_run_provenance <- function(x, ...) {
  cli::cli_h2("BrainGnomes run provenance {.val {x$run_id}}")
  cli::cli_text("Recorded: {x$recorded_at}")
  cli::cli_text("Invocation: {x$invocation$interface}")
  cli::cli_text("Steps: {paste(x$request$steps, collapse = ', ')}")
  if (isTRUE(x$execution$scope_deferred)) {
    cli::cli_text("Scope: deferred until Flywheel synchronization")
  } else {
    subjects <- x$execution$subjects
    cli::cli_text(
      "Scope: {length(unique(subjects$sub_id))} subject{?s}, {nrow(subjects)} subject/session row{?s}"
    )
  }
  cli::cli_text(
    "BrainGnomes: {x$software$braingnomes$version}; artifacts: {nrow(x$artifacts)}; tracked jobs: {nrow(x$current_jobs)}"
  )
  cli::cli_text("Record: {.file {x$provenance_file}}")
  invisible(x)
}
