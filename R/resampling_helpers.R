#' Load the Python image-resampling backend
#'
#' This centralizes dependency handling for the TemplateFlow and atlas
#' resampling paths so both use the same Python environment and bundled
#' implementation.
#'
#' @keywords internal
#' @noRd
load_image_resampling_backend <- function(required_modules,
                                          install_dependencies = TRUE,
                                          lg = NULL,
                                          lock_directory = NULL) {
  checkmate::assert_character(
    required_modules, min.len = 1L, any.missing = FALSE, unique = TRUE
  )
  checkmate::assert_flag(install_dependencies)
  checkmate::assert_string(lock_directory, null.ok = TRUE)

  install_dependencies <- isTRUE(getOption(
    "BrainGnomes.install_py_deps", install_dependencies
  ))
  force_managed_env <- isTRUE(getOption(
    "BrainGnomes.py_force_managed_env", FALSE
  ))

  logger_logs_dir <- function(logger) {
    log_dir <- path.expand("~")
    if (!checkmate::test_class(logger, "Logger")) return(log_dir)
    primary_dest <- tryCatch(
      logger$appenders$postprocess_log$destination,
      error = function(...) NULL
    )
    if (is.null(primary_dest) || !nzchar(primary_dest)) return(log_dir)

    tryCatch(
      suppressWarnings(normalizePath(
        dirname(primary_dest), winslash = "/", mustWork = FALSE
      )),
      error = function(...) {
        to_log(
          lg, "warn",
          "Cannot find log root for Python dependency lock based on {primary_dest}"
        )
        log_dir
      }
    )
  }

  python_env_root <- function() {
    reticulate_python <- Sys.getenv("RETICULATE_PYTHON", "")
    if (nzchar(reticulate_python) && file.exists(reticulate_python)) {
      return(dirname(dirname(reticulate_python)))
    }
    virtual_env <- Sys.getenv("VIRTUAL_ENV", "")
    if (nzchar(virtual_env)) return(virtual_env)
    conda_prefix <- Sys.getenv("CONDA_PREFIX", "")
    if (nzchar(conda_prefix)) return(conda_prefix)
    ""
  }

  env_is_writable <- function(path) {
    nzchar(path) && dir.exists(path) && file.access(path, 2) == 0
  }

  with_unset_env <- function(vars, expr) {
    old_env <- Sys.getenv(vars, unset = NA)
    on.exit({
      for (nm in names(old_env)) {
        val <- old_env[[nm]]
        if (is.na(val)) {
          Sys.unsetenv(nm)
        } else {
          do.call(Sys.setenv, setNames(list(val), nm))
        }
      }
    }, add = TRUE)
    Sys.unsetenv(vars)
    force(expr)
  }

  py_initialized <- reticulate::py_available(initialize = FALSE)
  use_managed_env <- force_managed_env && !py_initialized
  if (force_managed_env && py_initialized) {
    to_log(
      lg, "warn",
      paste0(
        "BrainGnomes.py_force_managed_env is TRUE, but Python is already ",
        "initialized; using the active Python environment."
      )
    )
  }

  if (!use_managed_env && install_dependencies && !py_initialized) {
    env_root <- python_env_root()
    if (nzchar(env_root) && !env_is_writable(env_root)) {
      use_managed_env <- TRUE
      to_log(
        lg, "warn",
        paste0(
          "Active Python environment '{env_root}' is not writable. ",
          "Falling back to a managed reticulate environment."
        )
      )
    }
  }

  missing <- character()
  if (install_dependencies) {
    if (is.null(lock_directory)) lock_directory <- logger_logs_dir(lg)
    if (!dir.exists(lock_directory)) {
      dir.create(lock_directory, recursive = TRUE, showWarnings = FALSE)
    }
    if (!dir.exists(lock_directory)) {
      stop(
        "Cannot create Python dependency lock directory: ", lock_directory,
        call. = FALSE
      )
    }

    lock_file <- file.path(
      lock_directory, "brain_gnomes_python_dependencies.lock"
    )
    lock_handle <- NULL
    release_lock <- function() {
      if (!is.null(lock_handle)) {
        filelock::unlock(lock_handle)
        lock_handle <<- NULL
      }
    }
    lock_timeout <- getOption("BrainGnomes.py_install_lock_timeout", 600)
    lock_handle <- tryCatch(
      filelock::lock(lock_file, timeout = lock_timeout),
      error = function(e) {
        stop(
          glue::glue(
            "Unable to acquire dependency installation lock at {lock_file}. ",
            "Another process may still be installing Python packages. ",
            "Increase option 'BrainGnomes.py_install_lock_timeout' to wait ",
            "longer if needed. Original error: {conditionMessage(e)}"
          ),
          call. = FALSE
        )
      }
    )
    on.exit(release_lock(), add = TRUE)

    if (use_managed_env && !py_initialized) {
      managed_env_vars <- c(
        "RETICULATE_PYTHON", "RETICULATE_PYTHON_ENV", "VIRTUAL_ENV",
        "CONDA_PREFIX"
      )
      missing <- with_unset_env(managed_env_vars, {
        reticulate::py_require(required_modules)
        required_modules[!vapply(
          required_modules, reticulate::py_module_available, logical(1)
        )]
      })
    } else {
      missing <- required_modules[!vapply(
        required_modules, reticulate::py_module_available, logical(1)
      )]
      if (length(missing) > 0L) {
        message("Installing missing Python packages into the active environment...")
        install_error <- NULL
        tryCatch(
          reticulate::py_install(missing),
          error = function(e) install_error <<- e
        )
        if (!is.null(install_error)) {
          hint <- paste0(
            "If the active Python environment is not writable, set ",
            "options(BrainGnomes.py_force_managed_env = TRUE) or point ",
            "RETICULATE_PYTHON to a user-writable environment."
          )
          stop(
            glue::glue(
              "Python package installation failed: ",
              "{conditionMessage(install_error)}. {hint}"
            ),
            call. = FALSE
          )
        }
        missing <- required_modules[!vapply(
          required_modules, reticulate::py_module_available, logical(1)
        )]
      }
    }
    release_lock()
  } else {
    missing <- required_modules[!vapply(
      required_modules, reticulate::py_module_available, logical(1)
    )]
  }

  if (length(missing) > 0L) {
    stop(
      glue::glue(
        "The following required Python modules are missing: ",
        "{paste(missing, collapse = ', ')}. Install them in the active ",
        "Python environment, or set ",
        "options(BrainGnomes.py_force_managed_env = TRUE) to use a managed ",
        "environment."
      ),
      call. = FALSE
    )
  }

  script_path <- system.file(
    "fetch_matched_template_image.py", package = "BrainGnomes"
  )
  if (!file.exists(script_path)) {
    stop("Required Python script not found: ", script_path, call. = FALSE)
  }
  backend <- new.env(parent = parent.frame())
  reticulate::source_python(script_path, envir = backend)
  backend
}

#' Compute a stable checksum for a NIfTI spatial grid
#'
#' @keywords internal
#' @noRd
nifti_grid_checksum <- function(path) {
  checkmate::assert_file_exists(path)
  header <- RNifti::niftiHeader(path)
  grid <- list(
    dimensions = as.integer(header$dim[2:4]),
    voxel_sizes = as.numeric(header$pixdim[2:4]),
    spatial_units = bitwAnd(as.integer(header$xyzt_units), 7L),
    qform_code = as.integer(header$qform_code),
    sform_code = as.integer(header$sform_code),
    qform = as.numeric(unclass(RNifti::xform(header, TRUE))),
    sform = as.numeric(unclass(RNifti::xform(header, FALSE)))
  )
  serialized <- tempfile("nifti-grid-")
  on.exit(unlink(serialized, force = TRUE), add = TRUE)
  saveRDS(grid, serialized, version = 2L)
  unname(tools::md5sum(serialized))
}

#' Validate label preservation after nearest-neighbour atlas resampling
#'
#' @keywords internal
#' @noRd
validate_resampled_atlas_labels <- function(source_file, resampled_file) {
  source_values <- as.vector(RNifti::readNifti(source_file))
  if (!checkmate::test_integerish(source_values, tol = 1e-6)) {
    stop(
      "Atlas ", source_file,
      " contains non-integer labels (outside tolerance).",
      call. = FALSE
    )
  }
  source_labels <- sort(unique(source_values[source_values > 0]))
  if (length(source_labels) == 0L) {
    stop(
      "Atlas '", source_file, "' contains no positive ROI labels.",
      call. = FALSE
    )
  }

  resampled_values <- as.vector(RNifti::readNifti(resampled_file))
  if (!checkmate::test_integerish(resampled_values, tol = 1e-6)) {
    stop(
      "Nearest-neighbour atlas resampling produced non-integer labels.",
      call. = FALSE
    )
  }
  resampled_labels <- sort(unique(resampled_values[resampled_values > 0]))
  lost <- setdiff(source_labels, resampled_labels)
  introduced <- setdiff(resampled_labels, source_labels)
  if (length(lost) > 0L || length(introduced) > 0L) {
    details <- c(
      if (length(lost) > 0L) {
        paste0("lost labels: ", paste(lost, collapse = ", "))
      },
      if (length(introduced) > 0L) {
        paste0("introduced labels: ", paste(introduced, collapse = ", "))
      }
    )
    stop(
      "Atlas resampling did not preserve the label set (",
      paste(details, collapse = "; "), "). ",
      "Use an atlas whose parcels remain represented on the target grid.",
      call. = FALSE
    )
  }
  invisible(source_labels)
}

#' Resolve the atlas image to use for ROI extraction
#'
#' Matching atlas/BOLD grids always use the configured atlas directly. A grid
#' mismatch can be resampled only when the caller explicitly opts in and the
#' atlas coordinate space matches the BOLD `space` entity. The atlas filename's
#' formal BIDS `space` entity takes precedence over the configured fallback.
#'
#' @keywords internal
#' @noRd
prepare_atlas_for_extraction <- function(atlas_file, bold_file,
                                         allow_atlas_resampling = FALSE,
                                         atlas_space = NULL,
                                         cache_dir,
                                         lg = NULL) {
  checkmate::assert_file_exists(atlas_file)
  checkmate::assert_file_exists(bold_file)
  checkmate::assert_flag(allow_atlas_resampling)
  checkmate::assert_string(atlas_space, null.ok = TRUE, min.chars = 1L)
  checkmate::assert_string(cache_dir, min.chars = 1L)

  grid <- pp_compare_nifti_grid(
    bold_file, atlas_file,
    reference_label = "BOLD image",
    candidate_label = "atlas"
  )
  if (isTRUE(grid$passed)) {
    to_log(
      lg, "debug",
      "Atlas {atlas_file} already matches the BOLD grid; using it without resampling"
    )
    return(atlas_file)
  }

  if (!allow_atlas_resampling) {
    to_log(
      lg, "fatal",
      paste0(
        "{grid$message} Set allow_atlas_resampling = TRUE only when the atlas ",
        "is already in the BOLD coordinate space and nearest-neighbour ",
        "resampling is appropriate. Declare that space with a formal atlas ",
        "filename space-<label> entity or the atlas_space fallback."
      )
    )
  }

  bids_info <- as.list(extract_bids_info(bold_file))
  bold_space <- bids_info$space
  if (length(bold_space) == 0L || is.null(bold_space) ||
      is.na(bold_space) || !nzchar(bold_space)) {
    to_log(
      lg, "fatal",
      paste0(
        "Cannot verify the coordinate space before atlas resampling because ",
        "the BOLD filename has no space-<label> entity: {bold_file}"
      )
    )
  }
  filename_space <- bids_space_from_filename(atlas_file)[[1L]]
  has_filename_space <- !is.na(filename_space) && nzchar(filename_space)
  has_configured_space <- !is.null(atlas_space)
  if (has_filename_space && has_configured_space &&
      !identical(filename_space, atlas_space)) {
    to_log(
      lg, "fatal",
      paste0(
        "Atlas filename space-{filename_space} conflicts with configured ",
        "atlas_space '{atlas_space}' for {atlas_file}. Remove the YAML ",
        "fallback or make the two declarations agree."
      )
    )
  }
  resolved_atlas_space <- if (has_filename_space) filename_space else atlas_space
  if (is.null(resolved_atlas_space)) {
    to_log(
      lg, "fatal",
      paste0(
        "The atlas filename has no formal space-<label> entity, so atlas_space ",
        "is required when allow_atlas_resampling = TRUE and the atlas grid ",
        "does not match. The BOLD image declares space-{bold_space}."
      )
    )
  }
  if (!identical(resolved_atlas_space, bold_space)) {
    to_log(
      lg, "fatal",
      paste0(
        "Atlas resampling requires matching coordinate spaces, but ",
        "the atlas declares space-{resolved_atlas_space} and the BOLD image ",
        "declares space-{bold_space}. Registration between spaces is not ",
        "performed."
      )
    )
  }

  to_log(
    lg, "info",
    paste0(
      "Atlas {atlas_file} is declared in space-{resolved_atlas_space} ",
      "{if (has_filename_space) 'by its filename' else 'by atlas_space'} but ",
      "does not match the BOLD voxel grid. Resampling it with ",
      "nearest-neighbour interpolation."
    )
  )
  resample_atlas_to_bold(atlas_file, bold_file, cache_dir, lg = lg)
}

#' Resample an atlas onto a BOLD reference grid with caching
#'
#' @keywords internal
#' @noRd
resample_atlas_to_bold <- function(atlas_file, bold_file, cache_dir, lg = NULL) {
  checkmate::assert_file_exists(atlas_file)
  checkmate::assert_file_exists(bold_file)
  checkmate::assert_string(cache_dir, min.chars = 1L)
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  if (!checkmate::test_directory_exists(cache_dir, access = "w")) {
    stop("Atlas cache directory is not writable: ", cache_dir, call. = FALSE)
  }

  source_checksum <- cached_artifact_checksum(
    atlas_file,
    cache_file = file.path(cache_dir, "atlas-checksums.rds"),
    label = paste0("atlas ", basename(atlas_file))
  )
  grid_checksum <- nifti_grid_checksum(bold_file)
  atlas_name <- sub("\\.nii(\\.gz)?$", "", basename(atlas_file))
  safe_name <- gsub("[^A-Za-z0-9._-]+", "-", atlas_name)
  output <- file.path(
    cache_dir,
    paste0(
      safe_name, "_source-", substr(source_checksum, 1L, 12L),
      "_grid-", substr(grid_checksum, 1L, 12L), ".nii.gz"
    )
  )

  lock_timeout <- getOption(
    "BrainGnomes.atlas_resample_lock_timeout", 600000
  )
  lock_file <- paste0(output, ".lock")
  lock <- tryCatch(
    filelock::lock(lock_file, timeout = lock_timeout),
    error = function(e) {
      stop(
        "Unable to acquire atlas-resampling cache lock at ", lock_file,
        ": ", conditionMessage(e), call. = FALSE
      )
    }
  )
  on.exit(filelock::unlock(lock), add = TRUE)

  if (file.exists(output)) {
    cached_grid <- tryCatch(
      pp_compare_nifti_grid(
        bold_file, output,
        reference_label = "BOLD image",
        candidate_label = "cached resampled atlas"
      ),
      error = function(e) NULL
    )
    if (!is.null(cached_grid) && isTRUE(cached_grid$passed)) {
      validate_resampled_atlas_labels(atlas_file, output)
      to_log(lg, "info", "Using cached resampled atlas {output}")
      return(output)
    }
  }

  backend <- load_image_resampling_backend(
    required_modules = c("nibabel", "nilearn"),
    install_dependencies = TRUE,
    lg = lg,
    lock_directory = cache_dir
  )
  temp_output <- tempfile(
    pattern = paste0(".", safe_name, "-"),
    tmpdir = cache_dir,
    fileext = ".nii.gz"
  )
  on.exit(unlink(temp_output, force = TRUE), add = TRUE)
  backend$resample_image_to_reference(
    source_file = atlas_file,
    reference_file = bold_file,
    output = temp_output,
    interpolation = "nearest"
  )

  result_grid <- pp_compare_nifti_grid(
    bold_file, temp_output,
    reference_label = "BOLD image",
    candidate_label = "resampled atlas"
  )
  if (!isTRUE(result_grid$passed)) {
    stop(result_grid$message, call. = FALSE)
  }
  validate_resampled_atlas_labels(atlas_file, temp_output)

  if (file.exists(output)) unlink(output, force = TRUE)
  if (!file.rename(temp_output, output)) {
    stop("Could not atomically install resampled atlas: ", output, call. = FALSE)
  }
  to_log(
    lg, "info",
    "Resampled atlas {atlas_file} onto the BOLD grid with nearest-neighbour interpolation and cached it at {output}"
  )
  output
}
