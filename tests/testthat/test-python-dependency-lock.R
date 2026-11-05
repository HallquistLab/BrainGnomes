test_that("dependency locks ignore shared subject logs and persist in the user cache", {
  root <- withr::local_tempdir()
  withr::local_envvar(R_USER_CACHE_DIR = file.path(root, "user-cache"))
  withr::local_options(
    BrainGnomes.install_py_deps = TRUE,
    BrainGnomes.py_force_managed_env = FALSE
  )
  log_dir <- file.path(root, "shared-subject")
  dir.create(log_dir)
  legacy_lock <- file.path(log_dir, "brain_gnomes_python_dependencies.lock")
  file.create(legacy_lock)
  Sys.chmod(legacy_lock, "0444")
  withr::defer(Sys.chmod(legacy_lock, "0666"))
  legacy_mode <- file.info(legacy_lock)$mode
  logger <- lgr::get_logger_glue("BrainGnomes.test.python_dependency_lock")
  logger$add_appender(
    lgr::AppenderFile$new(file.path(log_dir, "postprocess.log")),
    name = "postprocess_log"
  )
  withr::defer(logger$remove_appender("postprocess_log"))

  local_mocked_bindings(
    py_available = function(initialize = FALSE) TRUE,
    py_module_available = function(module) TRUE,
    py_install = function(...) stop("No packages are missing"),
    source_python = function(...) invisible(NULL),
    .package = "reticulate"
  )
  expect_true(is.environment(load_image_resampling_backend("nibabel", lg = logger)))
  lock_file <- file.path(
    tools::R_user_dir("BrainGnomes", "cache"), "locks",
    "brain_gnomes_python_dependencies.lock"
  )
  expect_true(file.exists(lock_file))
  expect_true(file.exists(legacy_lock))
  expect_identical(file.info(legacy_lock)$mode, legacy_mode)
  # A job with no subject logger uses the very same lock, not a second home lock.
  expect_true(is.environment(load_image_resampling_backend("nibabel")))
})

test_that("lock timeout stops before checking or installing Python packages", {
  root <- withr::local_tempdir()
  withr::local_envvar(R_USER_CACHE_DIR = root)
  withr::local_options(
    BrainGnomes.install_py_deps = TRUE,
    BrainGnomes.py_force_managed_env = FALSE,
    BrainGnomes.py_install_lock_timeout = NULL
  )
  observed_timeout <- NULL
  local_mocked_bindings(
    lock = function(path, timeout, ...) {
      observed_timeout <<- timeout
      NULL
    },
    .package = "filelock"
  )
  local_mocked_bindings(
    py_available = function(initialize = FALSE) TRUE,
    py_module_available = function(...) stop("Dependencies must not be checked without a lock"),
    py_install = function(...) stop("Packages must not be installed without a lock"),
    source_python = function(...) stop("Backend must not be loaded without a lock"),
    .package = "reticulate"
  )
  expect_error(load_image_resampling_backend("nibabel"), "Timed out.*milliseconds")
  expect_identical(observed_timeout, 600000)
  withr::local_options(BrainGnomes.py_install_lock_timeout = 1250)
  expect_error(load_image_resampling_backend("nibabel"), "Timed out")
  expect_identical(observed_timeout, 1250)
})

test_that("lock file access errors give permission guidance", {
  root <- withr::local_tempdir()
  withr::local_envvar(R_USER_CACHE_DIR = root)
  local_mocked_bindings(
    lock = function(...) stop("Cannot open lock file: Permission denied"),
    .package = "filelock"
  )
  error <- tryCatch(acquire_python_dependency_lock(), error = identity)
  expect_s3_class(error, "error")
  expect_match(conditionMessage(error), "Check read/write permissions")
  expect_match(conditionMessage(error), "Permission denied")
  expect_false(grepl("wait longer", conditionMessage(error), fixed = TRUE))
})

test_that("real dependency locks exclude other processes and release on setup errors", {
  root <- withr::local_tempdir()
  withr::local_envvar(R_USER_CACHE_DIR = root)
  withr::local_options(
    BrainGnomes.install_py_deps = TRUE,
    BrainGnomes.py_force_managed_env = FALSE
  )
  lock_file <- file.path(
    tools::R_user_dir("BrainGnomes", "cache"), "locks",
    "brain_gnomes_python_dependencies.lock"
  )
  probe_script <- file.path(root, "probe.R")
  writeLines(c(
    "handle <- filelock::lock(commandArgs(TRUE)[[1L]], timeout = 0)",
    "cat(if (is.null(handle)) 'busy' else 'available', '\\n')",
    "if (!is.null(handle)) invisible(filelock::unlock(handle))"
  ), probe_script)
  # Return lock availability from a fresh R process using the paths above.
  # Same-process reacquisition cannot test exclusion between separate jobs.
  probe_lock <- function() {
    system2(
      file.path(R.home("bin"), "Rscript"),
      c("--vanilla", shQuote(probe_script), shQuote(lock_file)),
      stdout = TRUE, stderr = TRUE
    )
  }
  handle <- acquire_python_dependency_lock(timeout = 0)
  withr::defer(filelock::unlock(handle))
  expect_identical(trimws(probe_lock()), "busy")
  filelock::unlock(handle)
  expect_identical(trimws(probe_lock()), "available")

  missing <- FALSE
  local_mocked_bindings(
    py_available = function(initialize = FALSE) TRUE,
    py_module_available = function(module) !missing,
    py_install = function(...) stop("simulated installation failure"),
    source_python = function(...) {
      # Dependency setup must release its lock before sourcing the backend.
      expect_identical(trimws(probe_lock()), "available")
      invisible(NULL)
    },
    .package = "reticulate"
  )
  load_image_resampling_backend("nibabel")
  expect_true(file.exists(lock_file))
  missing <- TRUE
  expect_message(
    expect_error(load_image_resampling_backend("nibabel"), "simulated installation failure"),
    "Installing missing Python packages"
  )
  expect_identical(trimws(probe_lock()), "available")
  expect_true(file.exists(lock_file))
})

test_that("disabling installation bypasses dependency locks", {
  root <- withr::local_tempdir()
  withr::local_envvar(R_USER_CACHE_DIR = root)
  withr::local_options(
    BrainGnomes.install_py_deps = FALSE,
    BrainGnomes.py_force_managed_env = FALSE
  )
  local_mocked_bindings(
    acquire_python_dependency_lock = function(...) stop("Must not acquire a lock"),
    .package = "BrainGnomes"
  )
  local_mocked_bindings(
    py_available = function(initialize = FALSE) TRUE,
    py_module_available = function(module) TRUE,
    source_python = function(...) invisible(NULL),
    .package = "reticulate"
  )
  expect_true(is.environment(load_image_resampling_backend("nibabel")))
  expect_false(dir.exists(file.path(root, "R", "BrainGnomes", "locks")))
})
