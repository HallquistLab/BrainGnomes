make_extract_cli_fixture <- function() {
  root <- tempfile("extract-cli-")
  input_dir <- file.path(root, "input")
  dir.create(input_dir, recursive = TRUE)

  dims <- c(2L, 2L, 2L, 30L)
  x <- seq(-2, 2, length.out = dims[[4]])
  bold <- array(0, dim = dims)
  for (volume in seq_along(x)) {
    bold[1, , , volume] <- 100 + x[[volume]]
    bold[2, , , volume] <- 100 + x[[volume]]^3
  }

  atlas <- array(0L, dim = dims[1:3])
  atlas[1, , ] <- 1L
  atlas[2, , ] <- 2L

  bold_file <- file.path(input_dir, "sub-01_task-rest_desc-clean_bold.nii.gz")
  atlas_file <- file.path(root, "DemoAtlas.nii.gz")
  RNifti::writeNifti(RNifti::asNifti(bold), bold_file)
  RNifti::writeNifti(RNifti::asNifti(atlas), atlas_file)

  list(
    root = root,
    input_dir = input_dir,
    atlas_file = atlas_file,
    x = x,
    config = list(
      input_regex = "task:rest desc:preproc suffix:bold",
      bids_desc = "clean",
      atlases = atlas_file,
      roi_reduce = "mean",
      min_vox_per_roi = 1L,
      rtoz = FALSE,
      save_ts = TRUE,
      overwrite = FALSE
    )
  )
}

run_extract_cli_fixture <- function(fixture, config, label) {
  script <- system.file("extract_cli.R", package = "BrainGnomes")
  if (!nzchar(script)) stop("Cannot locate installed extraction helper.")

  config$out_dir <- file.path(fixture$root, label)
  dir.create(config$out_dir, recursive = TRUE, showWarnings = FALSE)
  config_file <- file.path(fixture$root, paste0(label, ".yaml"))
  yaml::write_yaml(config, config_file)

  previous_log_file <- Sys.getenv("log_file", unset = NA_character_)
  previous_pkg_dir <- Sys.getenv("pkg_dir", unset = NA_character_)
  on.exit({
    if (is.na(previous_log_file)) {
      Sys.unsetenv("log_file")
    } else {
      Sys.setenv(log_file = previous_log_file)
    }
    if (is.na(previous_pkg_dir)) {
      Sys.unsetenv("pkg_dir")
    } else {
      Sys.setenv(pkg_dir = previous_pkg_dir)
    }
  }, add = TRUE)
  Sys.setenv(log_file = file.path(fixture$root, paste0(label, ".log")))
  Sys.unsetenv("pkg_dir")

  script_env <- new.env(parent = globalenv())
  script_env$commandArgs <- function(trailingOnly = FALSE) {
    c(
      "R",
      paste0("--file=", script),
      "--args",
      paste0("--input=", fixture$input_dir),
      paste0("--config_yaml=", config_file)
    )
  }

  source(script, local = script_env)
  invisible(config$out_dir)
}

test_that("extraction helper honors every nested correlation method and method vectors", {
  skip_if_not_installed("RNifti")
  skip_if_not_installed("corpcor")
  skip_if_not_installed("yaml")

  fixture <- make_extract_cli_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE, force = TRUE), add = TRUE)
  methods <- c("pearson", "spearman", "kendall", "cor.shrink")

  for (method in methods) {
    config <- fixture$config
    config$correlation <- list(method = method)
    out_dir <- run_extract_cli_fixture(fixture, config, bids_camelcase(method))

    method_entity <- bids_camelcase(method)
    cor_file <- file.path(
      out_dir,
      "DemoAtlas",
      paste0(
        "sub-01_task-rest_desc-clean_rois-DemoAtlas_cor-",
        method_entity,
        "_connectivity.tsv"
      )
    )
    ts_file <- file.path(
      out_dir,
      "DemoAtlas",
      "sub-01_task-rest_desc-clean_rois-DemoAtlas_timeseries.tsv"
    )

    expect_true(file.exists(cor_file), info = method)
    expect_true(file.exists(ts_file), info = method)
    series <- as.matrix(read.delim(ts_file, check.names = FALSE)[, c("roi1", "roi2")])
    expected <- if (method == "cor.shrink") {
      corpcor::cor.shrink(series, verbose = FALSE)
    } else {
      stats::cor(series, method = method)
    }
    observed <- as.matrix(read.delim(cor_file, check.names = FALSE))
    expected <- matrix(as.numeric(expected), nrow = nrow(expected))
    expect_equal(unname(observed), expected, tolerance = 1e-7, info = method)
  }

  config <- fixture$config
  config$correlation <- list(method = methods)
  out_dir <- run_extract_cli_fixture(fixture, config, "all-methods")
  cor_files <- list.files(
    file.path(out_dir, "DemoAtlas"),
    pattern = "_connectivity\\.tsv$",
    full.names = TRUE
  )
  expect_length(cor_files, length(methods))
  expect_true(all(vapply(methods, function(method) {
    any(grepl(paste0("_cor-", bids_camelcase(method), "_"), basename(cor_files)))
  }, logical(1))))
})

test_that("extraction helper forwards save_ts from the project configuration", {
  skip_if_not_installed("RNifti")
  skip_if_not_installed("yaml")

  fixture <- make_extract_cli_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE, force = TRUE), add = TRUE)

  config <- fixture$config
  config$correlation <- list(method = "spearman")
  config$save_ts <- FALSE
  out_dir <- run_extract_cli_fixture(fixture, config, "without-timeseries")

  cor_file <- file.path(
    out_dir,
    "DemoAtlas",
    "sub-01_task-rest_desc-clean_rois-DemoAtlas_cor-spearman_connectivity.tsv"
  )
  ts_files <- list.files(
    file.path(out_dir, "DemoAtlas"),
    pattern = "_timeseries\\.tsv$"
  )

  expect_true(file.exists(cor_file))
  expect_length(ts_files, 0L)
  observed <- as.matrix(read.delim(cor_file, check.names = FALSE))
  expect_equal(unname(observed[1, 2]), 1, tolerance = 1e-7)

  with_timeseries <- config
  with_timeseries$save_ts <- TRUE
  with_timeseries_dir <- run_extract_cli_fixture(
    fixture, with_timeseries, "with-timeseries"
  )
  expect_length(
    list.files(with_timeseries_dir, pattern = "_timeseries\\.tsv$", recursive = TRUE),
    1L
  )
  expect_length(
    list.files(with_timeseries_dir, pattern = "_connectivity\\.tsv$", recursive = TRUE),
    1L
  )

  backward_compatible <- config
  backward_compatible$save_ts <- NULL
  backward_compatible_dir <- run_extract_cli_fixture(
    fixture, backward_compatible, "without-save-ts-setting"
  )
  expect_length(
    list.files(backward_compatible_dir, pattern = "_timeseries\\.tsv$", recursive = TRUE),
    1L
  )
  expect_length(
    list.files(backward_compatible_dir, pattern = "_connectivity\\.tsv$", recursive = TRUE),
    1L
  )
})

test_that("extraction helper writes and manifests requested ROI diagnostics", {
  skip_if_not_installed("RNifti")
  skip_if_not_installed("yaml")

  fixture <- make_extract_cli_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE, force = TRUE), add = TRUE)

  config <- fixture$config
  config$correlation <- list(method = "pearson")
  config$save_diagnostics <- TRUE
  config$output_manifest_file <- file.path(fixture$root, "diagnostic-outputs.json")
  out_dir <- run_extract_cli_fixture(fixture, config, "with-diagnostics")

  diagnostics_files <- list.files(
    out_dir,
    pattern = "_roidiagnostics\\.tsv$",
    recursive = TRUE,
    full.names = TRUE
  )
  expect_length(diagnostics_files, 1L)
  diagnostics <- read.delim(diagnostics_files, check.names = FALSE)
  expect_named(diagnostics, c(
    "roi", "atlas_value", "n_vox_atlas", "n_vox_in_mask",
    "n_vox_usable", "min_vox_required", "proportion_in_mask",
    "proportion_usable", "proportion_usable_in_mask", "retained",
    "exclusion_reason"
  ))

  manifest <- jsonlite::fromJSON(
    config$output_manifest_file,
    simplifyVector = FALSE
  )
  expect_equal(manifest$file_count, 3L)
  expect_true(any(grepl(
    "_roidiagnostics\\.tsv$",
    vapply(manifest$files, `[[`, character(1), "path")
  )))
})

test_that("extraction helper preserves paired inputs from multiple streams", {
  skip_if_not_installed("RNifti")
  skip_if_not_installed("yaml")

  fixture <- make_extract_cli_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE, force = TRUE), add = TRUE)

  source_bold <- file.path(
    fixture$input_dir,
    "sub-01_task-rest_desc-clean_bold.nii.gz"
  )
  additional_inputs <- file.path(fixture$input_dir, c(
    "sub-01_task-nback_desc-denoised_bold.nii.gz",
    "sub-01_task-rest_desc-denoised_bold.nii.gz",
    "sub-01_task-nback_desc-clean_bold.nii.gz"
  ))
  expect_true(all(file.copy(source_bold, additional_inputs)))

  config <- fixture$config
  config$input_regex <- c(
    "task:rest desc:preproc suffix:bold",
    "task:nback desc:preproc suffix:bold"
  )
  config$bids_desc <- c("clean", "denoised")
  config$correlation <- list(method = "none")
  config$output_manifest_file <- file.path(fixture$root, "paired-outputs.json")
  out_dir <- run_extract_cli_fixture(fixture, config, "paired-streams")

  timeseries_files <- list.files(
    file.path(out_dir, "DemoAtlas"),
    pattern = "_timeseries\\.tsv$",
    full.names = TRUE
  )
  expect_setequal(
    basename(timeseries_files),
    c(
      "sub-01_task-rest_desc-clean_rois-DemoAtlas_timeseries.tsv",
      "sub-01_task-nback_desc-denoised_rois-DemoAtlas_timeseries.tsv"
    )
  )
  expect_false(any(grepl("task-rest_desc-denoised", timeseries_files)))
  expect_false(any(grepl("task-nback_desc-clean", timeseries_files)))

  manifest <- jsonlite::fromJSON(
    config$output_manifest_file,
    simplifyVector = FALSE
  )
  expect_equal(manifest$file_count, 2L)
  expect_setequal(
    vapply(manifest$files, `[[`, character(1), "path"),
    file.path("DemoAtlas", basename(timeseries_files))
  )
})

test_that("extraction helper writes a manifest containing only its exact outputs", {
  skip_if_not_installed("RNifti")
  skip_if_not_installed("yaml")

  fixture <- make_extract_cli_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE, force = TRUE), add = TRUE)

  output_dir <- file.path(fixture$root, "with-manifest")
  dir.create(output_dir)
  unrelated_file <- file.path(output_dir, "unrelated-job.tsv")
  writeLines("unrelated", unrelated_file)

  config <- fixture$config
  config$correlation <- list(method = "pearson")
  config$output_manifest_file <- file.path(fixture$root, "extract-outputs.json")
  out_dir <- run_extract_cli_fixture(fixture, config, "with-manifest")

  expect_true(file.exists(config$output_manifest_file))
  manifest_json <- paste(
    readLines(config$output_manifest_file, warn = FALSE),
    collapse = "\n"
  )
  manifest <- jsonlite::fromJSON(manifest_json, simplifyVector = FALSE)
  manifest_paths <- vapply(manifest$files, `[[`, character(1), "path")

  expect_identical(manifest$scope, "explicit")
  expect_equal(manifest$file_count, 2L)
  expect_setequal(
    basename(manifest_paths),
    c(
      "sub-01_task-rest_desc-clean_rois-DemoAtlas_timeseries.tsv",
      "sub-01_task-rest_desc-clean_rois-DemoAtlas_cor-pearson_connectivity.tsv"
    )
  )
  expect_false("unrelated-job.tsv" %in% manifest_paths)
  expect_true(verify_output_manifest(out_dir, manifest_json)$verified)

  unlink(file.path(out_dir, manifest_paths[[1L]]))
  expect_false(verify_output_manifest(out_dir, manifest_json)$verified)
})

test_that("extraction helper applies the interactively configured ROI mask", {
  skip_if_not_installed("RNifti")
  skip_if_not_installed("yaml")

  fixture <- make_extract_cli_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE, force = TRUE), add = TRUE)

  bold_file <- file.path(fixture$input_dir, "sub-01_task-rest_desc-clean_bold.nii.gz")
  bold <- RNifti::readNifti(bold_file)
  bold[1, 1, 1, ] <- 100 + 10 * fixture$x
  RNifti::writeNifti(bold, bold_file)

  mask <- array(1L, dim = c(2L, 2L, 2L))
  mask[1, 1, 1] <- 0L
  mask_file <- file.path(fixture$root, "selected-mask.nii.gz")
  RNifti::writeNifti(RNifti::asNifti(mask), mask_file)

  scfg <- structure(list(
    extract_rois = list(
      enable = TRUE,
      default = list(mask_file = "old-mask.nii.gz")
    )
  ), class = "bg_project_cfg")
  local_mocked_bindings(
    setup_job = function(cfg, ...) cfg,
    prompt_input = function(...) mask_file,
    .package = "BrainGnomes"
  )
  configured <- setup_extract_stream(
    scfg,
    fields = "extract_rois/default/mask_file",
    stream_name = "default"
  )

  unmasked <- fixture$config
  unmasked$correlation <- list(method = "none")
  unmasked_dir <- run_extract_cli_fixture(fixture, unmasked, "without-mask")

  masked <- unmasked
  masked$mask_file <- configured$extract_rois$default$mask_file
  masked_dir <- run_extract_cli_fixture(fixture, masked, "with-mask")

  filename <- "sub-01_task-rest_desc-clean_rois-DemoAtlas_timeseries.tsv"
  unmasked_series <- read.delim(file.path(unmasked_dir, "DemoAtlas", filename))
  masked_series <- read.delim(file.path(masked_dir, "DemoAtlas", filename))

  expect_identical(configured$extract_rois$default$mask_file, mask_file)
  expect_equal(unmasked_series$roi1, 100 + 3.25 * fixture$x, tolerance = 1e-6)
  expect_equal(masked_series$roi1, 100 + fixture$x, tolerance = 1e-6)
  expect_equal(masked_series$roi2, unmasked_series$roi2, tolerance = 1e-7)
})

test_that("extraction helper preserves masked atlas labels and matrix dimensions", {
  skip_if_not_installed("RNifti")
  skip_if_not_installed("yaml")

  fixture <- make_extract_cli_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE, force = TRUE), add = TRUE)

  partial_mask <- array(0L, dim = c(2L, 2L, 2L))
  partial_mask[1, , ] <- 1L
  partial_mask_file <- file.path(fixture$root, "partial-mask.nii.gz")
  empty_mask_file <- file.path(fixture$root, "empty-mask.nii.gz")
  RNifti::writeNifti(RNifti::asNifti(partial_mask), partial_mask_file)
  RNifti::writeNifti(
    RNifti::asNifti(array(0L, dim = c(2L, 2L, 2L))),
    empty_mask_file
  )

  config <- fixture$config
  config$correlation <- list(method = "pearson")
  config$mask_file <- partial_mask_file
  partial_dir <- run_extract_cli_fixture(fixture, config, "partially-masked")

  config$mask_file <- empty_mask_file
  expect_warning(
    empty_dir <- run_extract_cli_fixture(fixture, config, "fully-masked"),
    regexp = "writing an all-NA connectivity matrix"
  )

  ts_name <- "sub-01_task-rest_desc-clean_rois-DemoAtlas_timeseries.tsv"
  cor_name <- "sub-01_task-rest_desc-clean_rois-DemoAtlas_cor-pearson_connectivity.tsv"
  for (out_dir in list(partial_dir, empty_dir)) {
    series <- read.delim(file.path(out_dir, "DemoAtlas", ts_name), check.names = FALSE)
    connectivity <- read.delim(
      file.path(out_dir, "DemoAtlas", cor_name),
      check.names = FALSE
    )
    expect_named(series, c("volume", "roi1", "roi2"))
    expect_equal(dim(series), c(30L, 3L))
    expect_named(connectivity, c("roi1", "roi2"))
    expect_equal(dim(connectivity), c(2L, 2L))
    expect_true(all(is.na(series$roi2)))
    expect_true(all(is.na(connectivity$roi2)))
  }

  empty_matrix <- read.delim(
    file.path(empty_dir, "DemoAtlas", cor_name),
    check.names = FALSE
  )
  expect_true(all(is.na(empty_matrix)))
})

test_that("extraction helper supports nested and legacy time-series-only settings", {
  skip_if_not_installed("RNifti")
  skip_if_not_installed("yaml")

  fixture <- make_extract_cli_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE, force = TRUE), add = TRUE)

  nested <- fixture$config
  nested$correlation <- list(method = "none")
  nested_dir <- run_extract_cli_fixture(fixture, nested, "nested-none")
  ts_file <- file.path(
    nested_dir,
    "DemoAtlas",
    "sub-01_task-rest_desc-clean_rois-DemoAtlas_timeseries.tsv"
  )
  expect_true(file.exists(ts_file))
  expect_length(list.files(nested_dir, pattern = "_connectivity\\.tsv$", recursive = TRUE), 0L)
  expect_equal(dim(read.delim(ts_file, check.names = FALSE)), c(30L, 3L))

  legacy <- fixture$config
  legacy$cor_method <- "none"
  legacy_dir <- run_extract_cli_fixture(fixture, legacy, "legacy-none")
  expect_length(list.files(legacy_dir, pattern = "_timeseries\\.tsv$", recursive = TRUE), 1L)
  expect_length(list.files(legacy_dir, pattern = "_connectivity\\.tsv$", recursive = TRUE), 0L)
})

test_that("extraction helper rejects invalid time-series-only configurations", {
  skip_if_not_installed("RNifti")
  skip_if_not_installed("yaml")

  fixture <- make_extract_cli_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE, force = TRUE), add = TRUE)

  mixed <- fixture$config
  mixed$correlation <- list(method = c("none", "pearson"))
  expect_error(
    run_extract_cli_fixture(fixture, mixed, "mixed-none"),
    "'none' cannot be combined with correlation methods"
  )
  expect_length(list.files(file.path(fixture$root, "mixed-none"), recursive = TRUE), 0L)

  disabled <- fixture$config
  disabled$correlation <- list(method = "none")
  disabled$save_ts <- FALSE
  expect_error(
    run_extract_cli_fixture(fixture, disabled, "disabled-outputs"),
    "cor_method = 'none' requires save_ts = TRUE"
  )
  expect_length(list.files(file.path(fixture$root, "disabled-outputs"), recursive = TRUE), 0L)
})

test_that("extraction helper accepts matching and legacy flat correlation settings", {
  skip_if_not_installed("RNifti")
  skip_if_not_installed("yaml")

  fixture <- make_extract_cli_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE, force = TRUE), add = TRUE)

  config <- fixture$config
  config$cor_method <- "kendall"
  legacy_dir <- run_extract_cli_fixture(fixture, config, "legacy-method")
  expect_true(file.exists(file.path(
    legacy_dir,
    "DemoAtlas",
    "sub-01_task-rest_desc-clean_rois-DemoAtlas_cor-kendall_connectivity.tsv"
  )))

  config$correlation <- list(method = "kendall")
  matching_dir <- run_extract_cli_fixture(fixture, config, "matching-methods")
  expect_true(file.exists(file.path(
    matching_dir,
    "DemoAtlas",
    "sub-01_task-rest_desc-clean_rois-DemoAtlas_cor-kendall_connectivity.tsv"
  )))
})

test_that("extraction helper rejects conflicting or missing correlation settings", {
  skip_if_not_installed("RNifti")
  skip_if_not_installed("yaml")

  fixture <- make_extract_cli_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE, force = TRUE), add = TRUE)

  conflicting <- fixture$config
  conflicting$correlation <- list(method = "spearman")
  conflicting$cor_method <- "pearson"
  expect_error(
    run_extract_cli_fixture(fixture, conflicting, "conflicting-methods"),
    "Conflicting correlation methods.*correlation/method.*cor_method"
  )
  expect_length(list.files(file.path(fixture$root, "conflicting-methods")), 0L)

  missing <- fixture$config
  expect_error(
    run_extract_cli_fixture(fixture, missing, "missing-methods"),
    "A correlation method must be configured.*correlation/method.*cor_method"
  )
  expect_length(list.files(file.path(fixture$root, "missing-methods")), 0L)
})

test_that("submit_extract_rois preserves nested methods and save_ts in scheduled arguments", {
  tmp <- tempfile("submit-extract-config-")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  scfg <- list(
    metadata = list(
      postproc_directory = file.path(tmp, "postproc"),
      rois_directory = file.path(tmp, "rois")
    ),
    compute_environment = list(scheduler = "slurm"),
    postprocess = list(clean = list(
      input_regex = "task:rest desc:preproc suffix:bold",
      bids_desc = "clean"
    )),
    extract_rois = list(default = list(
      input_streams = "clean",
      atlases = file.path(tmp, "DemoAtlas.nii.gz"),
      mask_file = file.path(tmp, "selected-mask.nii.gz"),
      correlation = list(method = c("spearman", "kendall")),
      save_ts = FALSE,
      save_diagnostics = TRUE,
      allow_atlas_resampling = TRUE,
      atlas_space = "MNI152NLin2009cAsym",
      roi_reduce = "mean",
      min_vox_per_roi = 1L,
      rtoz = FALSE
    ))
  )

  captured_env <- NULL
  local_mocked_bindings(
    get_job_script = function(...) "extract_rois.sbatch",
    cluster_job_submit = function(..., env_variables = NULL) {
      captured_env <<- env_variables
      "12345"
    },
    log_submission_command = function(...) invisible(NULL)
  )

  job_id <- submit_extract_rois(
    scfg = scfg,
    sub_id = "01",
    sched_script = "extract_subject.sbatch",
    ex_stream = "default"
  )

  expect_identical(job_id, "12345")
  parsed <- parse_cli_args(captured_env[["extract_cli"]])
  expect_identical(parsed$correlation$method, c("spearman", "kendall"))
  expect_false(parsed$save_ts)
  expect_true(parsed$save_diagnostics)
  expect_true(parsed$allow_atlas_resampling)
  expect_identical(parsed$atlas_space, "MNI152NLin2009cAsym")
  expect_identical(parsed$mask_file, scfg$extract_rois$default$mask_file)

  scfg$extract_rois$default$correlation$method <- "none"
  scfg$extract_rois$default$save_ts <- TRUE
  submit_extract_rois(
    scfg = scfg,
    sub_id = "01",
    sched_script = "extract_subject.sbatch",
    ex_stream = "default"
  )
  timeseries_only <- parse_cli_args(captured_env[["extract_cli"]])
  expect_identical(timeseries_only$correlation$method, "none")
  expect_true(timeseries_only$save_ts)
})

test_that("submit_extract_rois serializes aligned sources for multiple streams", {
  tmp <- tempfile("submit-extract-sources-")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  scfg <- list(
    metadata = list(
      postproc_directory = file.path(tmp, "postproc"),
      rois_directory = file.path(tmp, "rois")
    ),
    compute_environment = list(scheduler = "slurm"),
    postprocess = list(
      clean = list(
        input_regex = c(
          "task:rest run:1 desc:preproc suffix:bold",
          "task:rest run:2 desc:preproc suffix:bold"
        ),
        bids_desc = "clean"
      ),
      denoised = list(
        input_regex = "task:nback desc:preproc suffix:bold",
        bids_desc = "denoised"
      )
    ),
    extract_rois = list(default = list(
      input_streams = c("clean", "denoised"),
      atlases = file.path(tmp, "DemoAtlas.nii.gz"),
      correlation = list(method = "none"),
      save_ts = TRUE,
      roi_reduce = "mean",
      min_vox_per_roi = 1L,
      rtoz = FALSE
    ))
  )

  captured_env <- NULL
  local_mocked_bindings(
    get_job_script = function(...) "extract_rois.sbatch",
    cluster_job_submit = function(..., env_variables = NULL) {
      captured_env <<- env_variables
      "12345"
    },
    log_submission_command = function(...) invisible(NULL)
  )

  expect_identical(
    submit_extract_rois(
      scfg = scfg,
      sub_id = "01",
      sched_script = "extract_subject.sbatch",
      ex_stream = "default"
    ),
    "12345"
  )
  parsed <- parse_cli_args(captured_env[["extract_cli"]])
  expect_identical(
    parsed$input_regex,
    c(
      "task:rest run:1 desc:preproc suffix:bold",
      "task:rest run:2 desc:preproc suffix:bold",
      "task:nback desc:preproc suffix:bold"
    )
  )
  expect_identical(parsed$bids_desc, c("clean", "clean", "denoised"))
})

test_that("interactive extraction setup preserves none and enables time-series output", {
  scfg <- structure(list(
    extract_rois = list(
      enable = TRUE,
      default = list(
        correlation = list(method = "pearson"),
        save_ts = FALSE
      )
    )
  ), class = "bg_project_cfg")

  local_mocked_bindings(
    setup_job = function(cfg, ...) cfg,
    select_list_safe = function(...) "none",
    prompt_input = function(...) stop("save_ts should not be prompted when correlation is none"),
    .package = "BrainGnomes"
  )

  updated <- setup_extract_stream(
    scfg,
    fields = c(
      "extract_rois/default/correlation/method",
      "extract_rois/default/save_ts"
    ),
    stream_name = "default"
  )

  expect_identical(updated$extract_rois$default$correlation$method, "none")
  expect_true(updated$extract_rois$default$save_ts)
  serialized <- nested_list_to_args(updated$extract_rois$default, collapse = TRUE)
  parsed <- parse_cli_args(serialized)
  expect_identical(parsed$correlation$method, "none")
  expect_true(parsed$save_ts)
})
