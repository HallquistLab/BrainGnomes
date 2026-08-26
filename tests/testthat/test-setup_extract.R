test_that("interactive extraction setup retains masks for existing and new streams", {
  skip_if_not_installed("yaml")

  tmp <- tempfile("setup-extract-mask-")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)
  mask_path <- file.path(tmp, "selected-mask.nii.gz")
  file.create(mask_path)

  responses <- c(mask_path, "fresh", mask_path)
  response_index <- 0L
  local_mocked_bindings(
    setup_job = function(cfg, ...) cfg,
    prompt_input = function(...) {
      response_index <<- response_index + 1L
      responses[[response_index]]
    },
    .package = "BrainGnomes"
  )

  existing <- structure(list(
    metadata = list(project_directory = tmp),
    extract_rois = list(
      enable = TRUE,
      demo = list(mask_file = "old-mask.nii.gz")
    )
  ), class = "bg_project_cfg")

  updated_existing <- setup_extract_stream(
    existing,
    fields = "extract_rois/demo/mask_file",
    stream_name = "demo"
  )
  expect_identical(updated_existing$extract_rois$demo$mask_file, mask_path)
  existing_yaml <- file.path(tmp, "existing.yaml")
  save_project_config(updated_existing, file = existing_yaml)
  expect_identical(yaml::read_yaml(existing_yaml)$extract_rois$demo$mask_file, mask_path)

  new_stream <- structure(list(
    metadata = list(project_directory = tmp),
    extract_rois = list(enable = TRUE)
  ), class = "bg_project_cfg")

  updated_new <- setup_extract_stream(
    new_stream,
    fields = "extract_rois/fresh/mask_file",
    stream_name = "fresh"
  )
  expect_identical(updated_new$extract_rois$fresh$mask_file, mask_path)
  new_yaml <- file.path(tmp, "new.yaml")
  save_project_config(updated_new, file = new_yaml)
  expect_identical(yaml::read_yaml(new_yaml)$extract_rois$fresh$mask_file, mask_path)
  expect_identical(response_index, 3L)
})

test_that("interactive extraction setup normalizes intentionally empty masks", {
  response <- NA_character_
  local_mocked_bindings(
    setup_job = function(cfg, ...) cfg,
    prompt_input = function(...) response,
    .package = "BrainGnomes"
  )

  cfg <- structure(list(
    extract_rois = list(
      enable = TRUE,
      demo = list(mask_file = "previous-mask.nii.gz")
    )
  ), class = "bg_project_cfg")

  for (empty_response in list(NA_character_, "", "   ", ".na.character")) {
    response <- empty_response
    cleared <- setup_extract_stream(
      cfg,
      fields = "extract_rois/demo/mask_file",
      stream_name = "demo"
    )
    expect_null(cleared$extract_rois$demo$mask_file)
  }
})

test_that("interactive extraction setup records ROI diagnostics preference", {
  local_mocked_bindings(
    setup_job = function(cfg, ...) cfg,
    prompt_input = function(...) TRUE,
    .package = "BrainGnomes"
  )

  cfg <- structure(list(
    extract_rois = list(enable = TRUE, demo = list(save_diagnostics = FALSE))
  ), class = "bg_project_cfg")

  updated <- setup_extract_stream(
    cfg,
    fields = "extract_rois/demo/save_diagnostics",
    stream_name = "demo"
  )
  expect_true(updated$extract_rois$demo$save_diagnostics)
})

test_that("extraction validation defaults and checks ROI diagnostics preference", {
  tmp <- tempfile("validate-extract-diagnostics-")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)
  atlas <- file.path(tmp, "atlas.nii.gz")
  file.create(atlas)

  config <- list(
    input_streams = "clean",
    atlases = atlas,
    roi_reduce = "mean",
    rtoz = FALSE,
    min_vox_per_roi = 1L
  )
  defaulted <- validate_extract_config_single(config, quiet = TRUE)
  expect_false(defaulted$extract_rois$save_diagnostics)
  expect_false("extract_rois/save_diagnostics" %in% defaulted$gaps)

  config$save_diagnostics <- "yes"
  invalid <- validate_extract_config_single(config, quiet = TRUE)
  expect_null(invalid$extract_rois$save_diagnostics)
  expect_true("extract_rois/save_diagnostics" %in% invalid$gaps)
})
