run_brain_gnomes_cli <- function(args = character()) {
  script <- system.file("BrainGnomes", package = "BrainGnomes")
  if (!nzchar(script)) {
    script <- normalizePath(file.path("inst", "BrainGnomes"), mustWork = TRUE)
  }

  out <- suppressWarnings(system2(
    command = file.path(R.home("bin"), "Rscript"),
    args = c(script, args),
    stdout = TRUE,
    stderr = TRUE
  ))
  status <- attr(out, "status")
  if (is.null(status)) status <- 0L
  list(status = as.integer(status), output = out)
}

test_that("BrainGnomes --help prints global help", {
  res <- run_brain_gnomes_cli("--help")
  expect_equal(res$status, 0L)
  expect_true(any(grepl("^Usage: BrainGnomes <command> \\[options\\]$", res$output)))
  expect_true(any(grepl("^  doctor <project_directory\\|config\\.yaml>", res$output)))
  expect_true(any(grepl("^  plan <project_directory\\|config\\.yaml>", res$output)))
  expect_true(any(grepl("^  validate-bids <project_directory\\|config\\.yaml>", res$output)))
  expect_true(any(grepl("^  retry <project_directory\\|config\\.yaml>", res$output)))
  expect_true(any(grepl("Use 'BrainGnomes help <command>'", res$output, fixed = TRUE)))
  expect_false(any(grepl("--steps=.*bids_validation", res$output)))
})

test_that("BrainGnomes help run_project prints command help", {
  res <- run_brain_gnomes_cli(c("help", "run_project"))
  expect_equal(res$status, 0L)
  expect_true(any(grepl("^Usage: BrainGnomes run <project_directory\\|config\\.yaml\\|plan\\.yaml> \\[options\\]$", res$output)))
  expect_true(any(grepl("^Options:$", res$output)))
  expect_true(any(grepl("schema_version brain-gnomes-plan-v1", res$output, fixed = TRUE)))
})

test_that("BrainGnomes run_project --help prints command help", {
  res <- run_brain_gnomes_cli(c("run_project", "--help"))
  expect_equal(res$status, 0L)
  expect_true(any(grepl("^Usage: BrainGnomes run <project_directory\\|config\\.yaml\\|plan\\.yaml> \\[options\\]$", res$output)))
  expect_true(any(grepl("^  --debug", res$output)))
  expect_true(any(grepl("^  --force", res$output)))
  expect_true(any(grepl("^  --dry-run", res$output)))
  expect_true(any(grepl("^  --extract-streams=<streams>", res$output)))
  expect_true(any(grepl("^  --log-level=<level>", res$output)))
  expect_false(any(grepl("--steps=.*bids_validation", res$output)))
})

test_that("run_project CLI forwards selected extraction streams", {
  captured <- NULL
  cfg <- structure(list(metadata = list(project_name = "cli-test")), class = "bg_project_cfg")

  local_mocked_bindings(
    load_project = function(input) {
      expect_identical(input, "/proj/example")
      cfg
    },
    run_project = function(scfg, ...) {
      captured <<- c(list(scfg = scfg), list(...))
      TRUE
    },
    .package = "BrainGnomes"
  )

  cli_args <- parse_cli_args(c(
    "--steps=extract_rois",
    "--extract_streams=rest task",
    "--dry-run"
  ))
  expect_true(BrainGnomes:::run_project_cli("/proj/example", cli_args))
  expect_identical(captured$scfg, cfg)
  expect_identical(captured$steps, "extract_rois")
  expect_identical(captured$extract_streams, c("rest", "task"))
  expect_true(captured$dry_run)
})

test_that("BrainGnomes status --help prints command help", {
  res <- run_brain_gnomes_cli(c("status", "--help"))
  expect_equal(res$status, 0L)
  expect_true(any(grepl("^Usage: BrainGnomes status <project_directory\\|config\\.yaml> \\[options\\]$", res$output)))
  expect_true(any(grepl("^  --sub-id=<id>", res$output)))
  expect_true(any(grepl("^  --summary", res$output)))
  expect_true(any(grepl("^  --run=<id\\|latest>", res$output)))
  expect_true(any(grepl("^  --watch", res$output)))
})

test_that("BrainGnomes lifecycle commands have command-specific help", {
  for (command in c("init", "config", "doctor", "plan", "validate-bids", "logs", "diagnose", "retry", "cancel")) {
    res <- run_brain_gnomes_cli(c(command, "--help"))
    expect_equal(res$status, 0L, info = command)
    expect_true(any(grepl(paste0("^Usage: BrainGnomes ", command), res$output)), info = command)
  }
})

test_that("BrainGnomes rejects unknown options before project access", {
  res <- run_brain_gnomes_cli(c("doctor", "/does/not/exist", "--definitely-unknown"))
  expect_equal(res$status, 2L)
  expect_true(any(grepl("Unknown option", res$output, fixed = TRUE)))
})

test_that("BrainGnomes init and config validate support a headless first project", {
  installed_export <- suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"),
    c("-e", shQuote("cat('initialize_project' %in% getNamespaceExports('BrainGnomes'))")),
    stdout = TRUE, stderr = FALSE
  ))
  skip_if_not(identical(installed_export, "TRUE"),
    "installed package predates lifecycle CLI exports")
  root <- tempfile("cli-init-")
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  init <- run_brain_gnomes_cli(c("init", "cli_demo", root, "--non-interactive"))
  expect_equal(init$status, 0L, info = paste(init$output, collapse = "\n"))
  expect_true(file.exists(file.path(root, "project_config.yaml")))

  validation <- run_brain_gnomes_cli(c(
    "config", "validate", file.path(root, "project_config.yaml"), "--format=json"
  ))
  expect_equal(validation$status, 0L, info = paste(validation$output, collapse = "\n"))
  parsed <- jsonlite::fromJSON(paste(validation$output, collapse = "\n"))
  expect_true(parsed$valid)
})

test_that("BrainGnomes cli_project is an unknown command", {
  res <- run_brain_gnomes_cli("cli_project")
  expect_equal(res$status, 2L)
  expect_true(any(grepl("^Unknown command: cli_project$", res$output)))
  expect_true(any(grepl("^Usage: BrainGnomes <command> \\[options\\]$", res$output)))
})

test_that("BrainGnomes without args prints help and exits nonzero", {
  res <- run_brain_gnomes_cli()
  expect_equal(res$status, 1L)
  expect_true(any(grepl("^Usage: BrainGnomes <command> \\[options\\]$", res$output)))
})
