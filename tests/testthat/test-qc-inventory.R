# Build a minimal study with real filesystem evidence and an isolated database.
make_qc_fixture <- function(root = tempfile("qc study ")) {
  dirs <- file.path(root, c("bids", "fmriprep", "postproc", "rois", "mriqc", "logs"))
  for (path in dirs) dir.create(path, recursive = TRUE)
  db <- file.path(root, "tracking.sqlite")
  create_tracking_db(db)
  cfg <- structure(list(metadata = list(project_name = "QC example",
    project_directory = root, bids_directory = dirs[1], fmriprep_directory = dirs[2],
    postproc_directory = dirs[3], rois_directory = dirs[4], mriqc_directory = dirs[5],
    log_directory = dirs[6], sqlite_db = db),
    fmriprep = list(enable = TRUE), mriqc = list(enable = TRUE),
    postprocess = list(enable = TRUE, clean = list(bids_desc = "clean",
      input_regex = "desc:preproc suffix:bold", validate_postproc_steps = TRUE)),
    extract_rois = list(enable = TRUE, network = list(input_streams = "clean",
      atlases = file.path(root, "atlas_demo.nii.gz"), save_ts = TRUE,
      save_diagnostics = TRUE, correlation = list(method = "pearson")))),
    class = "bg_project_cfg")
  list(root = root, cfg = cfg, db = db)
}

# Write small evidence files; the collector must never load their image arrays.
write_qc_fixture_file <- function(path, text = "placeholder image") {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(text, path)
  normalizePath(path, winslash = "/")
}

test_that("QC path identities normalize filesystem aliases", {
  root <- tempfile("qc-path-identity-")
  path <- write_qc_fixture_file(file.path(root, "nested", "artifact.txt"))
  on.exit(unlink(root, recursive = TRUE))
  alias <- file.path(root, "nested", ".", "artifact.txt")

  expect_identical(
    BrainGnomes:::qc_path_key(path),
    BrainGnomes:::qc_path_key(alias)
  )
})

test_that("QC path identities normalize missing targets and preserve sentinels", {
  root <- tempfile("qc-missing-identity-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  target <- file.path(root, "future", "artifact.nii.gz")
  alias <- file.path(root, ".", "future", "artifact.nii.gz")
  expect_identical(qc_path_key(c(alias, NA_character_, "")),
    c(qc_path_key(target), NA_character_, ""))
  expect_false(file.exists(target))
})

# Add one acquisition, matching derivative evidence, and an optional failed job.
populate_qc_fixture <- function(fixture, subject = "001", run = "01", failed = FALSE) {
  cfg <- fixture$cfg
  prefix <- paste0("sub-", subject, "_ses-A_task-rest_acq-fast_dir-AP_run-", run)
  raw <- write_qc_fixture_file(file.path(cfg$metadata$bids_directory,
    paste0("sub-", subject), "ses-A", "func", paste0(prefix, "_bold.nii.gz")))
  preproc <- write_qc_fixture_file(file.path(cfg$metadata$fmriprep_directory,
    paste0("sub-", subject), "ses-A", "func", paste0(prefix, "_space-MNI152NLin6Asym_desc-preproc_bold.nii.gz")))
  confounds <- write_qc_fixture_file(qc_find_confounds(preproc),
    c("framewise_displacement\ttrans_x", "n/a\t0", "0.1\t1", "0.5\t2", "0.3\t3"))
  expected <- qc_expected_derivatives(cfg, preproc, new.env())
  expected <- expected[expected$sub_id %in% subject & expected$run %in% run, , drop = FALSE]
  pp <- expected$derivative_file[expected$stage == "postprocess"]
  rois <- expected$derivative_file[expected$stage == "extract_rois"]
  if (!failed) {
    write_qc_fixture_file(pp)
    for (roi in rois) write_qc_fixture_file(roi, "roi1\troi2\n0.1\tNA")
  }
  censor <- write_qc_fixture_file(get_censor_file(as.list(extract_bids_info(pp))), c("1", "0", "1", "1"))
  audit <- file.path(cfg$metadata$log_directory, paste0("sub-", subject),
    sub("\\.nii.gz$", "_postproc-validation.json", basename(pp)))
  dir.create(dirname(audit), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(list(schema_version = "postproc-validation-v1",
    intended_final_file = pp, pipeline_completed = !failed,
    overall_status = if (failed) "failed" else "passed",
    checks = list(list(step = "apply_mask", status = if (failed) "failed" else "passed",
      message = "Mask replay result"))), audit, auto_unbox = TRUE)
  diag <- sub("_timeseries.tsv$", "_roidiagnostics.tsv", rois[grepl("_timeseries.tsv$", rois)])
  write_qc_fixture_file(diag, c("roi\tretained\tproportion_usable\texclusion_reason",
    "roi1\tTRUE\t0.95\tNA", "roi2\tFALSE\t0.02\tbelow_threshold"))
  report <- write_qc_fixture_file(file.path(cfg$metadata$fmriprep_directory,
    paste0("sub-", subject, ".html")), "<html>Evidence report</html>")
  mriqc <- file.path(cfg$metadata$mriqc_directory, paste0("sub-", subject), "ses-A",
    "func", paste0(prefix, "_bold.json"))
  dir.create(dirname(mriqc), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(list(fd_mean = 0.25, tsnr = 43.2, dvars_std = 1.1), mriqc, auto_unbox = TRUE)
  insert_tracked_job(fixture$db, paste0(subject, run), list(
    job_name = paste0("postprocess_clean_sub-", subject, "_ses-A"),
    sequence_id = "run-current", status = if (failed) "FAILED" else "COMPLETED",
    stage = "postprocess", stream = "clean", sub_id = subject, ses_id = "A"))
  list(raw = raw, preproc = preproc, pp = pp, rois = rois, audit = audit,
    confounds = confounds, censor = censor, diag = diag, report = report, mriqc = mriqc)
}

test_that("QC retains failed audits for missing outputs under symlink aliases", {
  skip_on_os("windows") # Creating directory symlinks requires extra privileges.
  root <- tempfile("qc-real-root-")
  dir.create(root)
  alias <- paste0(root, "-alias")
  on.exit(unlink(c(alias, root), recursive = TRUE))
  skip_if_not(file.symlink(root, alias), "Directory symlinks unavailable")
  fixture <- make_qc_fixture(alias)
  paths <- populate_qc_fixture(fixture, failed = TRUE)
  expect_false(file.exists(paths$pp))

  result <- collect_qc_inventory(fixture$cfg)
  expect_identical(subset(result$inventory, stage == "postprocess")$validation_status,
    "failed")
  expect_false(any(grepl("matching target", result$issues$message, fixed = TRUE)))

  # A different missing filename must remain distinct, even under the alias.
  audit <- jsonlite::read_json(paths$audit)
  audit$intended_final_file <- paste0(paths$pp, "-different")
  jsonlite::write_json(audit, paths$audit, auto_unbox = TRUE)
  unmatched <- collect_qc_inventory(fixture$cfg)
  expect_identical(subset(unmatched$inventory, stage == "postprocess")$validation_status,
    "invalid")
})

test_that("empty projects produce typed inventories and retain explicit expectations", {
  fixture <- make_qc_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE))
  result <- collect_qc_inventory(fixture$cfg)
  expect_s3_class(result, "bg_qc_inventory")
  expect_equal(nrow(result$inventory), 0L)
  expect_type(result$inventory$derivative_file, "character")
  expect_type(result$metrics$value, "double")
  empty_scope <- collect_qc_inventory(fixture$cfg, subjects = data.frame(sub_id = character()))
  expect_equal(nrow(empty_scope$workflow), 0L)
  result <- collect_qc_inventory(fixture$cfg, subjects = data.frame(sub_id = "007", ses_id = "baseline"))
  expect_equal(nrow(result$workflow), 4L)
  expect_true(all(result$workflow$status == "NOT_TRACKED"))
  expect_identical(unique(result$workflow$sub_id), "007")
  expect_output(print(result), "4 workflow units")
})

test_that("collection preserves identities, missing outputs, evidence, and metric definitions", {
  fixture <- make_qc_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE))
  paths <- populate_qc_fixture(fixture)
  populate_qc_fixture(fixture, "002", failed = TRUE)
  before <- unname(tools::md5sum(fixture$db))
  result <- collect_qc_inventory(fixture$cfg)
  expect_identical(unname(tools::md5sum(fixture$db)), before)
  expect_equal(nrow(result$acquisitions), 2L)
  expect_setequal(result$acquisitions$sub_id, c("001", "002"))
  expect_true(all(result$acquisitions$acquisition == "fast"))
  pp <- subset(result$inventory, stage == "postprocess")
  expect_identical(pp$output_status, c("available", "missing"))
  expect_identical(pp$validation_status, c("passed", "failed"))
  expect_true(pp$attention[2L])
  expect_equal(subset(result$metrics, metric == "mean_fd")$value, c(0.3, 0.3))
  expect_equal(subset(result$metrics, metric == "mean_fd")$n_missing, c(1L, 1L))
  expect_true(all(subset(result$metrics, metric == "censor_retained_percent")$value == 75))
  expect_true(all(subset(result$metrics, metric == "roi_retained_percent")$value == 50))
  expect_true(all(subset(result$metrics, metric == "mriqc_tsnr")$value == 43.2))
  expect_true(all(subset(result$inventory, stage == "extract_rois")$diagnostics_status == "roi_loss"))
  report_ids <- subset(result$evidence, path == paths$report)$record_id
  expect_true(all(result$inventory$sub_id[match(report_ids, result$inventory$record_id)] == "001"))
  expect_false(any(c("reviewer", "decision", "included") %in% names(result$inventory)))
})

test_that("malformed and absent evidence cannot become a QC pass", {
  fixture <- make_qc_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE))
  paths <- populate_qc_fixture(fixture)
  writeLines("{invalid json", paths$audit)
  writeLines(c("1", "9"), paths$censor)
  writeLines("wrong\tcolumns\n1\t2", paths$diag)
  result <- collect_qc_inventory(fixture$cfg)
  expect_equal(subset(result$inventory, stage == "postprocess")$validation_status, "invalid")
  expect_true(all(subset(result$inventory, stage == "extract_rois")$diagnostics_status == "invalid"))
  expect_false(any(result$metrics$metric == "censor_retained_percent"))
  expect_gte(nrow(result$issues), 3L)
  unlink(paths$audit)
  result <- collect_qc_inventory(fixture$cfg)
  expect_equal(subset(result$inventory, stage == "postprocess")$validation_status, "missing")
})

test_that("historical manifest associations do not imply production by the latest attempt", {
  fixture <- make_qc_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE))
  paths <- populate_qc_fixture(fixture)
  manifest <- capture_output_manifest(dirname(paths$pp), files = paths$pp)
  con <- DBI::dbConnect(RSQLite::SQLite(), fixture$db)
  DBI::dbExecute(con, "UPDATE job_tracking SET output_manifest = ?, time_submitted = '2026-09-01 10:00:00'", params = list(manifest))
  insert_tracked_job(fixture$db, "retry", list(job_name = "postprocess_clean_sub-001_ses-A",
    sequence_id = "run-retry", status = "FAILED", stage = "postprocess", stream = "clean", sub_id = "001", ses_id = "A"))
  DBI::dbExecute(con, "UPDATE job_tracking SET time_submitted = '2026-09-02 10:00:00' WHERE job_id = 'retry'")
  DBI::dbDisconnect(con)
  result <- collect_qc_inventory(fixture$cfg)
  expect_equal(subset(result$workflow, stage == "postprocess")$status, "FAILED")
  expect_equal(subset(result$inventory, stage == "postprocess")$output_status, "available")
  expect_equal(subset(result$inventory, stage == "postprocess")$workflow_status, "FAILED")
  expect_equal(subset(result$inventory, stage == "postprocess")$workflow_scope, "participant_session")
  expect_false(result$manifest_files$is_current_attempt)
  expect_true(result$manifest_files$matches_recorded_file)
  writeLines("changed image bytes", paths$pp)
  result <- collect_qc_inventory(fixture$cfg)
  expect_equal(result$manifest_files$status, "changed")
  expect_equal(nrow(result$jobs), 2L)
})

test_that("TSV and RDS exports retain the same snapshot and protect prior snapshots", {
  fixture <- make_qc_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE))
  populate_qc_fixture(fixture)
  result <- collect_qc_inventory(fixture$cfg)
  directory <- file.path(fixture$root, "export")
  expect_path_identical(write_qc_inventory(result, directory), directory)
  expect_identical(readRDS(file.path(directory, "inventory.rds")), result)
  observed <- data.table::fread(file.path(directory, "inventory.tsv"), colClasses = "character")
  expect_setequal(observed$record_id, result$inventory$record_id)
  expect_true(all(observed$sub_id == "001"))
  expect_error(write_qc_inventory(result, directory), "new or empty")
  metadata <- jsonlite::read_json(file.path(directory, "snapshot.json"))
  expect_equal(metadata$snapshot_id, result$metadata$snapshot_id)
})

test_that("interactive components handle empty data and escape report content", {
  skip_if_not_installed("reactable")
  skip_if_not_installed("plotly")
  fixture <- make_qc_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE))
  empty <- collect_qc_inventory(fixture$cfg)
  expect_s3_class(qc_dashboard_widgets(empty, fixture$root)$inventory, "reactable")
  populate_qc_fixture(fixture)
  result <- collect_qc_inventory(fixture$cfg)
  widgets <- qc_dashboard_widgets(result, fixture$root)
  expect_s3_class(widgets$inventory, "reactable")
  expect_s3_class(widgets$metric_plot, "plotly")
  link <- qc_evidence_links(data.frame(kind = "<script>bad</script>",
    path = file.path(fixture$root, "a & b.html"), exists = TRUE), fixture$root)
  markup <- as.character(link)
  expect_match(markup, "&lt;script&gt;", fixed = TRUE)
  expect_match(markup, "a%20%26%20b.html", fixed = TRUE)
})

test_that("details use one inert canonical payload and lazy JavaScript callbacks", {
  for (package in c("reactable", "plotly", "htmltools", "htmlwidgets")) skip_if_not_installed(package)
  fixture <- make_qc_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE))
  populate_qc_fixture(fixture)
  snapshot <- collect_qc_inventory(fixture$cfg)
  snapshot$checks$message[1L] <- "</script><img src=x onerror=alert(1)>"
  original <- snapshot
  widgets <- qc_dashboard_widgets(snapshot, fixture$root)
  json <- as.character(widgets$payload$children[[1L]])
  expect_false(grepl("</script>", json, fixed = TRUE))
  payload <- jsonlite::fromJSON(json)
  expect_identical(payload$checks$message, snapshot$checks$message)
  expect_equal(nrow(payload$metrics), nrow(snapshot$metrics))
  for (name in c("inventory", "attention", "metrics", "checks", "jobs")) {
    columns <- widgets[[name]]$x$tag$attribs$columns
    callbacks <- lapply(columns, function(column) column$details)
    callbacks <- Filter(Negate(is.null), callbacks)
    expect_length(callbacks, 1L)
    expect_s3_class(callbacks[[1L]], "JS_EVAL")
  }
  ids <- vapply(widgets[vapply(widgets, inherits, logical(1), "reactable")],
    function(widget) widget$x$tag$attribs$elementId, character(1))
  expect_false(anyDuplicated(ids) > 0L)
  expect_false("qc-metrics" %in% ids)
  expect_identical(snapshot, original)
})

test_that("embedded details omit unused tracking history without truncating exports", {
  skip_if_not_installed("htmltools")
  fixture <- make_qc_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE))
  populate_qc_fixture(fixture)
  snapshot <- collect_qc_inventory(fixture$cfg)
  snapshot$jobs$output_manifest <- rep("UNUSED_SERIALIZED_JOB_MANIFEST", nrow(snapshot$jobs))
  manifests <- snapshot$manifest_files[rep(NA_integer_, 2L), , drop = FALSE]
  manifests$path <- c(snapshot$inventory$derivative_file[1L], file.path(fixture$root, "unrelated.txt"))
  manifests$status <- c("matches_size_and_mtime", "changed")
  snapshot$manifest_files <- manifests
  original <- snapshot
  metrics <- qc_metric_observations(snapshot$metrics, snapshot$inventory)
  payload <- qc_dashboard_payload(snapshot, snapshot$inventory, metrics, fixture$root)
  decoded <- jsonlite::fromJSON(as.character(payload$children[[1L]]))
  expect_false("output_manifest" %in% names(decoded$jobs))
  expect_setequal(decoded$jobs$job_id, snapshot$jobs$job_id)
  expect_true(all(c("sequence_id", "attempt", "provenance_path") %in% names(decoded$jobs)))
  expect_identical(decoded$manifests$path, snapshot$inventory$derivative_file[1L])
  expect_identical(decoded$manifests$status, "matches_size_and_mtime")
  expect_identical(snapshot, original)
  exported <- write_qc_inventory(snapshot, file.path(fixture$root, "export"))
  expect_identical(readRDS(file.path(exported, "inventory.rds")), original)
  expect_equal(nrow(data.table::fread(file.path(exported, "manifest_files.tsv"))), 2L)
  expect_identical(data.table::fread(file.path(exported, "jobs.tsv"))$output_manifest,
    "UNUSED_SERIALIZED_JOB_MANIFEST")
})

test_that("raw identities and missing upstream work survive collection", {
  fixture <- make_qc_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE))
  for (echo in c("1", "2")) write_qc_fixture_file(file.path(
    fixture$cfg$metadata$bids_directory, "sub-010", "ses-B", "func",
    paste0("sub-010_ses-B_task-rest_acq-fast_rec-test_dir-PA_run-02_echo-", echo, "_part-mag_bold.nii.gz")))
  scope_path <- file.path(fixture$cfg$metadata$log_directory, "runs", "planned", "subjects.tsv")
  write_qc_fixture_file(scope_path, c("sub_id\tses_id", "009\tA"))
  result <- collect_qc_inventory(fixture$cfg)
  expect_equal(nrow(result$acquisitions), 2L)
  expect_setequal(result$acquisitions$echo, c("1", "2"))
  expect_true(all(result$acquisitions$part == "mag"))
  expect_setequal(result$workflow$sub_id, c("009", "010"))
  expect_equal(nrow(result$inventory), 0L)
  expect_true(all(result$workflow$status == "NOT_TRACKED"))
})

test_that("existing native products remain visible when upstream data are absent", {
  fixture <- make_qc_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE))
  paths <- populate_qc_fixture(fixture)
  unlink(paths$preproc)
  result <- collect_qc_inventory(fixture$cfg)
  expect_true(norm_path(paths$pp) %in% norm_path(result$inventory$derivative_file))
  expect_true(all(norm_path(paths$rois) %in% norm_path(result$inventory$derivative_file)))
  expect_true(all(is.na(result$inventory$source_file)))
  expect_true(all(result$inventory$expectation_basis == "discovered"))
})

test_that("shared stream destinations and malformed manifests are explicit issues", {
  fixture <- make_qc_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE))
  populate_qc_fixture(fixture)
  fixture$cfg$postprocess$duplicate <- fixture$cfg$postprocess$clean
  con <- DBI::dbConnect(RSQLite::SQLite(), fixture$db)
  DBI::dbExecute(con, "UPDATE job_tracking SET output_manifest = ?", params = list(
    '{"output_dir":"/tmp","files":[{"path":"missing.nii.gz"}]}'))
  DBI::dbDisconnect(con)
  result <- collect_qc_inventory(fixture$cfg)
  expect_true(all(subset(result$inventory, stage == "postprocess")$association_status == "shared_destination"))
  expect_true(any(grepl("share this destination", result$issues$message)))
  expect_true(any(grepl("lacks a valid path", result$issues$message)))
})

test_that("Quarto can render a snapshot without a project or running R server", {
  skip_if(Sys.getenv("BRAINGNOMES_TEST_QUARTO") != "true", "Set BRAINGNOMES_TEST_QUARTO=true for real Quarto rendering")
  skip_if(!nzchar(Sys.which("quarto")), "Quarto is unavailable")
  for (package in c("reactable", "plotly", "htmltools", "htmlwidgets", "knitr", "rmarkdown")) skip_if_not_installed(package)
  fixture <- make_qc_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE))
  result <- collect_qc_inventory(fixture$cfg)
  html <- render_qc_browser_fixture(result, file.path(fixture$root, "dashboard"))
  expect_true(file.exists(html))
  expect_match(paste(readLines(html, warn = FALSE), collapse = "\n"), "reactable")
  expect_true(file.exists(file.path(dirname(html), "inventory.tsv")))
  expect_identical(readRDS(file.path(dirname(html), "inventory.rds")), result)
})

test_that("unknown atlas identities do not pool different diagnostic directories", {
  skip_if_not_installed("plotly")
  inventory <- data.frame(record_id = c("a", "b"), sub_id = c("001", "002"),
    ses_id = NA_character_, run = "01", stage = "extract_rois", stream = NA_character_,
    input_stream = NA_character_, atlas = NA_character_, task = "rest",
    space = "MNI", resolution = NA_character_)
  metrics <- data.frame(record_id = c("a", "b"), metric = "roi_retained_percent",
    source_file = c("/rois/atlasA/sub-001_roidiagnostics.tsv", "/rois/atlasB/sub-002_roidiagnostics.tsv"),
    value = c(90, 95), unit = "%", n_observations = 10L, n_missing = 0L)
  rows <- qc_metric_observations(metrics, inventory)
  expect_length(unique(rows$group_id), 2L)
})

test_that("disabled, missing required, and unknown diagnostics stay distinct", {
  fixture <- make_qc_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE))
  paths <- populate_qc_fixture(fixture)
  unlink(c(paths$audit, paths$censor, paths$diag))
  fixture$cfg$postprocess$clean$scrubbing <- list(enable = FALSE)
  fixture$cfg$extract_rois$network$save_diagnostics <- FALSE
  x <- collect_qc_inventory(fixture$cfg)
  pp <- subset(x$inventory, stage == "postprocess")
  expect_equal(pp$validation_status, "missing")
  expect_equal(pp$diagnostics_status, "not_applicable")
  expect_match(pp$attention_reasons, "Saved audit missing", fixed = TRUE)
  expect_true(all(subset(x$inventory, stage == "extract_rois")$diagnostics_status == "not_applicable"))
  expect_true(all(subset(x$inventory, stage == "fmriprep")$validation_status == "not_applicable"))
  fixture$cfg$postprocess$clean$validate_postproc_steps <- FALSE
  fixture$cfg$postprocess$clean$scrubbing$enable <- TRUE
  pp <- subset(collect_qc_inventory(fixture$cfg)$inventory, stage == "postprocess")
  expect_equal(pp$validation_status, "not_applicable")
  expect_equal(pp$diagnostics_status, "missing")
  fixture$cfg$postprocess$clean$scrubbing <- NULL
  pp <- subset(collect_qc_inventory(fixture$cfg)$inventory, stage == "postprocess")
  expect_equal(pp$diagnostics_status, "unavailable")
})

test_that("saved checks retain warnings, timing, and numeric structured measurements", {
  fixture <- make_qc_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE))
  paths <- populate_qc_fixture(fixture)
  audit <- jsonlite::read_json(paths$audit)
  audit$updated_at <- "2026-10-10T10:00:01Z"
  audit$checks[[1L]]$warnings <- c("Calibration warning", "Inspect saved mask")
  audit$checks[[1L]]$started_at <- "2026-10-10T10:00:00Z"
  audit$checks[[1L]]$elapsed_seconds <- 1.25
  audit$checks[[1L]]$details <- list(max_absolute_error = 0.002,
    n_mismatched = 3L, volumes_compared = 4L, samples = c(1, 3, 4))
  jsonlite::write_json(audit, paths$audit, auto_unbox = TRUE)
  x <- collect_qc_inventory(fixture$cfg)
  expect_equal(x$checks$elapsed_seconds, 1.25)
  expect_equal(x$checks$audited_at, audit$updated_at)
  expect_equal(jsonlite::fromJSON(x$checks$details_json)$samples, c(1, 3, 4))
  expect_match(x$checks$warnings, "Calibration warning\nInspect saved mask", fixed = TRUE)
  expect_true(subset(x$inventory, stage == "postprocess")$audit_warnings)
  expect_match(subset(x$inventory, stage == "postprocess")$attention_reasons, "warnings")
  metric <- subset(x$metrics, metric == "apply_mask: max_absolute_error")
  expect_equal(metric$value, 0.002)
  expect_equal(metric$n_observations, 4L)
  expect_equal(qc_display_identity(x$checks, x$inventory)$sub_id, "001")
})

test_that("saved run scope uses recorded settings rather than current configuration", {
  fixture <- make_qc_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE))
  populate_qc_fixture(fixture)
  populate_qc_fixture(fixture, "002")
  path <- file.path(fixture$cfg$metadata$log_directory, "runs", "run-current", "provenance.json")
  dir.create(dirname(path), recursive = TRUE)
  saved <- list(schema_version = "brain-gnomes-run-provenance-v1", run_id = "run-current",
    recorded_at = "2026-10-10T10:00:00Z", configuration = list(values = unclass(fixture$cfg)),
    request = list(steps = "postprocess", postprocess_streams = "clean"),
    execution = list(scope_status = "resolved", subjects = data.frame(sub_id = "001", ses_id = "A")))
  jsonlite::write_json(saved, path, auto_unbox = TRUE, na = "null")
  fixture$cfg$postprocess$clean$bids_desc <- "changed"
  x <- collect_qc_inventory(fixture$cfg, run_id = "latest")
  expect_equal(x$metadata$run_id, "run-current")
  expect_equal(x$metadata$scope, "run")
  expect_equal(unique(x$acquisitions$sub_id), "001")
  expect_equal(unique(x$inventory$sub_id), "001")
  expect_equal(unique(x$workflow$stage), "postprocess")
  expect_equal(subset(x$inventory, stage == "postprocess")$output_status, "available")
  expect_equal(subset(x$inventory, stage == "fmriprep")$record_role, "upstream_input")
  expect_equal(qc_dashboard_summary(x)$totals$outputs, 1L)
  expect_equal(qc_dashboard_summary(x)$totals$upstream_inputs, 1L)
  # An input can need attention even though selected output counts exclude it.
  flagged <- x
  flagged$inventory$attention[flagged$inventory$record_role == "upstream_input"] <- TRUE
  expect_equal(qc_dashboard_summary(flagged)$participants$attention_records, 1L)
  expect_error(collect_qc_inventory(fixture$cfg, subjects = data.frame(sub_id = "003"),
    run_id = "latest"), "do not combine")
  saved$execution$scope_status <- "deferred"
  jsonlite::write_json(saved, path, auto_unbox = TRUE, na = "null")
  expect_error(collect_qc_inventory(fixture$cfg, run_id = "latest"), "unresolved")
})

test_that("run row filtering retains exact retry units and participant-wide reports", {
  cfg <- list(postprocess = list(enable = TRUE, a = list(), b = list()),
    extract_rois = list(enable = FALSE))
  context <- list(run_id = "retry", steps = "postprocess", config = cfg,
    subjects = data.frame(sub_id = c("001", "002"), ses_id = c("A", "B")),
    work_units = data.frame(sub_id = c("001", "002"), ses_id = c("A", "B"),
      stage = "postprocess", stream = c("a", "b")))
  rows <- expand.grid(sub_id = c("001", "002"), ses_id = c("A", "B"),
    stage = "postprocess", stream = c("a", "b"), stringsAsFactors = FALSE)
  expect_equal(sum(qc_scope_rows(rows, context)), 2L)
  reports <- data.frame(sub_id = c("001", "002", "003"), ses_id = NA_character_)
  expect_equal(qc_scope_rows(reports, context, subject_evidence = TRUE), c(TRUE, TRUE, FALSE))
  expect_false(any(qc_scope_rows(reports, context)))
  context$subjects <- data.frame(sub_id = "001", ses_id = NA_character_)
  context$work_units <- NULL
  later_sessions <- data.frame(sub_id = "001", ses_id = c(NA_character_, "added-later"))
  expect_equal(qc_scope_rows(later_sessions, context), c(TRUE, FALSE))
})

test_that("manifest summaries count duplicate associations separately from paths", {
  files <- data.frame(stage = "postprocess", stream = "clean",
    status = c("matches_size_and_mtime", "matches_size_and_mtime", "missing"),
    path = c("/one", "/one", "/two"))
  summary <- qc_manifest_summary(files)
  matched <- subset(summary, status == "matches_size_and_mtime")
  expect_equal(matched$associations, 2L)
  expect_equal(matched$unique_paths, 1L)
})

test_that("version 1.0 snapshots remain renderable without inventing audit requirements", {
  for (package in c("reactable", "plotly", "htmltools", "htmlwidgets")) skip_if_not_installed(package)
  fixture <- make_qc_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE))
  for (populated in c(FALSE, TRUE)) {
    if (populated) populate_qc_fixture(fixture)
    legacy <- collect_qc_inventory(fixture$cfg)
    legacy$inventory[c("record_role", "attention_reasons", "validation_required",
      "diagnostics_required", "audit_warnings")] <- NULL
    legacy$checks[c("warnings", "started_at", "audited_at", "elapsed_seconds", "details_json")] <- NULL
    legacy$metadata[c("settings", "scope", "run_id", "selected_steps")] <- NULL
    original <- legacy
    summary <- qc_dashboard_summary(legacy)
    expect_equal(summary$totals$outputs, nrow(legacy$inventory))
    expect_equal(summary$totals$required_audits, 0L)
    widgets <- qc_dashboard_widgets(legacy, fixture$root)
    expect_identical(widgets$summary, summary)
    expect_s3_class(widgets$inventory, "reactable")
    expect_identical(legacy, original)
  }
})

test_that("render failures retain exports and can resume without recollection", {
  skip_on_os("windows")
  for (package in c("reactable", "plotly", "htmltools", "htmlwidgets", "knitr", "rmarkdown")) skip_if_not_installed(package)
  fixture <- make_qc_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE))
  x <- collect_qc_inventory(fixture$cfg)
  output <- file.path(fixture$root, "report")
  expect_error(render_qc_dashboard(x, output, quarto = ""), "Quarto")
  expect_identical(readRDS(file.path(output, "inventory.rds")), x)
  expect_equal(jsonlite::read_json(file.path(output, "render-status.json"))$state, "failed")
  renderer <- file.path(fixture$root, "fake-quarto")
  writeLines(c("#!/bin/sh", "if [ \"$1\" = \"--version\" ]; then echo 1.9.0; exit 0; fi",
    "echo 'intentional renderer failure' >&2", "exit 17"), renderer)
  Sys.chmod(renderer, "0755")
  before <- tools::md5sum(file.path(output, c("inventory.rds", "inventory.tsv", "snapshot.json")))
  cache_before <- Sys.getenv("XDG_CACHE_HOME", unset = NA_character_)
  expect_error(render_qc_dashboard(output, quarto = renderer), "exit code 17")
  expect_match(paste(readLines(file.path(output, "render.log")), collapse = "\n"), "intentional renderer failure")
  expect_equal(jsonlite::read_json(file.path(output, "render-status.json"))$exit_code, 17L)
  expect_identical(Sys.getenv("XDG_CACHE_HOME", unset = NA_character_), cache_before)
  writeLines(c("#!/bin/sh", "if [ \"$1\" = \"--version\" ]; then echo 1.9.0; exit 0; fi",
    "test -d \"$XDG_CACHE_HOME\" || exit 2", "echo '<html>rendered</html>' > index.html"), renderer)
  html <- render_qc_dashboard(output, quarto = renderer)
  expect_true(file.exists(html))
  expect_equal(jsonlite::read_json(file.path(output, "render-status.json"))$state, "complete")
  expect_identical(tools::md5sum(names(before)), before)
  expect_error(render_qc_dashboard(output, quarto = renderer), "already exists")
})

test_that("browser stress fixtures preserve multi-run joins and typed empty tables", {
  fixture <- make_qc_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE))
  populate_qc_fixture(fixture, run = "01")
  populate_qc_fixture(fixture, run = "02")
  snapshot <- collect_qc_inventory(fixture$cfg)
  expanded <- expand_qc_browser_fixture(snapshot, participants = 3L)
  expect_equal(nrow(expanded$acquisitions), 6L)
  expect_setequal(expanded$inventory$sub_id, c("001", "002", "003"))
  expect_true(all(expanded$metrics$record_id %in% expanded$inventory$record_id))
  expect_identical(expanded$manifest_files, snapshot$manifest_files)
  fd <- qc_metric_sources(qc_metric_observations(expanded$metrics, expanded$inventory))
  fd <- subset(fd, metric == "mean_fd")
  expect_equal(nrow(fd), 6L)
  expect_equal(sum(is.na(fd$value)), 2L)
})

test_that("coverage exposes raw acquisitions without resolved upstream outputs", {
  fixture <- make_qc_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE))
  populate_qc_fixture(fixture)
  raw <- file.path(fixture$cfg$metadata$bids_directory, "sub-002", "func",
    "sub-002_task-rest_run-01_bold.nii.gz")
  write_qc_fixture_file(raw)
  snapshot <- collect_qc_inventory(fixture$cfg, subjects = data.frame(sub_id = "003"), quiet = TRUE)
  coverage <- snapshot$coverage
  expect_equal(subset(coverage, sub_id == "002" & stage == "fmriprep")$coverage_status,
    "unresolved_outputs")
  expect_true(all(subset(coverage, sub_id == "002")$attention))
  expect_true(all(subset(coverage, sub_id == "003")$coverage_status == "inputs_not_observed"))
  queue <- qc_review_queue(snapshot$inventory, coverage, snapshot$metadata)
  expect_true(any(queue$sub_id %in% "002" & queue$products == 0L))
  expect_gt(subset(qc_dashboard_summary(snapshot)$participants, sub_id == "002")$review_items, 0L)
  expect_equal(subset(qc_dashboard_summary(snapshot)$participants, sub_id == "002")$output_records, 0L)
  expect_true(all(subset(coverage, sub_id == "001" & stage == "fmriprep")$coverage_status == "observed"))
  output <- file.path(fixture$root, "export")
  write_qc_inventory(snapshot, output)
  expect_true(file.exists(file.path(output, "coverage.tsv")))
  snapshot$coverage <- NULL
  expect_equal(qc_snapshot_coverage(snapshot), coverage)
})

test_that("coverage does not multiply raw acquisitions by output spaces", {
  fixture <- make_qc_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE))
  paths <- populate_qc_fixture(fixture)
  second_space <- sub("space-MNI152NLin6Asym", "space-T1w", paths$preproc, fixed = TRUE)
  write_qc_fixture_file(second_space)
  snapshot <- collect_qc_inventory(fixture$cfg, quiet = TRUE)
  coverage <- subset(snapshot$coverage, stage == "fmriprep")
  expect_equal(nrow(coverage), 1L)
  expect_equal(coverage$resolved_outputs, 2L)
  expect_equal(coverage$coverage_status, "observed")
})

test_that("saved mixed-stage retries preserve exact AROMA scope and submission uncertainty", {
  fixture <- make_qc_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE))
  populate_qc_fixture(fixture, "001")
  populate_qc_fixture(fixture, "002")
  units <- data.frame(stage = c("postprocess", "postprocess", "aroma"),
    sub_id = c("001", "002", "002"), ses_id = c("A", "A", NA), stream = c("clean", "clean", NA))
  saved <- list(schema_version = "brain-gnomes-run-provenance-v1", run_id = "run-current",
    state = "submission_started", recorded_at = "2026-10-10T10:00:00Z",
    configuration = list(values = unclass(fixture$cfg)),
    request = list(steps = c("postprocess", "aroma"), postprocess_streams = "clean", work_units = units),
    submission = list(submitted_job_ids = character(), tracked_jobs = data.frame()),
    execution = list(scope_status = "resolved", subjects = data.frame(sub_id = c("001", "002"), ses_id = "A")))
  path <- file.path(fixture$cfg$metadata$log_directory, "runs", "run-current", "provenance.json")
  dir.create(dirname(path), recursive = TRUE)
  jsonlite::write_json(saved, path, auto_unbox = TRUE, na = "null")
  snapshot <- collect_qc_inventory(fixture$cfg, run_id = "run-current", quiet = TRUE)
  expect_equal(nrow(snapshot$workflow), 3L)
  expect_equal(subset(snapshot$workflow, stage == "aroma")$sub_id, "002")
  expect_false(any(snapshot$coverage$stage == "aroma" & snapshot$coverage$sub_id == "001"))
  expect_equal(snapshot$metadata$submission$acknowledged_jobs, 0L)
  expect_equal(snapshot$metadata$submission$state, "submission_started")
  queue <- qc_review_queue(snapshot$inventory, snapshot$coverage, snapshot$metadata)
  expect_true(any(queue$category == "Submission unconfirmed"))
  banner <- as.character(qc_scope_banner(snapshot$metadata))
  expect_match(banner, "Saved run: run-current", fixed = TRUE)
  expect_match(banner, "submission_started", fixed = TRUE)
  saved$state <- "submitted"
  saved$submission$submitted_job_ids <- "12345"
  jsonlite::write_json(saved, path, auto_unbox = TRUE, na = "null")
  acknowledged <- collect_qc_inventory(fixture$cfg, run_id = "run-current", quiet = TRUE)
  expect_false(any(qc_review_queue(acknowledged$inventory, acknowledged$coverage,
    acknowledged$metadata)$category == "Submission unconfirmed"))
  saved$request$work_units <- list(stage = "aroma")
  jsonlite::write_json(saved, path, auto_unbox = TRUE, na = "null")
  expect_error(collect_qc_inventory(fixture$cfg, run_id = "run-current", quiet = TRUE), "stream")
})

test_that("legacy filename stream labels stay diagnosable without polluting selectors", {
  fixture <- make_qc_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE))
  paths <- populate_qc_fixture(fixture)
  label <- basename(paths$preproc)
  insert_tracked_job(fixture$db, "legacy-job", list(job_name = paste0("postprocess_", label),
    sequence_id = "older-run", status = "FAILED", stage = "postprocess", sub_id = "001", ses_id = "A"))
  snapshot <- collect_qc_inventory(fixture$cfg, quiet = TRUE)
  unresolved <- subset(snapshot$workflow, stream_attribution == "unresolved_legacy")
  expect_equal(unresolved$reported_stream, label)
  expect_true(is.na(unresolved$stream))
  expect_true(label %in% snapshot$jobs$stream)
  controls <- as.character(qc_dashboard_controls(snapshot))
  expect_false(grepl(label, controls, fixed = TRUE))
  expect_match(controls, 'value="clean"', fixed = TRUE)
  expect_false(qc_legacy_streams(label, "postprocess", list(postprocess = label)))
})

test_that("exact saved scopes keep sessionless units alongside labeled sessions", {
  fixture <- make_qc_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE))
  empty <- collect_qc_inventory(fixture$cfg, quiet = TRUE)
  inspection <- inspect_project(fixture$cfg, refresh = FALSE)
  state <- new.env(parent = emptyenv())
  state$issues <- list()
  people <- data.frame(sub_id = "001", ses_id = c(NA_character_, "A"))
  workflow <- qc_workflow(fixture$cfg, inspection, empty$acquisitions, empty$inventory,
    people, state, run_scope = TRUE)
  expect_setequal(subset(workflow, stage == "postprocess")$ses_id, c(NA_character_, "A"))
  project_rows <- data.frame(sub_id = NA_character_, ses_id = NA_character_, stage = c("flywheel_sync", "aroma"),
    stream = NA_character_)
  units <- data.frame(stage = "flywheel_sync", stream = NA_character_, sub_id = NA_character_, ses_id = NA_character_)
  expect_identical(qc_retry_rows(project_rows, units), c(TRUE, FALSE))
})

test_that("disabled sections and scrubbing application have explicit explanations", {
  for (package in c("reactable", "plotly", "htmltools", "htmlwidgets")) skip_if_not_installed(package)
  fixture <- make_qc_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE))
  fixture$cfg$postprocess$clean$validate_postproc_steps <- FALSE
  fixture$cfg$postprocess$clean$scrubbing <- list(enable = TRUE, apply = FALSE,
    interpolate = FALSE, expression = "framewise_displacement > 0.5")
  fixture$cfg$extract_rois$enable <- FALSE
  snapshot <- collect_qc_inventory(fixture$cfg, quiet = TRUE)
  widgets <- qc_dashboard_widgets(snapshot, fixture$root)
  expect_match(widgets$validation_note, "Validation not enabled", fixed = TRUE)
  expect_match(widgets$region_note, "ROI extraction not selected", fixed = TRUE)
  settings <- snapshot$metadata$settings
  expect_identical(subset(settings, setting == "Apply scrubbing to volumes")$value, "FALSE")
  expect_identical(subset(settings, setting == "Interpolate scrubbed volumes")$value, "FALSE")
  expect_match(subset(settings, setting == "Scrubbing rule")$value, "framewise_displacement", fixed = TRUE)
  expect_message(collect_qc_inventory(fixture$cfg), "QC: reading configuration")
  expect_silent(collect_qc_inventory(fixture$cfg, quiet = TRUE))
})

test_that("FD reads only its source column once while retaining all record associations", {
  fixture <- make_qc_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE))
  path <- file.path(fixture$root, "wide.tsv")
  writeLines(c("framewise_displacement\tlarge_unused_column", "n/a\tword", "0.2\tother", "0.4\ttext"), path)
  state <- new.env(parent = emptyenv())
  state$issues <- state$evidence <- state$metrics <- list()
  calls <- 0L
  reader <- qc_read_tsv
  local_mocked_bindings(qc_read_tsv = function(path, state, columns = NULL) {
    calls <<- calls + 1L
    expect_identical(columns, "framewise_displacement")
    result <- reader(path, state, columns)
    expect_identical(names(result), "framewise_displacement")
    result
  })
  expect_equal(qc_collect_motion(state, "first-space", path), "available")
  expect_equal(qc_collect_motion(state, "second-space", path), "available")
  expect_equal(calls, 1L)
  metrics <- qc_bind_rows(state$metrics, qc_empty_metrics())
  expect_equal(metrics$value, c(.3, .3))
  expect_equal(metrics$n_observations, c(2L, 2L))
  expect_equal(metrics$n_missing, c(1L, 1L))
  expect_setequal(metrics$record_id, c("first-space", "second-space"))
  writeLines("unrelated\n0.2", path)
  fresh <- new.env(parent = emptyenv())
  fresh$issues <- list()
  expect_identical(reader(path, fresh, "framewise_displacement"), data.frame())
})

test_that("large manifest histories have explicit bounded previews and complete exports", {
  for (package in c("reactable", "plotly", "htmltools", "htmlwidgets")) skip_if_not_installed(package)
  fixture <- make_qc_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE))
  snapshot <- collect_qc_inventory(fixture$cfg, quiet = TRUE)
  snapshot$manifest_files <- data.frame(job_id = as.character(seq_len(503)), stage = "postprocess",
    stream = "clean", status = "missing", path = paste0("/unmounted/", seq_len(503)))
  original <- snapshot
  widgets <- qc_dashboard_widgets(snapshot, fixture$root)
  expect_match(widgets$exception_note, "200 of 503", fixed = TRUE)
  expect_equal(length(jsonlite::fromJSON(widgets$manifest_exceptions$x$tag$attribs$data)$path), 200L)
  output <- file.path(fixture$root, "export")
  write_qc_inventory(snapshot, output)
  expect_equal(nrow(data.table::fread(file.path(output, "manifest_files.tsv"))), 503L)
  expect_identical(readRDS(file.path(output, "inventory.rds")), original)
})

test_that("a 500-participant offline report supports real desktop and mobile review", {
  skip_if(Sys.getenv("BRAINGNOMES_TEST_QC_BROWSER") != "true",
    "Set BRAINGNOMES_TEST_QC_BROWSER=true for the opt-in Chromium walkthrough")
  node <- Sys.which("node")
  skip_if(!nzchar(node) || !nzchar(Sys.which("quarto")), "Node and Quarto are required")
  skip_if(!nzchar(Sys.getenv("BRAINGNOMES_PLAYWRIGHT_MODULE")) ||
    !file.exists(Sys.getenv("BRAINGNOMES_CHROMIUM")), "Set local Playwright module and Chromium paths")
  for (package in c("reactable", "plotly", "htmltools", "htmlwidgets", "knitr", "rmarkdown")) skip_if_not_installed(package)
  fixture <- make_qc_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE))
  populate_qc_fixture(fixture, run = "01")
  populate_qc_fixture(fixture, run = "02")
  snapshot <- expand_qc_browser_fixture(collect_qc_inventory(fixture$cfg))
  snapshot$checks$message[1L] <- '<img src=x onerror="throw new Error(\'unsafe saved text\')">'
  original <- snapshot
  html <- render_qc_browser_fixture(snapshot, file.path(fixture$root, "browser-report"))
  output <- system2(node, c(shQuote(test_path("fixtures", "qc-dashboard-browser.cjs")), shQuote(html)),
    stdout = TRUE, stderr = TRUE)
  expect_null(attr(output, "status"), info = paste(output, collapse = "\n"))
  expect_match(paste(output, collapse = "\n"), "QC browser walkthrough passed", fixed = TRUE)
  expect_identical(readRDS(file.path(dirname(html), "inventory.rds")), original)
})
