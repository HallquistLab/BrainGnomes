test_that("get_project_status reports completion", {
  root <- tempfile("status-")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  log_dir <- file.path(root, "logs"); dir.create(log_dir)
  bids_dir <- file.path(root, "bids"); dir.create(bids_dir)
  fmriprep_dir <- tempfile("fmriprep_"); dir.create(fmriprep_dir)
  postproc_dir <- file.path(root, "postproc"); dir.create(postproc_dir)
  mriqc_dir <- file.path(root, "mriqc"); dir.create(mriqc_dir)


  sub <- "01"; ses_a <- "A"; ses_b <- "B"
  dir.create(file.path(log_dir, paste0("sub-", sub)))
  dir.create(file.path(bids_dir, paste0("sub-", sub), paste0("ses-", ses_a)), recursive = TRUE)
  dir.create(file.path(bids_dir, paste0("sub-", sub), paste0("ses-", ses_b)), recursive = TRUE)
  dir.create(file.path(fmriprep_dir, paste0("sub-", sub)), recursive = TRUE)
  dir.create(file.path(fmriprep_dir, paste0("sub-", sub), paste0("ses-", ses_a)), recursive = TRUE)
  dir.create(file.path(postproc_dir, paste0("sub-", sub)), recursive = TRUE)
  dir.create(file.path(postproc_dir, paste0("sub-", sub), paste0("ses-", ses_a)), recursive = TRUE)

  cat("2024-05-04 10:00:00", file = file.path(log_dir, paste0("sub-", sub), paste0(".bids_conversion_sub-", sub, "_ses-", ses_a, "_complete")))
  cat("2024-05-04 11:00:00", file = file.path(log_dir, paste0("sub-", sub), paste0(".fmriprep_sub-", sub, "_complete")))
  cat("2024-05-04 12:00:00", file = file.path(log_dir, paste0("sub-", sub), paste0(".postprocess_stream1_sub-", sub, "_ses-", ses_a, "_complete")))

  scfg <- list(
    metadata = list(log_directory = log_dir, bids_directory = bids_dir, fmriprep_directory = fmriprep_dir, mriqc_directory = mriqc_dir, postproc_directory = postproc_dir),
    bids_conversion = list(enable = TRUE),
    mriqc = list(enable = FALSE),
    fmriprep = list(enable = TRUE),
    aroma = list(enable = FALSE),
    postprocess = list(enable = TRUE, stream1 = list())
  )
  class(scfg) <- "bg_project_cfg"

  res <- get_project_status(scfg)
  expect_equal(nrow(res), 2)
  expect_true(res$bids_conversion_complete[res$ses_id == ses_a])
  expect_false(res$bids_conversion_complete[res$ses_id == ses_b])
  expect_true(all(res$fmriprep_complete))
  expect_true(res$stream1_complete[res$ses_id == ses_a])
  expect_false(res$stream1_complete[res$ses_id == ses_b])

  sm <- summary(res)
  expect_equal(sm$n_complete[sm$step == "bids_conversion_complete"], 1)
})

test_that("get_project_status returns configured typed columns for an empty project", {
  root <- tempfile("status-empty-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  log_dir <- file.path(root, "logs")
  bids_dir <- file.path(root, "bids")
  dir.create(log_dir)
  dir.create(bids_dir)

  scfg <- structure(list(
    metadata = list(log_directory = log_dir, bids_directory = bids_dir),
    bids_conversion = list(enable = TRUE),
    mriqc = list(enable = TRUE),
    fmriprep = list(enable = FALSE),
    aroma = list(enable = TRUE),
    postprocess = list(enable = TRUE, clean = list()),
    extract_rois = list(enable = TRUE, clean = list(), networks = list())
  ), class = "bg_project_cfg")

  result <- get_project_status(scfg)
  expected_names <- c(
    "sub_id", "ses_id",
    "bids_conversion_complete", "bids_conversion_time",
    "mriqc_complete", "mriqc_time",
    "aroma_complete", "aroma_time",
    "clean_complete", "clean_time",
    "extract_rois_clean_complete", "extract_rois_clean_time",
    "extract_rois_networks_complete", "extract_rois_networks_time"
  )

  expect_s3_class(result, "bg_status_df")
  expect_s3_class(result, "data.frame")
  expect_identical(dim(result), c(0L, length(expected_names)))
  expect_named(result, expected_names)
  expect_identical(result$sub_id, character())
  expect_identical(result$ses_id, character())

  complete_columns <- grep("_complete$", names(result), value = TRUE)
  time_columns <- grep("_time$", names(result), value = TRUE)
  expect_true(all(vapply(result[complete_columns], is.logical, logical(1))))
  expect_true(all(vapply(result[time_columns], inherits, logical(1), what = "POSIXct")))

  status_summary <- summary(result)
  expect_identical(status_summary$step, complete_columns)
  expect_true(all(status_summary$n_complete == 0))
})

test_that("subject and project status report each ROI extraction stream", {
  root <- tempfile("status-extract-rois-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  log_dir <- file.path(root, "logs")
  bids_dir <- file.path(root, "bids")
  postproc_dir <- file.path(root, "postproc")
  rois_dir <- file.path(root, "data_rois")
  for (directory in c(log_dir, bids_dir, postproc_dir, rois_dir)) {
    dir.create(directory)
  }

  sub <- "01"
  sessions <- c("A", "B")
  sub_log_dir <- file.path(log_dir, paste0("sub-", sub))
  dir.create(sub_log_dir)
  for (session in sessions) {
    dir.create(
      file.path(bids_dir, paste0("sub-", sub), paste0("ses-", session)),
      recursive = TRUE
    )
    dir.create(
      file.path(postproc_dir, paste0("sub-", sub), paste0("ses-", session)),
      recursive = TRUE
    )
  }

  marker <- function(step, stream, session, time) {
    path <- file.path(
      sub_log_dir,
      paste0(".", step, "_", stream, "_sub-", sub, "_ses-", session, "_complete")
    )
    writeLines(time, path)
    path
  }
  postprocess_marker <- marker("postprocess", "shared", "A", "2026-08-25 09:00:00")
  shared_marker <- marker("extract_rois", "shared", "A", "2026-08-25 10:00:00")
  alternate_marker <- marker("extract_rois", "alternate", "B", "2026-08-25 11:00:00")

  scfg <- structure(list(
    metadata = list(
      log_directory = log_dir,
      bids_directory = bids_dir,
      postproc_directory = postproc_dir,
      rois_directory = rois_dir
    ),
    bids_conversion = list(enable = FALSE),
    mriqc = list(enable = FALSE),
    fmriprep = list(enable = FALSE),
    aroma = list(enable = FALSE),
    postprocess = list(enable = TRUE, shared = list()),
    extract_rois = list(
      enable = TRUE,
      shared = list(),
      alternate = list()
    )
  ), class = "bg_project_cfg")

  subject_status <- get_subject_status(scfg, sub)
  expect_equal(subject_status$ses_id, sessions)
  expect_named(
    subject_status,
    c(
      "sub_id", "ses_id",
      "shared_complete", "shared_time",
      "extract_rois_shared_complete", "extract_rois_shared_time",
      "extract_rois_alternate_complete", "extract_rois_alternate_time"
    )
  )
  expect_identical(subject_status$shared_complete, c(TRUE, FALSE))
  expect_identical(subject_status$extract_rois_shared_complete, c(TRUE, FALSE))
  expect_identical(subject_status$extract_rois_alternate_complete, c(FALSE, TRUE))
  expect_equal(
    subject_status$shared_time[[1]],
    parse_complete_time(postprocess_marker)
  )
  expect_equal(
    subject_status$extract_rois_shared_time[[1]],
    parse_complete_time(shared_marker)
  )
  expect_equal(
    as.numeric(subject_status$extract_rois_alternate_time[[2]]),
    as.numeric(parse_complete_time(alternate_marker))
  )

  project_status <- get_project_status(scfg)
  expect_equal(project_status, subject_status)
  status_summary <- summary(project_status)
  expect_equal(
    status_summary$n_complete[
      status_summary$step == "extract_rois_shared_complete"
    ],
    1
  )
  expect_equal(
    status_summary$n_complete[
      status_summary$step == "extract_rois_alternate_complete"
    ],
    1
  )

  scfg$extract_rois$enable <- FALSE
  disabled_status <- get_subject_status(scfg, sub, "A")
  expect_false(any(grepl("^extract_rois_", names(disabled_status))))
})

test_that("extraction completion uses stream-specific session tracking records", {
  root <- tempfile("status-extract-rois-db-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  log_dir <- file.path(root, "logs")
  bids_dir <- file.path(root, "bids")
  rois_dir <- file.path(root, "data_rois")
  dir.create(log_dir)
  dir.create(bids_dir)
  dir.create(rois_dir)
  dir.create(file.path(log_dir, "sub-01"))

  sqlite_db <- file.path(root, "tracking.sqlite")
  create_tracking_db(sqlite_db)
  insert_tracked_job(
    sqlite_db = sqlite_db,
    job_id = "extract-complete",
    tracking_args = list(job_name = "extract_rois_demo_sub-01_ses-A")
  )
  insert_tracked_job(
    sqlite_db = sqlite_db,
    job_id = "extract-failed",
    tracking_args = list(job_name = "extract_rois_other_sub-01_ses-A")
  )
  update_tracked_job_status(
    sqlite_db = sqlite_db,
    job_id = "extract-complete",
    status = "COMPLETED"
  )
  update_tracked_job_status(
    sqlite_db = sqlite_db,
    job_id = "extract-failed",
    status = "FAILED"
  )

  scfg <- structure(list(
    metadata = list(
      log_directory = log_dir,
      bids_directory = bids_dir,
      rois_directory = rois_dir,
      sqlite_db = sqlite_db
    ),
    extract_rois = list(enable = TRUE, demo = list(), other = list())
  ), class = "bg_project_cfg")

  complete <- is_step_complete(
    scfg,
    sub_id = "01",
    ses_id = "A",
    step_name = "extract_rois",
    ex_stream = "demo"
  )
  failed <- is_step_complete(
    scfg,
    sub_id = "01",
    ses_id = "A",
    step_name = "extract_rois",
    ex_stream = "other"
  )
  expect_true(complete$complete)
  expect_equal(complete$db_status, "COMPLETED")
  expect_match(
    basename(complete$complete_file),
    "^\\.extract_rois_demo_sub-01_ses-A_complete$"
  )
  expect_false(failed$complete)
  expect_equal(failed$db_status, "FAILED")
})

test_that("extraction completion verifies its explicit job output manifest", {
  root <- tempfile("status-extract-rois-manifest-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  log_dir <- file.path(root, "logs")
  bids_dir <- file.path(root, "bids")
  rois_dir <- file.path(root, "data_rois")
  dir.create(log_dir)
  dir.create(bids_dir)
  dir.create(rois_dir)

  selected_file <- file.path(
    rois_dir,
    "DemoAtlas",
    "sub-01_ses-A_rois-DemoAtlas_timeseries.tsv"
  )
  dir.create(dirname(selected_file))
  writeLines("selected", selected_file)
  writeLines("another job", file.path(rois_dir, "unrelated.tsv"))

  sqlite_db <- file.path(root, "tracking.sqlite")
  create_tracking_db(sqlite_db)
  insert_tracked_job(
    sqlite_db = sqlite_db,
    job_id = "extract-manifest",
    tracking_args = list(job_name = "extract_rois_demo_sub-01_ses-A")
  )
  update_tracked_job_status(
    sqlite_db = sqlite_db,
    job_id = "extract-manifest",
    status = "COMPLETED",
    output_manifest = capture_output_manifest(
      rois_dir,
      files = selected_file
    )
  )

  scfg <- structure(list(
    metadata = list(
      log_directory = log_dir,
      bids_directory = bids_dir,
      rois_directory = rois_dir,
      sqlite_db = sqlite_db
    ),
    extract_rois = list(enable = TRUE, demo = list())
  ), class = "bg_project_cfg")

  complete <- is_step_complete(
    scfg,
    sub_id = "01",
    ses_id = "A",
    step_name = "extract_rois",
    ex_stream = "demo"
  )
  expect_true(complete$complete)
  expect_true(isTRUE(complete$manifest_verified))
  expect_equal(complete$verification_source, "db_manifest_verified")

  unlink(selected_file)
  incomplete <- is_step_complete(
    scfg,
    sub_id = "01",
    ses_id = "A",
    step_name = "extract_rois",
    ex_stream = "demo"
  )
  expect_false(incomplete$complete)
  expect_false(isTRUE(incomplete$manifest_verified))
  expect_equal(incomplete$verification_source, "db_manifest_failed")
})

test_that("ROI extraction batch scripts write successful completion markers", {
  scripts <- file.path(
    system.file("hpc_scripts", package = "BrainGnomes"),
    c("extract_rois_subject.sbatch", "extract_rois_subject.pbs")
  )
  expect_true(all(file.exists(scripts)))
  for (script in scripts) {
    contents <- paste(readLines(script, warn = FALSE), collapse = "\n")
    expect_true(
      grepl(
        'date +"%Y-%m-%d %H:%M:%S" > "$complete_file"',
        contents,
        fixed = TRUE
      ),
      info = basename(script)
    )
    expect_true(
      grepl(
        '--output_manifest_file "$output_manifest_file"',
        contents,
        fixed = TRUE
      ),
      info = basename(script)
    )
    expect_true(
      grepl('! -s "$output_manifest_file"', contents, fixed = TRUE),
      info = basename(script)
    )
  }
})

test_that("process_subject schedules each extraction stream and session distinctly", {
  root <- tempfile("status-extract-scheduling-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  directories <- file.path(
    root,
    c("logs", "bids", "scratch", "data_rois", "postproc")
  )
  for (directory in directories) dir.create(directory)
  names(directories) <- c("logs", "bids", "scratch", "rois", "postproc")

  subject <- "F08"
  on.exit(
    try(lgr::get_logger(c("sub", subject))$config(NULL), silent = TRUE),
    add = TRUE
  )
  sessions <- c("A", "B")
  bids_sub_dir <- file.path(directories[["bids"]], paste0("sub-", subject))
  dir.create(bids_sub_dir)
  bids_ses_dirs <- file.path(bids_sub_dir, paste0("ses-", sessions))
  for (directory in bids_ses_dirs) dir.create(directory)

  stream_config <- list(ncores = 1L, nhours = 1, memgb = 1)
  scfg <- structure(list(
    metadata = list(
      project_directory = root,
      log_directory = directories[["logs"]],
      bids_directory = directories[["bids"]],
      scratch_directory = directories[["scratch"]],
      rois_directory = directories[["rois"]],
      postproc_directory = directories[["postproc"]],
      sqlite_db = NULL
    ),
    compute_environment = list(scheduler = "slurm"),
    extract_rois = list(
      enable = TRUE,
      shared = stream_config,
      alternate = stream_config
    ),
    force = FALSE,
    debug = FALSE,
    log_level = "INFO"
  ), class = "bg_project_cfg")

  sub_cfg <- data.frame(
    sub_id = rep(subject, 2L),
    ses_id = sessions,
    dicom_sub_dir = NA_character_,
    dicom_ses_dir = NA_character_,
    bids_sub_dir = rep(bids_sub_dir, 2L),
    bids_ses_dir = bids_ses_dirs,
    stringsAsFactors = FALSE
  )
  sub_cfg$steps <- rep(list(list()), 2L)
  steps <- c(
    bids_conversion = FALSE,
    mriqc = FALSE,
    fmriprep = FALSE,
    aroma = FALSE,
    postprocess = FALSE,
    extract_rois = TRUE
  )

  scheduled <- list()
  local_mocked_bindings(
    get_job_script = function(...) "extract_rois_subject.sbatch",
    get_job_sched_args = function(...) character(),
    submit_extract_rois = function(
        scfg, sub_dir, sub_id, ses_id, env_variables, sched_script,
        sched_args, parent_ids, lg, tracking_sqlite_db, tracking_args,
        ex_stream) {
      scheduled[[length(scheduled) + 1L]] <<- list(
        stream = ex_stream,
        session = ses_id,
        job_name = tracking_args$job_name,
        complete_file = unname(env_variables[["complete_file"]])
      )
      as.character(length(scheduled))
    },
    .package = "BrainGnomes"
  )

  expect_true(process_subject(scfg, sub_cfg, steps = steps))
  expect_length(scheduled, 4L)
  expect_setequal(
    vapply(scheduled, `[[`, character(1), "job_name"),
    c(
      "extract_rois_shared_sub-F08_ses-A",
      "extract_rois_alternate_sub-F08_ses-A",
      "extract_rois_shared_sub-F08_ses-B",
      "extract_rois_alternate_sub-F08_ses-B"
    )
  )
  expect_setequal(
    basename(vapply(scheduled, `[[`, character(1), "complete_file")),
    paste0(
      ".",
      c(
        "extract_rois_shared_sub-F08_ses-A",
        "extract_rois_alternate_sub-F08_ses-A",
        "extract_rois_shared_sub-F08_ses-B",
        "extract_rois_alternate_sub-F08_ses-B"
      ),
      "_complete"
    )
  )
})

test_that("is_step_complete prefers newer complete over stale fail", {
  root <- tempfile("status-logic-")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  log_dir <- file.path(root, "logs"); dir.create(log_dir)
  postproc_dir <- file.path(root, "postproc"); dir.create(postproc_dir)

  sub <- "01"
  dir.create(file.path(log_dir, paste0("sub-", sub)))
  dir.create(file.path(postproc_dir, paste0("sub-", sub)), recursive = TRUE)

  complete_file <- file.path(log_dir, paste0("sub-", sub), paste0(".postprocess_stream1_sub-", sub, "_complete"))
  fail_file <- file.path(log_dir, paste0("sub-", sub), paste0(".postprocess_stream1_sub-", sub, "_fail"))
  cat("2025-01-02 00:00:00", file = complete_file)
  cat("2025-01-01 00:00:00", file = fail_file)
  Sys.setFileTime(complete_file, as.POSIXct("2025-01-02 00:00:00", tz = "UTC"))
  Sys.setFileTime(fail_file, as.POSIXct("2025-01-01 00:00:00", tz = "UTC"))

  scfg <- list(
    metadata = list(log_directory = log_dir, postproc_directory = postproc_dir),
    bids_conversion = list(enable = FALSE),
    mriqc = list(enable = FALSE),
    fmriprep = list(enable = FALSE),
    aroma = list(enable = FALSE),
    postprocess = list(enable = TRUE, stream1 = list())
  )
  class(scfg) <- "bg_project_cfg"

  res <- is_step_complete(scfg, sub_id = sub, step_name = "postprocess", pp_stream = "stream1")
  expect_true(res$complete)

  Sys.setFileTime(fail_file, as.POSIXct("2025-01-03 00:00:00", tz = "UTC"))
  res_new_fail <- is_step_complete(scfg, sub_id = sub, step_name = "postprocess", pp_stream = "stream1")
  expect_false(res_new_fail$complete)
})

test_that("is_step_complete requires out_dir when manifest is missing or invalid", {
  root <- tempfile("status-db-")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  log_dir <- file.path(root, "logs")
  fmriprep_dir <- file.path(root, "fmriprep")
  dir.create(log_dir, recursive = TRUE)
  dir.create(fmriprep_dir, recursive = TRUE)

  sqlite_db <- file.path(root, "tracking.sqlite")
  create_tracking_db(sqlite_db)

  sub <- "01"
  job_name <- paste0("fmriprep_sub-", sub)
  insert_tracked_job(
    sqlite_db = sqlite_db,
    job_id = "job1",
    tracking_args = list(job_name = job_name)
  )

  scfg <- list(
    metadata = list(
      log_directory = log_dir,
      fmriprep_directory = fmriprep_dir,
      sqlite_db = sqlite_db
    ),
    bids_conversion = list(enable = FALSE),
    mriqc = list(enable = FALSE),
    fmriprep = list(enable = TRUE),
    aroma = list(enable = FALSE),
    postprocess = list(enable = FALSE)
  )
  class(scfg) <- "bg_project_cfg"

  update_tracked_job_status(sqlite_db = sqlite_db, job_id = "job1", status = "COMPLETED")

  out_dir <- file.path(fmriprep_dir, paste0("sub-", sub))
  dir.create(out_dir, recursive = TRUE)

  res_missing_manifest <- is_step_complete(scfg, sub_id = sub, step_name = "fmriprep")
  expect_true(res_missing_manifest$complete)
  expect_equal(res_missing_manifest$verification_source, "db_status_dir_exists")

  unlink(out_dir, recursive = TRUE, force = TRUE)
  res_missing_manifest_no_dir <- is_step_complete(scfg, sub_id = sub, step_name = "fmriprep")
  expect_false(res_missing_manifest_no_dir$complete)
  expect_equal(res_missing_manifest_no_dir$verification_source, "db_status_dir_missing")

  dir.create(out_dir, recursive = TRUE)
  update_tracked_job_status(
    sqlite_db = sqlite_db,
    job_id = "job1",
    status = "COMPLETED",
    output_manifest = "{not valid json"
  )

  res_invalid_manifest <- is_step_complete(scfg, sub_id = sub, step_name = "fmriprep")
  expect_true(res_invalid_manifest$complete)
  expect_true(is.na(res_invalid_manifest$manifest_verified))
  expect_equal(res_invalid_manifest$verification_source, "db_status_dir_exists")
})

test_that("is_step_complete handles old schema without output_manifest column", {
  root <- tempfile("status-db-old-")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  log_dir <- file.path(root, "logs")
  fmriprep_dir <- file.path(root, "fmriprep")
  dir.create(log_dir, recursive = TRUE)
  dir.create(fmriprep_dir, recursive = TRUE)

  sqlite_db <- file.path(root, "tracking.sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), sqlite_db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbExecute(con, "
    CREATE TABLE job_tracking (
      id INTEGER PRIMARY KEY,
      job_id VARCHAR NOT NULL UNIQUE,
      job_name VARCHAR,
      time_submitted INTEGER,
      time_ended INTEGER,
      status VARCHAR(24)
    )
  ")
  DBI::dbExecute(con, "
    INSERT INTO job_tracking (job_id, job_name, time_submitted, time_ended, status)
    VALUES ('job_old', 'fmriprep_sub-01', 1, 2, 'COMPLETED')
  ")

  scfg <- list(
    metadata = list(
      log_directory = log_dir,
      fmriprep_directory = fmriprep_dir,
      sqlite_db = sqlite_db
    ),
    bids_conversion = list(enable = FALSE),
    mriqc = list(enable = FALSE),
    fmriprep = list(enable = TRUE),
    aroma = list(enable = FALSE),
    postprocess = list(enable = FALSE)
  )
  class(scfg) <- "bg_project_cfg"

  out_dir <- file.path(fmriprep_dir, "sub-01")
  dir.create(out_dir, recursive = TRUE)

  res_old_schema <- is_step_complete(scfg, sub_id = "01", step_name = "fmriprep")
  expect_true(res_old_schema$complete)
  expect_true(is.na(res_old_schema$manifest_verified))
  expect_equal(res_old_schema$verification_source, "db_status_dir_exists")

  unlink(out_dir, recursive = TRUE, force = TRUE)
  res_old_schema_no_dir <- is_step_complete(scfg, sub_id = "01", step_name = "fmriprep")
  expect_false(res_old_schema_no_dir$complete)
  expect_equal(res_old_schema_no_dir$verification_source, "db_status_dir_missing")
})

test_that("is_step_complete surfaces DB query failures and does not fall back to .complete", {
  root <- tempfile("status-db-query-error-")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  log_dir <- file.path(root, "logs")
  fmriprep_dir <- file.path(root, "fmriprep")
  dir.create(log_dir, recursive = TRUE)
  dir.create(fmriprep_dir, recursive = TRUE)

  sqlite_db <- file.path(root, "tracking.sqlite")
  # Create a SQLite file that lacks the expected job_tracking table.
  con <- DBI::dbConnect(RSQLite::SQLite(), sqlite_db)
  DBI::dbExecute(con, "CREATE TABLE unrelated (id INTEGER PRIMARY KEY, value TEXT)")
  DBI::dbDisconnect(con)

  sub <- "01"
  out_dir <- file.path(fmriprep_dir, paste0("sub-", sub))
  dir.create(out_dir, recursive = TRUE)
  sub_log_dir <- file.path(log_dir, paste0("sub-", sub))
  dir.create(sub_log_dir, recursive = TRUE)
  complete_file <- file.path(sub_log_dir, paste0(".fmriprep_sub-", sub, "_complete"))
  writeLines("done", complete_file)

  scfg <- list(
    metadata = list(
      log_directory = log_dir,
      fmriprep_directory = fmriprep_dir,
      sqlite_db = sqlite_db
    ),
    bids_conversion = list(enable = FALSE),
    mriqc = list(enable = FALSE),
    fmriprep = list(enable = TRUE),
    aroma = list(enable = FALSE),
    postprocess = list(enable = FALSE)
  )
  class(scfg) <- "bg_project_cfg"

  expect_warning(
    res <- is_step_complete(scfg, sub_id = sub, step_name = "fmriprep"),
    regexp = "could not query job tracking database"
  )
  expect_false(res$complete)
  expect_equal(res$verification_source, "db_query_error")
  expect_true(is.character(res$db_error))
  expect_true(nzchar(res$db_error))
})

test_that("is_step_complete uses manifest verification for DB COMPLETED", {
  root <- tempfile("status-db-manifest-ok-")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  log_dir <- file.path(root, "logs")
  fmriprep_dir <- file.path(root, "fmriprep")
  dir.create(log_dir, recursive = TRUE)
  dir.create(fmriprep_dir, recursive = TRUE)

  sqlite_db <- file.path(root, "tracking.sqlite")
  create_tracking_db(sqlite_db)

  sub <- "01"
  job_name <- paste0("fmriprep_sub-", sub)
  insert_tracked_job(
    sqlite_db = sqlite_db,
    job_id = "job_manifest_ok",
    tracking_args = list(job_name = job_name)
  )

  out_dir <- file.path(fmriprep_dir, paste0("sub-", sub))
  dir.create(out_dir, recursive = TRUE)
  writeLines("data", file.path(out_dir, "output.txt"))
  manifest_json <- capture_output_manifest(out_dir)

  update_tracked_job_status(
    sqlite_db = sqlite_db,
    job_id = "job_manifest_ok",
    status = "COMPLETED",
    output_manifest = manifest_json
  )

  scfg <- list(
    metadata = list(
      log_directory = log_dir,
      fmriprep_directory = fmriprep_dir,
      sqlite_db = sqlite_db
    ),
    bids_conversion = list(enable = FALSE),
    mriqc = list(enable = FALSE),
    fmriprep = list(enable = TRUE),
    aroma = list(enable = FALSE),
    postprocess = list(enable = FALSE)
  )
  class(scfg) <- "bg_project_cfg"

  res <- is_step_complete(scfg, sub_id = sub, step_name = "fmriprep")
  expect_true(res$complete)
  expect_true(isTRUE(res$manifest_verified))
  expect_equal(res$verification_source, "db_manifest_verified")
})

test_that("is_step_complete fails when manifest verification fails", {
  root <- tempfile("status-db-manifest-fail-")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  log_dir <- file.path(root, "logs")
  fmriprep_dir <- file.path(root, "fmriprep")
  dir.create(log_dir, recursive = TRUE)
  dir.create(fmriprep_dir, recursive = TRUE)

  sqlite_db <- file.path(root, "tracking.sqlite")
  create_tracking_db(sqlite_db)

  sub <- "01"
  job_name <- paste0("fmriprep_sub-", sub)
  insert_tracked_job(
    sqlite_db = sqlite_db,
    job_id = "job_manifest_fail",
    tracking_args = list(job_name = job_name)
  )

  out_dir <- file.path(fmriprep_dir, paste0("sub-", sub))
  dir.create(out_dir, recursive = TRUE)
  file_path <- file.path(out_dir, "output.txt")
  writeLines("data", file_path)
  manifest_json <- capture_output_manifest(out_dir)

  update_tracked_job_status(
    sqlite_db = sqlite_db,
    job_id = "job_manifest_fail",
    status = "COMPLETED",
    output_manifest = manifest_json
  )

  unlink(file_path)

  scfg <- list(
    metadata = list(
      log_directory = log_dir,
      fmriprep_directory = fmriprep_dir,
      sqlite_db = sqlite_db
    ),
    bids_conversion = list(enable = FALSE),
    mriqc = list(enable = FALSE),
    fmriprep = list(enable = TRUE),
    aroma = list(enable = FALSE),
    postprocess = list(enable = FALSE)
  )
  class(scfg) <- "bg_project_cfg"

  res <- is_step_complete(scfg, sub_id = sub, step_name = "fmriprep")
  expect_false(res$complete)
  expect_false(isTRUE(res$manifest_verified))
  expect_equal(res$verification_source, "db_manifest_failed")
})

test_that("is_step_complete respects FAILED DB status", {
  root <- tempfile("status-db-failed-")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  log_dir <- file.path(root, "logs")
  fmriprep_dir <- file.path(root, "fmriprep")
  dir.create(log_dir, recursive = TRUE)
  dir.create(fmriprep_dir, recursive = TRUE)

  sqlite_db <- file.path(root, "tracking.sqlite")
  create_tracking_db(sqlite_db)

  sub <- "01"
  job_name <- paste0("fmriprep_sub-", sub)
  insert_tracked_job(
    sqlite_db = sqlite_db,
    job_id = "job_failed",
    tracking_args = list(job_name = job_name)
  )

  update_tracked_job_status(
    sqlite_db = sqlite_db,
    job_id = "job_failed",
    status = "FAILED"
  )

  out_dir <- file.path(fmriprep_dir, paste0("sub-", sub))
  dir.create(out_dir, recursive = TRUE)
  sub_log_dir <- file.path(log_dir, paste0("sub-", sub))
  dir.create(sub_log_dir, recursive = TRUE)
  complete_file <- file.path(sub_log_dir, paste0(".fmriprep_sub-", sub, "_complete"))
  writeLines("done", complete_file)

  scfg <- list(
    metadata = list(
      log_directory = log_dir,
      fmriprep_directory = fmriprep_dir,
      sqlite_db = sqlite_db
    ),
    bids_conversion = list(enable = FALSE),
    mriqc = list(enable = FALSE),
    fmriprep = list(enable = TRUE),
    aroma = list(enable = FALSE),
    postprocess = list(enable = FALSE)
  )
  class(scfg) <- "bg_project_cfg"

  res <- is_step_complete(scfg, sub_id = sub, step_name = "fmriprep")
  expect_false(res$complete)
  expect_equal(res$verification_source, "db_status_failed")
})

test_that("is_step_complete falls back to complete file for STARTED jobs", {
  root <- tempfile("status-db-started-")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  log_dir <- file.path(root, "logs")
  fmriprep_dir <- file.path(root, "fmriprep")
  dir.create(log_dir, recursive = TRUE)
  dir.create(fmriprep_dir, recursive = TRUE)

  sqlite_db <- file.path(root, "tracking.sqlite")
  create_tracking_db(sqlite_db)

  sub <- "01"
  job_name <- paste0("fmriprep_sub-", sub)
  insert_tracked_job(
    sqlite_db = sqlite_db,
    job_id = "job_started",
    tracking_args = list(job_name = job_name)
  )

  update_tracked_job_status(
    sqlite_db = sqlite_db,
    job_id = "job_started",
    status = "STARTED"
  )

  out_dir <- file.path(fmriprep_dir, paste0("sub-", sub))
  dir.create(out_dir, recursive = TRUE)
  sub_log_dir <- file.path(log_dir, paste0("sub-", sub))
  dir.create(sub_log_dir, recursive = TRUE)
  complete_file <- file.path(sub_log_dir, paste0(".fmriprep_sub-", sub, "_complete"))
  writeLines("done", complete_file)

  scfg <- list(
    metadata = list(
      log_directory = log_dir,
      fmriprep_directory = fmriprep_dir,
      sqlite_db = sqlite_db
    ),
    bids_conversion = list(enable = FALSE),
    mriqc = list(enable = FALSE),
    fmriprep = list(enable = TRUE),
    aroma = list(enable = FALSE),
    postprocess = list(enable = FALSE)
  )
  class(scfg) <- "bg_project_cfg"

  res <- is_step_complete(scfg, sub_id = sub, step_name = "fmriprep")
  expect_true(res$complete)
  expect_equal(res$verification_source, "complete_file")
})

test_that("check_status_reconciliation detects fail-marker/DB mismatch", {
  root <- tempfile("status-reconcile-fail-")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  log_dir <- file.path(root, "logs")
  bids_dir <- file.path(root, "bids")
  fmriprep_dir <- file.path(root, "fmriprep")
  dir.create(log_dir, recursive = TRUE)
  dir.create(bids_dir, recursive = TRUE)
  dir.create(fmriprep_dir, recursive = TRUE)

  sub <- "01"
  dir.create(file.path(log_dir, paste0("sub-", sub)), recursive = TRUE)
  dir.create(file.path(bids_dir, paste0("sub-", sub)), recursive = TRUE)
  dir.create(file.path(fmriprep_dir, paste0("sub-", sub)), recursive = TRUE)

  sqlite_db <- file.path(root, "tracking.sqlite")
  create_tracking_db(sqlite_db)
  insert_tracked_job(
    sqlite_db = sqlite_db,
    job_id = "job_reconcile_fail",
    tracking_args = list(job_name = paste0("fmriprep_sub-", sub))
  )
  update_tracked_job_status(sqlite_db = sqlite_db, job_id = "job_reconcile_fail", status = "STARTED")

  fail_file <- file.path(log_dir, paste0("sub-", sub), paste0(".fmriprep_sub-", sub, "_fail"))
  writeLines("failed", fail_file)

  scfg <- list(
    metadata = list(
      log_directory = log_dir,
      bids_directory = bids_dir,
      fmriprep_directory = fmriprep_dir,
      sqlite_db = sqlite_db
    ),
    bids_conversion = list(enable = FALSE),
    mriqc = list(enable = FALSE),
    fmriprep = list(enable = TRUE),
    aroma = list(enable = FALSE),
    postprocess = list(enable = FALSE)
  )
  class(scfg) <- "bg_project_cfg"

  rec <- check_status_reconciliation(
    scfg = scfg,
    sub_ids = sub,
    steps = "fmriprep",
    verbose = FALSE
  )

  expect_equal(nrow(rec), 1)
  expect_true(rec$fail_file_exists[1])
  expect_true(rec$discrepancy[1])
  expect_match(rec$details[1], ".fail file exists but DB status is: STARTED", fixed = TRUE)
})
