for (reader in c("status", "inspection", "submissions", "table lookup",
                 "completion", "prefetch", "latest run", "submission connection")) {
  test_that(paste(reader, "reads wait for a concurrent worker to commit"), {
    skip_on_os("windows")
    root <- tempfile("tracking-contention-")
    dir.create(root)
    withr::defer(unlink(root, recursive = TRUE))
    db <- file.path(root, "tracking.sqlite")
    ready <- file.path(root, "writer-ready")
    reading <- file.path(root, "reader-started")
    create_tracking_db(db)
    output <- file.path(root, "outputs", "sub-01")
    dir.create(output, recursive = TRUE)
    writeLines("output", file.path(output, "image.nii.gz"))
    job_name <- if (reader == "prefetch") "prefetch_templates" else "fmriprep_sub-01"
    insert_tracked_job(db, "1", list(sequence_id = "run-1", job_name = job_name))
    manifest <- capture_output_manifest(output)
    BrainGnomes:::ensure_job_submission_schema(db)

    # Hold an exclusive worker write until the parent starts its status read.
    # Release after a short delay so a reader with no busy handler fails, while
    # a correctly bounded reader observes the committed status.
    hold_write <- function() {
      con <- DBI::dbConnect(RSQLite::SQLite(), db)
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      DBI::dbExecute(con, "BEGIN EXCLUSIVE")
      status <- if (reader %in% c("completion", "prefetch")) "COMPLETED" else "STARTED"
      DBI::dbExecute(con, "UPDATE job_tracking SET status = ?, output_manifest = ? WHERE job_id = '1'",
        params = list(status, manifest))
      file.create(ready)
      deadline <- Sys.time() + 5
      while (!file.exists(reading) && Sys.time() < deadline) Sys.sleep(0.01)
      if (!file.exists(reading)) stop("Parent did not start its status read")
      Sys.sleep(0.2)
      DBI::dbExecute(con, "COMMIT")
      TRUE
    }
    writer <- parallel::mcparallel(hold_write(), silent = TRUE)
    collected <- FALSE
    withr::defer({
      if (!collected && is.null(parallel::mccollect(writer, wait = FALSE))) {
        tools::pskill(writer$pid)
        parallel::mccollect(writer)
      }
    })
    deadline <- Sys.time() + 5
    while (!file.exists(ready) && Sys.time() < deadline) Sys.sleep(0.01)
    expect_true(file.exists(ready))
    file.create(reading)
    expect_warning({
      if (reader == "status") {
        expect_identical(get_tracked_job_status("1", sqlite_db = db)$status, "STARTED")
      } else if (reader == "inspection") {
        cfg <- list(metadata = list(sqlite_db = db))
        expect_identical(BrainGnomes:::.read_project_tracking_jobs(cfg)$status, "STARTED")
      } else if (reader == "submissions") {
        expect_equal(nrow(BrainGnomes:::read_job_submission_attempts(db)), 0L)
      } else if (reader == "table lookup") {
        expect_true(BrainGnomes:::sqlite_table_exists(db, "job_tracking"))
      } else if (reader == "completion") {
        cfg <- list(metadata = list(sqlite_db = db, log_directory = root,
          fmriprep_directory = file.path(root, "outputs")))
        completion <- is_step_complete(cfg, "01", step_name = "fmriprep")
        expect_true(completion$complete)
        expect_identical(completion$verification_source, "db_manifest_verified")
      } else if (reader == "prefetch") {
        expect_true(prefetch_manifest_verified(db, output, job_id = "1"))
      } else if (reader == "latest run") {
        cfg <- list(metadata = list(sqlite_db = db, log_directory = root))
        expect_identical(resolve_run_id(cfg), "run-1")
      } else {
        con <- job_submission_connect(db)
        expect_identical(DBI::dbGetQuery(con, "PRAGMA synchronous")$synchronous, 2L)
        expect_identical(DBI::dbGetQuery(con, "SELECT status FROM job_tracking")$status, "STARTED")
        DBI::dbDisconnect(con)
      }
      gc() # Include deferred result/connection release in the warning assertion.
    }, NA)
    result <- parallel::mccollect(writer)
    collected <- TRUE
    expect_true(result[[1L]])
  })
}

test_that("tracking connections enforce read-only access and never create missing databases", {
  db <- tempfile(fileext = ".sqlite")
  withr::defer(unlink(db))
  expect_error(tracking_db_connect(db), "unable to open")
  expect_error(tracking_db_connect(db, read_only = FALSE), "unable to open")
  expect_false(file.exists(db))
  create_tracking_db(db)
  con <- tracking_db_connect(db)
  withr::defer(DBI::dbDisconnect(con))
  expect_error(DBI::dbExecute(con, "CREATE TABLE forbidden (id INTEGER)"), "readonly")
})

test_that("durable connection setup fails cleanly when a lock outlives its timeout", {
  db <- tempfile(fileext = ".sqlite")
  withr::defer(unlink(db))
  create_tracking_db(db)
  holder <- DBI::dbConnect(RSQLite::SQLite(), db, synchronous = NULL)
  withr::defer(DBI::dbDisconnect(holder))
  DBI::dbExecute(holder, "BEGIN EXCLUSIVE")
  withr::defer(DBI::dbRollback(holder))
  connect <- DBI::dbConnect
  opened <- NULL
  local_mocked_bindings(dbConnect = function(...) {
    opened <<- connect(...)
    opened
  }, .package = "DBI")
  expect_warning({
    expect_error(tracking_db_connect(db, read_only = FALSE, busy_timeout = 0.05),
      "database is locked")
    expect_false(DBI::dbIsValid(opened))
    gc()
  }, NA)
})
