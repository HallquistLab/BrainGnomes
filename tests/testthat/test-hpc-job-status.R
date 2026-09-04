test_that("scheduler status normalization remains scheduler-aware", {
  expect_identical(
    normalize_scheduler_job_status(c("S", "T", "Z", "C"), "local"),
    c("RUNNING", "SUSPENDED", "FAILED", "COMPLETED")
  )
  expect_identical(
    normalize_scheduler_job_status(
      c("PENDING", "RUNNING+", "COMPLETED", "OUT_OF_MEMORY"), "slurm"
    ),
    c("QUEUED", "RUNNING", "COMPLETED", "FAILED")
  )
})

test_that("scheduler_job_status delegates to existing scheduler queries", {
  calls <- 0L
  local_mocked_bindings(
    local_job_status = function(job_ids = NULL, user = NULL, ...) {
      calls <<- calls + 1L
      data.frame(
        PID = as.integer(job_ids), STAT = c("S", "C"),
        stringsAsFactors = FALSE
      )
    },
    .package = "BrainGnomes"
  )

  status <- scheduler_job_status(c("10", "11"), scheduler = "sh")
  expect_equal(calls, 1L)
  expect_identical(status$scheduler, c("local", "local"))
  expect_identical(status$scheduler_status, c("RUNNING", "COMPLETED"))
  expect_identical(status$scheduler_raw_status, c("S", "C"))
})

test_that("wait_for_job consumes the scheduler-neutral status adapter", {
  calls <- 0L
  local_mocked_bindings(
    scheduler_job_status = function(job_ids, scheduler = "local", user = NULL) {
      calls <<- calls + 1L
      data.frame(
        job_id = as.character(job_ids), scheduler = "slurm",
        scheduler_status = "COMPLETED", scheduler_raw_status = "COMPLETED",
        query_detail = NA_character_, stringsAsFactors = FALSE
      )
    },
    .package = "BrainGnomes"
  )

  expect_true(wait_for_job("123", scheduler = "slurm"))
  expect_equal(calls, 1L)
})
