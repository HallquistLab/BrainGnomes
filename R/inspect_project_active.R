# Helpers for active-job timing and optional read-only scheduler reconciliation.
# These functions use ordinary snake_case names even though they are internal.

normalize_inspection_subject_id <- function(subject_id) {
  if (is.null(subject_id)) return(NULL)
  if (length(subject_id) != 1L || is.na(subject_id)) {
    stop("subject_id must be one non-missing subject identifier.", call. = FALSE)
  }
  subject_id <- sub("^sub-", "", as.character(subject_id))
  if (!nzchar(subject_id)) {
    stop("subject_id must be a non-empty subject identifier.", call. = FALSE)
  }
  subject_id
}

parse_inspection_time <- function(x, timezone = Sys.timezone()) {
  if (!length(timezone) || is.na(timezone) || !nzchar(timezone)) timezone <- "UTC"
  if (inherits(x, "POSIXct")) return(as.POSIXct(x, tz = timezone))
  suppressWarnings(as.POSIXct(
    as.character(x), tz = timezone,
    tryFormats = c(
      "%Y-%m-%d %H:%M:%OS", "%Y-%m-%dT%H:%M:%OSZ",
      "%Y-%m-%dT%H:%M:%OS%z"
    )
  ))
}

parse_wall_time_seconds <- function(x) {
  vapply(as.character(x), function(value) {
    if (is.na(value) || !nzchar(value)) return(NA_real_)
    day_parts <- strsplit(value, "-", fixed = TRUE)[[1L]]
    if (length(day_parts) > 2L) return(NA_real_)
    days <- if (length(day_parts) == 2L) suppressWarnings(as.numeric(day_parts[[1L]])) else 0
    clock <- day_parts[[length(day_parts)]]
    clock_parts <- suppressWarnings(as.numeric(strsplit(clock, ":", fixed = TRUE)[[1L]]))
    if (is.na(days) || length(clock_parts) != 3L || any(is.na(clock_parts))) {
      return(NA_real_)
    }
    if (any(clock_parts < 0) || clock_parts[[2L]] >= 60 || clock_parts[[3L]] >= 60) {
      return(NA_real_)
    }
    days * 86400 + clock_parts[[1L]] * 3600 +
      clock_parts[[2L]] * 60 + clock_parts[[3L]]
  }, numeric(1), USE.NAMES = FALSE)
}

format_elapsed_seconds <- function(seconds) {
  vapply(seconds, function(value) {
    if (is.na(value) || !is.finite(value)) return(NA_character_)
    value <- max(0, round(value))
    days <- value %/% 86400
    hours <- (value %% 86400) %/% 3600
    minutes <- (value %% 3600) %/% 60
    if (days > 0L) paste0(days, "d ", hours, "h")
    else if (hours > 0L) paste0(hours, "h ", minutes, "m")
    else if (minutes > 0L) paste0(minutes, "m")
    else paste0(value, "s")
  }, character(1), USE.NAMES = FALSE)
}

empty_active_jobs <- function() {
  data.frame(
    job_id = character(), run_id = character(), sub_id = character(),
    ses_id = character(), stage = character(), stream = character(),
    database_status = character(), scheduler = character(),
    scheduler_status = character(), health = character(),
    submitted_at = as.POSIXct(character(), tz = "UTC"),
    started_at = as.POSIXct(character(), tz = "UTC"),
    database_updated_at = as.POSIXct(character(), tz = "UTC"),
    state_since = as.POSIXct(character(), tz = "UTC"),
    queue_seconds = numeric(), runtime_seconds = numeric(),
    state_age_seconds = numeric(), requested_wall_time = character(),
    requested_wall_seconds = numeric(), overdue = logical(), stale = logical(),
    stringsAsFactors = FALSE
  )
}

empty_scheduler_reconciliation <- function() {
  data.frame(
    job_id = character(), database_status = character(),
    scheduler = character(), scheduler_status = character(),
    scheduler_raw_status = character(), agrees = logical(),
    detail = character(),
    observed_at = as.POSIXct(character(), tz = "UTC"),
    stringsAsFactors = FALSE
  )
}

build_active_job_health <- function(scfg, jobs, retrieved_at = Sys.time(),
                                    refresh = FALSE) {
  active_jobs <- jobs[
    !is.na(jobs$lifecycle_status) &
      jobs$lifecycle_status %in% c("QUEUED", "RUNNING"),
    , drop = FALSE
  ]
  if (nrow(active_jobs) == 0L) {
    return(list(
      active = empty_active_jobs(),
      reconciliation = empty_scheduler_reconciliation()
    ))
  }

  timezone <- Sys.timezone()
  submitted <- parse_inspection_time(active_jobs$time_submitted, timezone)
  started <- parse_inspection_time(active_jobs$time_started, timezone)
  ended <- parse_inspection_time(active_jobs$time_ended, timezone)
  updated_numeric <- vapply(seq_len(nrow(active_jobs)), function(index) {
    values <- as.numeric(c(submitted[[index]], started[[index]], ended[[index]]))
    if (all(is.na(values))) NA_real_ else max(values, na.rm = TRUE)
  }, numeric(1))
  database_updated <- as.POSIXct(updated_numeric, origin = "1970-01-01", tz = timezone)
  state_since <- submitted
  running_with_start <- active_jobs$lifecycle_status == "RUNNING" & !is.na(started)
  state_since[running_with_start] <- started[running_with_start]
  age <- as.numeric(difftime(retrieved_at, state_since, units = "secs"))
  age[!is.na(age)] <- pmax(0, age[!is.na(age)])
  queue_seconds <- ifelse(active_jobs$lifecycle_status == "QUEUED", age, NA_real_)
  runtime_seconds <- ifelse(active_jobs$lifecycle_status == "RUNNING", age, NA_real_)
  wall_seconds <- parse_wall_time_seconds(active_jobs$wall_time)
  overdue <- active_jobs$lifecycle_status == "RUNNING" &
    !is.na(runtime_seconds) & !is.na(wall_seconds) & runtime_seconds > wall_seconds

  default_scheduler <- NA_character_
  if (!is.null(scfg$compute_environment) &&
      length(scfg$compute_environment$scheduler) == 1L) {
    default_scheduler <- as.character(scfg$compute_environment$scheduler)
  }
  scheduler <- as.character(active_jobs$scheduler)
  scheduler[is.na(scheduler) | !nzchar(scheduler)] <- default_scheduler
  scheduler <- tolower(scheduler)
  scheduler[scheduler == "sbatch"] <- "slurm"
  scheduler[scheduler == "qsub"] <- "torque"
  scheduler[scheduler == "sh"] <- "local"

  active <- data.frame(
    job_id = as.character(active_jobs$job_id),
    run_id = as.character(active_jobs$sequence_id),
    sub_id = as.character(active_jobs$sub_id),
    ses_id = as.character(active_jobs$ses_id),
    stage = as.character(active_jobs$stage),
    stream = as.character(active_jobs$stream),
    database_status = as.character(active_jobs$lifecycle_status),
    scheduler = scheduler, scheduler_status = NA_character_,
    health = as.character(active_jobs$lifecycle_status),
    submitted_at = submitted, started_at = started,
    database_updated_at = database_updated, state_since = state_since,
    queue_seconds = queue_seconds, runtime_seconds = runtime_seconds,
    state_age_seconds = age,
    requested_wall_time = as.character(active_jobs$wall_time),
    requested_wall_seconds = wall_seconds,
    overdue = overdue, stale = FALSE,
    stringsAsFactors = FALSE
  )
  active$health[active$overdue] <- "OVERDUE"
  reconciliation <- empty_scheduler_reconciliation()

  if (isTRUE(refresh)) {
    scheduler_key <- ifelse(is.na(scheduler) | !nzchar(scheduler), "<unset>", scheduler)
    groups <- split(seq_len(nrow(active)), factor(
      scheduler_key, levels = unique(scheduler_key)
    ))
    queried <- lapply(groups, function(index) {
      suppressWarnings(scheduler_job_status(
        active$job_id[index], scheduler = scheduler_key[[index[[1L]]]]
      ))
    })
    scheduler_rows <- do.call(rbind, queried)
    rownames(scheduler_rows) <- NULL
    scheduler_match <- match(
      paste(scheduler_key, active$job_id, sep = "\r"),
      paste(scheduler_rows$scheduler, scheduler_rows$job_id, sep = "\r")
    )
    active$scheduler_status <- scheduler_rows$scheduler_status[scheduler_match]

    uncertain <- active$scheduler_status %in% c(
      "MISSING", "UNKNOWN", "UNAVAILABLE"
    ) | is.na(active$scheduler_status)
    terminal <- active$scheduler_status %in% c(
      "COMPLETED", "FAILED", "CANCELLED"
    )
    suspended <- active$scheduler_status == "SUSPENDED"
    mismatch <- !uncertain & !terminal & !suspended &
      active$scheduler_status != active$database_status
    active$health[uncertain & !active$overdue] <- "UNCONFIRMED"
    active$health[terminal] <- "STALE_DATABASE"
    active$health[suspended] <- "SUSPENDED"
    active$health[mismatch] <- "DATABASE_LAG"
    active$stale <- terminal | mismatch

    agrees <- active$scheduler_status == active$database_status
    agrees[uncertain | suspended] <- NA
    detail <- vapply(seq_len(nrow(active)), function(index) {
      query_detail <- scheduler_rows$query_detail[[scheduler_match[[index]]]]
      if (!is.na(query_detail) && nzchar(query_detail)) return(query_detail)
      scheduler_status <- active$scheduler_status[[index]]
      database_status <- active$database_status[[index]]
      if (scheduler_status %in% c("MISSING", "UNKNOWN")) {
        "Scheduler status could not be confirmed."
      } else if (identical(scheduler_status, database_status)) {
        "Database and scheduler agree."
      } else {
        paste0(
          "Database reports ", database_status,
          "; scheduler reports ", scheduler_status, "."
        )
      }
    }, character(1))
    reconciliation <- data.frame(
      job_id = active$job_id,
      database_status = active$database_status,
      scheduler = active$scheduler,
      scheduler_status = active$scheduler_status,
      scheduler_raw_status = scheduler_rows$scheduler_raw_status[scheduler_match],
      agrees = agrees, detail = detail,
      observed_at = rep(as.POSIXct(retrieved_at), nrow(active)),
      stringsAsFactors = FALSE
    )
  }

  priority <- match(active$health, c(
    "STALE_DATABASE", "DATABASE_LAG", "SUSPENDED", "OVERDUE",
    "UNCONFIRMED", "RUNNING", "QUEUED"
  ))
  order_index <- order(
    priority, -replace(active$state_age_seconds, is.na(active$state_age_seconds), -Inf),
    active$job_id, na.last = TRUE
  )
  reconciliation <- if (nrow(reconciliation) == 0L) {
    reconciliation
  } else {
    reconciliation[order_index, , drop = FALSE]
  }
  list(
    active = active[order_index, , drop = FALSE],
    reconciliation = reconciliation
  )
}

active_status_display_table <- function(active) {
  if (nrow(active) == 0L) return(active)
  columns <- c(
    "job_id", "sub_id", "stage", "stream", "database_status", "health"
  )
  if (any(!is.na(active$scheduler_status))) {
    columns <- append(columns, "scheduler_status", after = 5L)
  }
  result <- active[, columns, drop = FALSE]
  result$age <- format_elapsed_seconds(active$state_age_seconds)
  result
}
