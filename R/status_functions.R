#' Parse completion time from a .complete file
#'
#' @param file Path to .complete file.
#' @return POSIXct time or NA if parsing fails.
#' @keywords internal
#' @importFrom lubridate parse_date_time
parse_complete_time <- function(file) {
  if (!file.exists(file)) return(as.POSIXct(NA))
  tm <- tryCatch(readLines(file, n = 1L, warn = FALSE), error = function(e) NULL)
  if (is.null(tm) || length(tm) == 0L) {
    return(as.POSIXct(NA))
  }
  parsed <- suppressWarnings(lubridate::parse_date_time(tm[1L], orders = c("y-m-d H:M:S", "mdy@HM", "mdy@HMS", "ymd HMS", "ymd HM", "mdy HM", "mdy HMS")))
  if (length(parsed) == 0L || is.na(parsed[1L])) return(as.POSIXct(NA))
  as.POSIXct(parsed)
}

status_spec <- function(scfg) {
  steps <- character()
  if (isTRUE(scfg$bids_conversion$enable)) steps <- c(steps, "bids_conversion")
  if (isTRUE(scfg$mriqc$enable)) steps <- c(steps, "mriqc")
  if (isTRUE(scfg$fmriprep$enable)) steps <- c(steps, "fmriprep")
  if (isTRUE(scfg$aroma$enable)) steps <- c(steps, "aroma")
  if (isTRUE(scfg$postprocess$enable)) steps <- c(steps, "postprocess")
  if (isTRUE(scfg$extract_rois$enable)) steps <- c(steps, "extract_rois")

  list(
    steps = steps,
    postprocess_streams = if ("postprocess" %in% steps) get_postprocess_stream_names(scfg) else character(),
    extract_streams = if ("extract_rois" %in% steps) get_extract_stream_names(scfg) else character()
  )
}

empty_project_status <- function(scfg) {
  spec <- status_spec(scfg)
  result <- list(sub_id = character(), ses_id = character())

  add_status_columns <- function(prefix) {
    result[[paste0(prefix, "_complete")]] <<- logical()
    result[[paste0(prefix, "_time")]] <<- as.POSIXct(character(), tz = "UTC")
  }

  for (step in setdiff(spec$steps, c("postprocess", "extract_rois"))) {
    add_status_columns(step)
  }
  for (stream in spec$postprocess_streams) add_status_columns(stream)
  for (stream in spec$extract_streams) add_status_columns(paste0("extract_rois_", stream))

  result <- as.data.frame(result, stringsAsFactors = FALSE)
  class(result) <- unique(c("bg_status_df", class(result)))
  result
}

#' Get processing status for a single subject
#'
#' @param scfg a project configuration object as produced by `load_project` or `setup_project`
#' @param sub_id Subject identifier.
#' @param ses_id Optional session identifier. When `NULL`, all sessions found in the
#'   subject's directory are returned.
#' @return A data.frame with columns indicating completion status and times for
#'   each enabled step. ROI-extraction streams use columns named
#'   `extract_rois_<stream>_complete` and `extract_rois_<stream>_time`, keeping
#'   them distinct from postprocessing streams with the same name.
#' @export
#' @importFrom checkmate assert_class assert_string
get_subject_status <- function(scfg, sub_id, ses_id = NULL) {
  checkmate::assert_class(scfg, "bg_project_cfg")
  checkmate::assert_string(sub_id)
  checkmate::assert_string(ses_id, null.ok = TRUE)

  status_spec <- status_spec(scfg)
  steps <- status_spec$steps
  pp_streams <- status_spec$postprocess_streams
  ex_streams <- status_spec$extract_streams

  log_dir <- scfg$metadata$log_directory
  sub_log_dir <- file.path(log_dir, paste0("sub-", sub_id))
  comp_files <- list.files(sub_log_dir, pattern = "_complete$", full.names = FALSE)
  ses_ids <- if (!is.null(ses_id)) {
    ses_id
  } else {
    # sessions referenced by .complete files
    comp_ses <- unique(sub("^.*_ses-([^_]+)_complete$", "\\1", comp_files[grepl("_ses-", comp_files)]))
    comp_ses <- comp_ses[!is.na(comp_ses) & comp_ses != "^.*_ses-([^_]+)_complete$"]
    # sessions present as directories in BIDS layout
    bids_sub_dir <- file.path(scfg$metadata$bids_directory, paste0("sub-", sub_id))
    bids_ses_dirs <- if (dir.exists(bids_sub_dir)) {
      list.dirs(bids_sub_dir, recursive = FALSE, full.names = FALSE)
    } else character(0)
    bids_ses <- sub("^ses-", "", bids_ses_dirs[grepl("^ses-", bids_ses_dirs)])
    sids <- union(comp_ses, bids_ses)
    if (length(sids) == 0) NA_character_ else sids
  }

  res <- lapply(ses_ids, function(ss) {
    row <- list(sub_id = sub_id, ses_id = ifelse(is.na(ss), NA_character_, ss))
    for (st in steps) {
      if (!st %in% c("postprocess", "extract_rois")) {
        chk <- is_step_complete(scfg, sub_id,
          ses_id = if (st %in% c("bids_conversion") && !is.na(ss)) ss else NULL,
          step_name = st
        )
        row[[paste0(st, "_complete")]] <- chk$complete
        row[[paste0(st, "_time")]] <- if (chk$complete) parse_complete_time(chk$complete_file) else as.POSIXct(NA)
      } else if (st == "postprocess") {
        for (stream in pp_streams) {
          chk <- is_step_complete(scfg, sub_id, ses_id = if (!is.na(ss)) ss else NULL, step_name = "postprocess", pp_stream = stream)
          row[[paste0(stream, "_complete")]] <- chk$complete
          row[[paste0(stream, "_time")]] <- if (chk$complete) parse_complete_time(chk$complete_file) else as.POSIXct(NA)
        }
      } else {
        for (stream in ex_streams) {
          chk <- is_step_complete(
            scfg,
            sub_id,
            ses_id = if (!is.na(ss)) ss else NULL,
            step_name = "extract_rois",
            ex_stream = stream
          )
          column_prefix <- paste0("extract_rois_", stream)
          row[[paste0(column_prefix, "_complete")]] <- chk$complete
          row[[paste0(column_prefix, "_time")]] <- if (chk$complete) parse_complete_time(chk$complete_file) else as.POSIXct(NA)
        }
      }
    }
    as.data.frame(row) # needed to preserve posixct objects in rbind
  })

  df <- do.call(rbind.data.frame, res)
  class(df) <- unique(c("bg_status_df", class(df)))
  df
}

#' Get processing status for all subjects
#'
#' @param scfg a project configuration object as produced by `load_project` or `setup_project`
#' @return A data.frame with one row per subject/session containing completion
#'   status columns for every configured stage and stream. When no subjects are
#'   present, returns a zero-row `bg_status_df` with the same typed columns,
#'   including character identifiers, logical completion flags, and POSIXct
#'   completion times.
#' @export
#' @importFrom checkmate assert_class
get_project_status <- function(scfg) {
  checkmate::assert_class(scfg, "bg_project_cfg")
  log_dir <- scfg$metadata$log_directory
  sub_dirs <- list.dirs(log_dir, recursive = FALSE, full.names = FALSE)
  sub_ids <- sub("^sub-", "", sub_dirs[grepl("^sub-", sub_dirs)])
  if (length(sub_ids) == 0L) return(empty_project_status(scfg))
  res <- lapply(sub_ids, function(id) {
    bids_sub_dir <- file.path(scfg$metadata$bids_directory, paste0("sub-", id))
    ses_dirs <- if (dir.exists(bids_sub_dir)) {
      list.dirs(bids_sub_dir, recursive = FALSE, full.names = FALSE)
    } else {
      character(0)
    }
    ses_ids <- sub("^ses-", "", ses_dirs[grepl("^ses-", ses_dirs)])
    if (length(ses_ids) == 0) {
      get_subject_status(scfg, id)
    } else {
      do.call(rbind, lapply(ses_ids, function(ss) get_subject_status(scfg, id, ss)))
    }
  })
  df <- do.call(rbind.data.frame, res)
  class(df) <- unique(c("bg_status_df", class(df)))
  df
}

#' Summarize project status
#'
#' @param object A data.frame produced by `get_project_status()`.
#' @param ... Additional arguments (unused)
#' @description Provides a tabular summary of completion counts for each step in the pipeline.
#' @return data.frame summarizing number of subjects completed for each step.
#' @export
summary.bg_status_df <- function(object, ...) {
  step_cols <- grep("_complete$", names(object), value = TRUE)
  counts <- vapply(step_cols, function(x) sum(object[[x]], na.rm = TRUE), numeric(1))
  data.frame(step = step_cols, n_complete = counts, row.names = NULL, stringsAsFactors = FALSE)
}
