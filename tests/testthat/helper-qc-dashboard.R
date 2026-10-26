#' Expand a collected two-scan prototype without creating imaging files
#' @param snapshot Synthetic snapshot with participant 001 as the prototype.
#' @param participants Number of synthetic participants for browser stress tests.
#' @return Snapshot with consistent record joins and deliberately unlocated paths.
#' @noRd
expand_qc_browser_fixture <- function(snapshot, participants = 500L) {
  inventory <- snapshot$inventory
  source_ids <- inventory$record_id[inventory$sub_id %in% "001"]
  tables <- names(snapshot)[vapply(snapshot, is.data.frame, logical(1))]
  for (name in tables) {
    rows <- snapshot[[name]]
    keep <- if ("sub_id" %in% names(rows)) rows$sub_id %in% "001" else
      if ("record_id" %in% names(rows)) rows$record_id %in% source_ids else rep(FALSE, nrow(rows))
    prototype <- rows[keep, , drop = FALSE]
    if (!nrow(prototype)) {
      snapshot[[name]] <- prototype
      next
    }
    chunks <- lapply(seq_len(participants), function(index) {
      subject <- sprintf("%03d", index)
      copy <- prototype
      for (field in names(copy)[vapply(copy, is.character, logical(1))]) {
        copy[[field]] <- gsub("sub-001", paste0("sub-", subject), copy[[field]], fixed = TRUE)
      }
      if ("sub_id" %in% names(copy)) copy$sub_id <- rep(subject, nrow(copy))
      if ("record_id" %in% names(copy)) copy$record_id <- ifelse(is.na(copy$record_id),
        NA_character_, paste0(subject, "-", copy$record_id))
      if ("job_id" %in% names(copy)) copy$job_id <- paste0(subject, "-", copy$job_id)
      if (name == "metrics") {
        fd <- copy$metric %in% "mean_fd"
        copy$value[fd] <- if (index == participants) NA_real_ else index / 1000
      }
      copy
    })
    snapshot[[name]] <- as.data.frame(data.table::rbindlist(chunks, use.names = TRUE))
  }
  snapshot$metadata$snapshot_id <- uuid::UUIDgenerate()
  snapshot$metadata$project <- "Synthetic browser regression (not study results)"
  snapshot
}

#' Include Quarto diagnostics when an opt-in synthetic report fails to render
#' @param snapshot Collected synthetic inventory.
#' @param output_dir New report directory.
#' @return Rendered HTML path; errors include the retained renderer log.
#' @noRd
render_qc_browser_fixture <- function(snapshot, output_dir) {
  tryCatch(BrainGnomes::render_qc_dashboard(snapshot, output_dir), error = function(error) {
    log <- file.path(output_dir, "render.log")
    details <- if (file.exists(log)) paste(readLines(log, warn = FALSE), collapse = "\n") else "No renderer log."
    stop(conditionMessage(error), "\n", details, call. = FALSE)
  })
}
