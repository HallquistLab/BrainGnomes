#' @details
#' \if{html}{\figure{braingnomes-header.png}{options: style='max-width: 100%; height: auto;' width='720' alt='BrainGnomes: gnomes working together on brain-processing assembly lines.'}}
#'
#' Start with [setup_project()] and [run_project()], then monitor progress with
#' [inspect_project()]. Use [diagnose_project()] to investigate failures and
#' unresolved submissions, and [collect_qc_inventory()] to review a saved QC
#' snapshot. The Quickstart introduces the complete workflow:
#' `vignette("braingnomes_quickstart", package = "BrainGnomes")`.
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom Rcpp sourceCpp
#' @useDynLib BrainGnomes, .registration = TRUE
## usethis namespace: end
NULL

# Print CLI PATH guidance once per installed package version.
.onAttach <- function(libname, pkgname) {
  cli_script <- system.file("BrainGnomes", package = pkgname)
  if (!nzchar(cli_script)) return(invisible(NULL))

  cli_dir <- normalizePath(dirname(cli_script), winslash = "/", mustWork = FALSE)
  path_entries <- strsplit(Sys.getenv("PATH", unset = ""), .Platform$path.sep, fixed = TRUE)[[1]]
  path_entries <- normalizePath(path_entries[nzchar(path_entries)], winslash = "/", mustWork = FALSE)
  cli_in_path <- cli_dir %in% unique(path_entries)

  if (isTRUE(cli_in_path)) return(invisible(NULL))

  current_version <- as.character(utils::packageVersion(pkgname))
  state_dir <- tools::R_user_dir(pkgname, which = "cache")
  state_file <- file.path(state_dir, "cli_path_message_version.rds")
  shown_version <- suppressWarnings(
    tryCatch(readRDS(state_file), error = function(e) "")
  )

  if (!identical(shown_version, current_version)) {
    packageStartupMessage(
      "If you want to call BrainGnomes on the command line, ensure this directory is in your system PATH: ",
      cli_dir
    )
    try(suppressWarnings({
      dir.create(state_dir, recursive = TRUE, showWarnings = FALSE)
      saveRDS(current_version, file = state_file)
    }), silent = TRUE)
  }

  invisible(NULL)
}

# Add globals to avoid R CMD check complaint
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    ".", ":=",
    "freq", "high", "label", "low",
    "power_db", "power_linear", "relative_power",
    "resample_template_to_bold"
  ))
}
