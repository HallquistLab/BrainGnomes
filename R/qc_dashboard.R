#' Export a QC inventory as RDS, TSV tables, and a snapshot description
#'
#' @param x A `bg_qc_inventory` returned by [collect_qc_inventory()].
#' @param output_dir A new or empty output directory. Existing snapshots are
#'   protected from replacement; use a new directory when refreshing a report.
#' @return Invisibly returns the absolute path to the exported directory.
#' @details Each data-frame component becomes a TSV with quoted text and `NA`
#'   for missing values. `inventory.rds` preserves the complete R object;
#'   `snapshot.json` records schema, snapshot ID, collection time, and project.
#'   IDs join tables within that snapshot. Reports contain participant IDs and
#'   source paths and should be shared with the same care as their source data.
#' @export
write_qc_inventory <- function(x, output_dir) {
  checkmate::assert_class(x, "bg_qc_inventory")
  assert_provenance_metadata(x, "qc_inventory")
  checkmate::assert_string(output_dir, min.chars = 1L)
  if (dir.exists(output_dir) && length(list.files(output_dir, all.files = TRUE,
      no.. = TRUE))) stop("output_dir must be new or empty; use a new snapshot directory.", call. = FALSE)
  if (!dir.exists(output_dir) && !dir.create(output_dir, recursive = TRUE)) {
    stop("Cannot create QC output directory: ", output_dir, call. = FALSE)
  }
  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)
  for (name in names(x)) if (is.data.frame(x[[name]])) {
    data.table::fwrite(x[[name]], file.path(output_dir, paste0(name, ".tsv")),
      sep = "\t", quote = "auto", na = "NA")
  }
  saveRDS(x, file.path(output_dir, "inventory.rds"))
  jsonlite::write_json(x$metadata, file.path(output_dir, "snapshot.json"),
    pretty = TRUE, auto_unbox = TRUE, na = "null")
  invisible(output_dir)
}

#' Render a Quarto dashboard for a study QC snapshot
#'
#' @param x A `bg_qc_inventory` returned by [collect_qc_inventory()], or a path
#'   to a previously exported snapshot directory to resume rendering.
#' @param output_dir A new or empty directory for the HTML and its downloadable
#'   RDS/TSV snapshot. When `x` is an exported directory, defaults to that
#'   directory. Existing successful HTML reports cannot be replaced.
#' @param title Optional dashboard title. Defaults to project and project/run scope.
#' @param quarto Path to the Quarto executable; defaults to the executable on
#'   `PATH`. Quarto 1.4 or newer is required.
#' @param quiet Suppress Quarto progress output.
#' @return Invisibly returns the absolute path to `index.html`.
#' @details Rendering requires optional packages `reactable`, `plotly`,
#'   `htmltools`, `htmlwidgets`, `knitr`, and `rmarkdown`. Collection and TSV/RDS
#'   export do not require these packages or Quarto. JavaScript and styles are
#'   embedded in the HTML; search, filtering, row details, and plots work without
#'   a server or network connection. Open the saved HTML in a web browser.
#'   Shared identity controls synchronize scoped counts, source distributions,
#'   and tables where the corresponding identity fields are recorded. Raw
#'   acquisitions do not have processing-stage identities, and workflow units
#'   do not have scan-task identities; each table labels its filter grain.
#'   Whole-snapshot context is explicitly labeled. Histogram and ECDF views
#'   support large cohorts; select a participant in the source review list for
#'   scan-level plots, computational checks, and original report links.
#'   Row details are generated on demand from indexed embedded evidence, without
#'   rereading project files. Table exports include all matching rows across
#'   pages; neighboring TSV links always export the full original snapshot.
#'   Large manifest exception tables display an explicitly labeled preview of at
#'   most 200 rows; that table's matching-row export contains only the preview.
#'   Full association history is preserved in `manifest_files.tsv` and the RDS.
#'   Acquisition coverage and submission review items expose unresolved work
#'   even without derivative paths. File counters describe resolved paths, not
#'   cohort completion. Saved-run scope and acknowledgements appear prominently.
#'   Downstream stage/stream filters retain upstream FD through selected inputs;
#'   space filters still require a matching spatial output association.
#'   TSV/RDS/JSON download links require the exported files beside the HTML;
#'   those files are not needed to view tables and plots.
#'   Linked evidence remains in its source
#'   location, using paths relative to the dashboard. Moving only the dashboard
#'   preserves its displayed snapshot but does not copy upstream reports, logs,
#'   or images. No human ratings or analysis decisions are collected.
#'   Exports survive missing rendering dependencies or rendering failure.
#'   `render-status.json` records the result, and `render.log` retains diagnostics
#'   even in quiet mode. An isolated writable Quarto cache is used and removed
#'   after rendering. Retry with `render_qc_dashboard("/path/to/export")`.
#' @seealso [collect_qc_inventory()], [write_qc_inventory()]
#' @export
render_qc_dashboard <- function(x, output_dir = NULL, title = NULL,
                                quarto = Sys.which("quarto"), quiet = TRUE) {
  checkmate::assert_string(title, null.ok = TRUE)
  checkmate::assert_flag(quiet)
  if (is.character(x)) {
    checkmate::assert_directory_exists(x)
    source_dir <- normalizePath(x, winslash = "/")
    snapshot <- readRDS(file.path(source_dir, "inventory.rds"))
    checkmate::assert_class(snapshot, "bg_qc_inventory")
    metadata <- jsonlite::read_json(file.path(source_dir, "snapshot.json"))
    if (!identical(metadata$snapshot_id, snapshot$metadata$snapshot_id)) {
      stop("Snapshot metadata does not match inventory.rds.", call. = FALSE)
    }
    if (is.null(output_dir)) output_dir <- source_dir
    checkmate::assert_string(output_dir, min.chars = 1L)
    same_dir <- identical(normalizePath(output_dir, winslash = "/", mustWork = FALSE), source_dir)
    x <- snapshot
    if (same_dir) {
      output_dir <- source_dir
      if (file.exists(file.path(output_dir, "index.html"))) {
        stop("A successful HTML report already exists; use a new snapshot directory.", call. = FALSE)
      }
    } else output_dir <- write_qc_inventory(x, output_dir)
  } else {
    checkmate::assert_class(x, "bg_qc_inventory")
    output_dir <- write_qc_inventory(x, output_dir)
  }
  # Export before checking optional renderer dependencies. Failed rendering
  # must not destroy or require recollection of the observed evidence.
  log <- file.path(output_dir, "render.log")
  writeLines("BrainGnomes dashboard rendering started.", log)
  result <- list(snapshot_id = x$metadata$snapshot_id, started_at = qc_time(Sys.time()),
    state = "failed", exit_code = NULL, message = "Rendering did not complete.")
  on.exit({
    result$finished_at <- qc_time(Sys.time())
    jsonlite::write_json(result, file.path(output_dir, "render-status.json"),
      pretty = TRUE, auto_unbox = TRUE, null = "null", na = "null")
    cat("\nResult: ", result$state, "; exit code: ",
      value_or_default(result$exit_code, "not launched"), "\n", result$message, "\n",
      sep = "", file = log, append = TRUE)
  }, add = TRUE)
  # Observe unexpected widget errors without suppressing the condition; input
  # is the error and output is its message in the retained render status.
  error_handler <- function(error) result$message <<- conditionMessage(error)
  # Keep a concrete failure reason even when failure precedes Quarto startup.
  # Input is the failure message; output is an error pointing to retained logs.
  render_failure <- function(message) {
    result$message <<- message
    stop(message, " The TSV/RDS snapshot is retained; see ", log, call. = FALSE)
  }
  needed <- c("reactable", "plotly", "htmltools", "htmlwidgets", "knitr", "rmarkdown")
  missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) render_failure(paste("Install dashboard packages:", paste(missing, collapse = ", ")))
  if (!checkmate::test_string(quarto) || !nzchar(quarto) || !file.exists(quarto)) {
    render_failure("Quarto >= 1.4 is required to render HTML.")
  }
  version <- system2(quarto, "--version", stdout = TRUE, stderr = TRUE)
  if (!length(version) || !grepl("^[0-9]+\\.[0-9]+", version[1L]) ||
      utils::compareVersion(version[1L], "1.4") < 0L) {
    render_failure("Quarto >= 1.4 is required for dashboards.")
  }
  result$quarto_version <- version[1L]
  if (is.null(title)) title <- paste(x$metadata$project, "|", if (identical(x$metadata$scope, "run"))
    paste("Run", x$metadata$run_id) else "Project", "| Study QC")
  widgets <- withCallingHandlers(qc_dashboard_widgets(x, output_dir), error = error_handler)
  result$embedded_detail_bytes <- nchar(as.character(widgets$payload), type = "bytes")
  cat("Embedded detail index: ", format(result$embedded_detail_bytes, big.mark = ","),
    " bytes. Complete historical tables remain in TSV/RDS exports.\n", sep = "", file = log, append = TRUE)
  # Serialize already-built widgets: Quarto can render the snapshot without
  # reconnecting to SQLite, rescanning data, or loading a different package build.
  saveRDS(widgets, file.path(output_dir, "dashboard-widgets.rds"))
  asset_dir <- system.file("qc_dashboard", package = "BrainGnomes", mustWork = TRUE)
  assets <- c("dashboard.css", "theme.scss", "dashboard.js", "controls.html")
  if (!all(file.copy(file.path(asset_dir, assets), output_dir, overwrite = TRUE))) {
    render_failure("Could not copy the dashboard assets.")
  }
  template <- readLines(file.path(asset_dir, "dashboard.qmd"), warn = FALSE)
  # Quarto uses YAML 1.2 booleans; R's YAML emitter otherwise writes yes/no.
  # Only the user-supplied title needs YAML escaping; the remaining header is fixed.
  header <- c("---", yaml::as.yaml(list(title = title)),
    "format:", "  dashboard:", "    theme: [cosmo, theme.scss]", "    orientation: rows",
    "    embed-resources: true", "    css: dashboard.css", "    scrolling: true",
    "    include-in-header: controls.html",
    "execute:", "  echo: false", "  warning: false", "  message: false",
    "params:", "  snapshot: inventory.rds", "  widgets: dashboard-widgets.rds", "---")
  qmd <- file.path(output_dir, "dashboard.qmd")
  writeLines(c(header, template), qmd)
  # Use the same R installation and library paths as the calling session.
  old_r <- Sys.getenv("QUARTO_R", unset = NA_character_)
  old_libs <- Sys.getenv("R_LIBS", unset = NA_character_)
  old_tmpdir <- Sys.getenv("TMPDIR", unset = NA_character_)
  old_cache <- Sys.getenv("XDG_CACHE_HOME", unset = NA_character_)
  cache <- tempfile("quarto-qc-cache-")
  if (!dir.create(cache)) render_failure("Could not create a writable Quarto cache.")
  on.exit({
    if (is.na(old_r)) Sys.unsetenv("QUARTO_R") else Sys.setenv(QUARTO_R = old_r)
    if (is.na(old_libs)) Sys.unsetenv("R_LIBS") else Sys.setenv(R_LIBS = old_libs)
    if (is.na(old_tmpdir)) Sys.unsetenv("TMPDIR") else Sys.setenv(TMPDIR = old_tmpdir)
    if (is.na(old_cache)) Sys.unsetenv("XDG_CACHE_HOME") else Sys.setenv(XDG_CACHE_HOME = old_cache)
    unlink(cache, recursive = TRUE)
  }, add = TRUE)
  Sys.setenv(QUARTO_R = R.home("bin"), R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep))
  # R may have fallen back from an unavailable cluster TMPDIR. Give Quarto
  # that same writable temporary directory rather than its stale environment.
  Sys.setenv(TMPDIR = tempdir(), XDG_CACHE_HOME = cache)
  args <- c("render", shQuote(qmd), "--output", "index.html", "--execute-dir",
    shQuote(output_dir))
  # Silence the calling console, not Quarto's error reporting: --quiet can
  # suppress the only explanation of a failed render.
  # Quarto's resource bundler also resolves generated JavaScript from the CLI
  # working directory, independently of the R chunk execution directory.
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(output_dir)
  status <- system2(quarto, args, stdout = log, stderr = log)
  result$exit_code <- status
  if (!quiet) cat(readLines(log, warn = FALSE), sep = "\n")
  html <- file.path(output_dir, "index.html")
  if (!identical(status, 0L) || !file.exists(html)) {
    if (file.exists(html)) unlink(html)
    render_failure(paste0("Quarto rendering failed (exit code ", status, ")."))
  }
  result$state <- "complete"
  result$message <- "HTML report generated."
  invisible(html)
}

#' Compute portable, URL-encoded paths relative to the dashboard directory
#' @keywords internal
#' @noRd
qc_evidence_href <- function(path, output_dir) {
  target <- qc_link_parts(path)
  base <- qc_link_parts(output_dir)
  common <- 0L
  while (common < min(length(target), length(base)) &&
      identical(target[common + 1L], base[common + 1L])) common <- common + 1L
  parts <- c(rep("..", length(base) - common), target[seq_along(target) > common])
  paste(vapply(parts, utils::URLencode, character(1), reserved = TRUE), collapse = "/")
}

#' Normalize URL path segments lexically without restatting archived evidence
#' @param path Saved absolute source path or a path relative to the R session.
#' @return Absolute path segments with dot components resolved, without I/O.
#' @noRd
qc_link_parts <- function(path) {
  path <- path.expand(path)
  if (.Platform$OS.type == "windows") path <- gsub("\\", "/", path, fixed = TRUE)
  if (!grepl("^(/|[A-Za-z]:/)", path)) path <- file.path(getwd(), path)
  segments <- strsplit(path, "/", fixed = TRUE)[[1L]]
  result <- character()
  for (segment in segments) {
    if (segment == ".") next
    if (segment == "..") {
      if (length(result) > 1L) result <- head(result, -1L)
    } else result <- c(result, segment)
  }
  result
}

#' Render safe evidence links and visibly distinguish missing targets
#' @keywords internal
#' @noRd
qc_evidence_links <- function(evidence, output_dir) {
  if (!nrow(evidence)) return(htmltools::tags$p(class = "qc-muted", "No linked evidence available."))
  htmltools::tags$ul(class = "qc-evidence", lapply(seq_len(nrow(evidence)), function(i) {
    item <- evidence[i, , drop = FALSE]
    label <- paste(gsub("_", " ", item$kind), "\u00b7", basename(item$path))
    htmltools::tags$li(if (isTRUE(item$exists)) {
      htmltools::tags$a(href = qc_evidence_href(item$path, output_dir),
        target = "_blank", rel = "noopener", label)
    } else htmltools::tags$span(class = "qc-muted", paste("Missing:", label)),
      htmltools::tags$small(class = "qc-path", item$path))
  }))
}

#' Build a consistently styled React table with optional expandable details
#' @param data Display rows; exports retain complete source tables.
#' @param columns Column overrides.
#' @param details Optional expanded row renderer.
#' @param page_size Initial page size.
#' @param id Optional stable widget ID for shared dashboard filters.
#' @param empty_message Explanation displayed when there are no rows.
#' @return Reactable widget, optionally registered with dashboard controls.
#' @keywords internal
#' @noRd
qc_reactable <- function(data, columns = NULL, details = NULL, page_size = 12L, id = NULL,
                         empty_message = "No records match this view") {
  rownames(data) <- NULL
  labels <- c(sub_id = "Participant", ses_id = "Session", run_id = "Workflow run",
    run = "Scan run", sequence_id = "Pipeline run", record_role = "Role",
    output_status = "File", diagnostics_status = "Diagnostics", mean_fd = "Mean FD (mm)",
    validation_status = "Saved audit", attention_reasons = "Needs attention because")
  filter_fields <- intersect(names(data), c("sub_id", "ses_id", "task", "stage", "stream", "space"))
  defaults <- stats::setNames(lapply(names(data), function(name) {
    label <- if (name %in% names(labels)) labels[[name]] else tools::toTitleCase(gsub("_", " ", name))
    definition <- reactable::colDef(name = label, na = "Unavailable", minWidth = 110,
      cell = if (is.character(data[[name]])) reactable::JS("function(cell) {
        return React.createElement('span', {title: cell.value}, cell.value == null ? 'Unavailable' : cell.value);
      }") else NULL,
      style = list(whiteSpace = "nowrap", overflow = "hidden", textOverflow = "ellipsis"),
      headerStyle = list(whiteSpace = "nowrap"))
    if (name == "ses_id") {
      definition$na <- "No session label"
      # Render a session value as text, explaining absent BIDS session labels.
      definition$cell <- reactable::JS("function(cell) { return cell.value == null ? 'No session label' : cell.value; }")
    }
    if (name %in% c("message", "definition", "attention_reasons", "warnings", "reason") ||
        (name == "value" && is.character(data[[name]]))) {
      definition$style <- list(whiteSpace = "normal", overflowWrap = "anywhere")
      definition$minWidth <- 240
    }
    if (name %in% filter_fields) {
      definition$filterInput <- reactable::JS("function(column) {
        var filter = column.filterValue;
        var value = filter && typeof filter === 'object' ? filter.qcExact : filter;
        return React.createElement('input', {
          value: value == null ? '' : value, placeholder: 'Filter...',
          'aria-label': 'Filter ' + column.name,
          style: {width: '100%', padding: '6px', border: '1px solid #9cacb8', borderRadius: '5px'},
          onChange: function(event) { column.setFilter(event.target.value || undefined); }
        });
      }")
      definition$filterMethod <- reactable::JS(
        "function(rows, id, filter) {
        if (filter && typeof filter === 'object' && filter.qcExact !== undefined)
          return rows.filter(function(row) {
            return filter.qcExact === null ? row.values[id] == null : row.values[id] === filter.qcExact;
          });
        return rows.filter(function(row) {
          return String(row.values[id] == null ? '' : row.values[id]).toLowerCase()
            .includes(String(filter == null ? '' : filter).toLowerCase());
        });
      }")
    }
    # Apply missing-value text only to real data columns. Applying it through
    # defaultColDef also labels React's synthetic row-expander column.
    override <- columns[[name]]
    if (!is.null(override)) definition <- utils::modifyList(definition,
      override[!vapply(override, is.null, logical(1))])
    definition
  }), names(data))
  widget <- reactable::reactable(data, columns = defaults, details = details, elementId = id,
    searchable = TRUE, filterable = TRUE, highlight = TRUE, striped = FALSE, wrap = FALSE,
    showPageSizeOptions = TRUE, defaultPageSize = page_size,
    pageSizeOptions = unique(c(page_size, 25L, 50L)), showSortable = TRUE,
    defaultColDef = reactable::colDef(minWidth = 110),
    theme = reactable::reactableTheme(color = "#183044", backgroundColor = "#ffffff",
      borderColor = "#e3e9ee", highlightColor = "#eaf5f4",
      headerStyle = list(background = "#f3f7f9", fontWeight = 600),
      searchInputStyle = list(borderRadius = "8px")),
    language = reactable::reactableLang(searchPlaceholder = "Search this table...",
      noData = empty_message))
  if (!is.null(id)) widget <- htmlwidgets::onRender(widget,
    "function(el, x, data) { if (window.bgQC) window.bgQC.register(el.id, data.fields, data.columns); }",
    # Lists retain array shape when htmlwidgets unboxes length-one vectors.
    data = list(fields = unname(as.list(filter_fields)), columns = unname(as.list(names(data)))))
  widget
}

#' Keep long table text compact while making the complete value available on hover
#' @keywords internal
#' @noRd
qc_text_cell <- function(value) {
  value <- qc_scalar(value)
  if (is.na(value)) return("Unavailable")
  htmltools::tags$span(title = value, value)
}

#' Give status values readable badges without equating availability with quality
#' @keywords internal
#' @noRd
qc_status_badge <- function(value) {
  tone <- if (value %in% c("COMPLETED", "available", "passed", "matches_size_and_mtime")) "teal"
    else if (value %in% c("FAILED", "BLOCKED", "CANCELLED", "failed", "error", "invalid", "missing", "changed", "empty_or_invalid", "ambiguous")) "red"
    else "neutral"
  htmltools::tags$span(class = paste("qc-badge", paste0("qc-", tone)),
    if (identical(value, "not_applicable")) "Not applicable" else gsub("_", " ", value))
}

#' Summarize tracked and expected workflow units in a stacked status chart
#' @keywords internal
#' @noRd
qc_workflow_plot <- function(workflow) {
  if (!nrow(workflow)) return(plotly::plot_ly(x = numeric(), y = character(), type = "bar"))
  labels <- paste(workflow$stage, ifelse(is.na(workflow$stream), "", workflow$stream))
  counts <- as.data.frame(table(stage = labels, status = workflow$status), stringsAsFactors = FALSE)
  counts <- counts[counts$Freq > 0L, , drop = FALSE]
  colors <- c(COMPLETED = "#168b80", RUNNING = "#468acf", QUEUED = "#93bfe8",
    FAILED = "#cf5360", BLOCKED = "#b36e35", CANCELLED = "#9b779d",
    NOT_TRACKED = "#c8d1d9", UNKNOWN = "#8a98a5")
  plot <- plotly::plot_ly(x = counts$Freq, y = counts$stage, color = counts$status,
    colors = colors, type = "bar", orientation = "h", hovertemplate = "%{y}<br>%{x} workflow units<extra></extra>")
  plotly::layout(plot, barmode = "stack", xaxis = list(title = "Workflow units"),
    yaxis = list(title = "", automargin = TRUE), margin = list(l = 20, r = 20, t = 12, b = 40),
    paper_bgcolor = "transparent", plot_bgcolor = "transparent",
    legend = list(orientation = "h", y = -0.2))
}

#' Plot a source-deduplicated cohort distribution with browser drill-down
#' @param metrics Saved metric rows, including unavailable observations.
#' @param inventory Derivative identities for metric comparison groups.
#' @return Plotly histogram with an offline browser scope hook.
#' @keywords internal
#' @noRd
qc_metrics_plot <- function(metrics, inventory) {
  rows <- qc_metric_sources(qc_metric_observations(metrics, inventory))
  first <- rows[rows$group_id %in% head(rows$group_id, 1L) & is.finite(rows$value), , drop = FALSE]
  plot <- plotly::plot_ly(x = first$value, type = "histogram",
    marker = list(color = "#168b80"), name = "Observed sources")
  plot <- plotly::layout(plot, showlegend = FALSE,
    xaxis = list(title = if (nrow(first)) first$unit[1L] else "Metric value"),
    yaxis = list(title = "Sources"), margin = list(t = 15, b = 55, l = 60, r = 15),
    paper_bgcolor = "transparent", plot_bgcolor = "transparent")
  htmlwidgets::onRender(plot,
    "function(el) { if (window.bgQC) window.bgQC.plot(el, 'metrics'); }")
}

#' Request browser-only details when a row is expanded
#' @param kind Detail lookup kind: record, attention, check, or job.
#' @return JavaScript callback, not an eagerly rendered R callback.
#' @noRd
qc_lazy_details <- function(kind) {
  reactable::JS(paste0("function(row) {
    return React.createElement('div', {className: 'qc-detail',
      dangerouslySetInnerHTML: {__html: window.bgQC.detail(row.values, '", kind, "')}});
  }"))
}

#' Build all dashboard components from one immutable inventory snapshot
#' @param x Collected QC inventory.
#' @param output_dir Report directory for portable evidence links.
#' @return Display widgets and one canonical lazy-detail payload.
#' @keywords internal
#' @noRd
qc_dashboard_widgets <- function(x, output_dir) {
  inventory <- x$inventory
  if (!"record_role" %in% names(inventory)) inventory$record_role <- rep("output", nrow(inventory))
  if (!"attention_reasons" %in% names(inventory)) inventory$attention_reasons <- rep("", nrow(inventory))
  if (!"validation_required" %in% names(inventory)) inventory$validation_required <- rep(NA, nrow(inventory))
  inventory$product <- basename(inventory$derivative_file)
  metrics <- qc_metric_observations(x$metrics, inventory)
  sources <- qc_metric_sources(metrics)
  review <- sources[sources$group_id %in% head(sources$group_id, 1L), , drop = FALSE]
  metric_fields <- c("sub_id", "value", "unit", "task", "run", "metric", "ses_id", "stage",
    "stream", "space", "atlas", "n_observations", "n_missing", "definition", "source_file", "record_id")
  metric_fields <- intersect(metric_fields, names(review))
  summary <- qc_dashboard_summary(x)
  status_column <- reactable::colDef(cell = reactable::JS("function(cell) {
    var value = cell.value == null ? 'Unavailable' : cell.value;
    var tone = /^(FAILED|BLOCKED|CANCELLED|failed|error|invalid|missing|ambiguous)$/.test(value) ? 'red' :
      /^(COMPLETED|available|passed)$/.test(value) ? 'teal' : 'neutral';
    return React.createElement('span', {className: 'qc-badge qc-' + tone}, value.replace(/_/g, ' '));
  }"), minWidth = 135)
  hidden_id <- reactable::colDef(show = FALSE)
  participant_cell <- reactable::colDef(cell = reactable::JS("function(cell) {
    if (cell.value == null) return 'Run-wide';
    return React.createElement('button', {className: 'qc-participant', type: 'button',
      'data-qc-participant': cell.value}, 'sub-' + cell.value);
  }"))
  columns <- list(record_id = hidden_id, output_status = status_column,
    validation_status = status_column, diagnostics_status = status_column,
    product = reactable::colDef(minWidth = 240))
  visible <- c("sub_id", "ses_id", "task", "run", "product", "stage", "stream", "space",
    "record_role", "output_status", "validation_status", "diagnostics_status", "attention_reasons", "record_id")
  coverage <- qc_snapshot_coverage(x)
  coverage_first <- c("sub_id", "stage", "stream", "coverage_status", "reason",
    "task", "run", "available_outputs", "resolved_outputs")
  coverage <- coverage[c(coverage_first, setdiff(names(coverage), coverage_first))]
  attention <- qc_review_queue(inventory, coverage, x$metadata)
  job_fields <- intersect(c("job_id", "sequence_id", "sub_id", "ses_id", "stage", "stream",
    "job_role", "attempt", "lifecycle_status", "is_current_attempt", "failure_category", "exit_code"), names(x$jobs))
  evidence <- qc_display_identity(x$evidence, inventory)
  evidence <- evidence[!duplicated(evidence[setdiff(names(evidence), c("evidence_id", "record_id"))]), ]
  evidence <- evidence[setdiff(names(evidence), c("evidence_id", "record_id"))]
  paths <- unique(na.omit(evidence$path))
  links <- stats::setNames(vapply(paths, qc_evidence_href, character(1), output_dir = output_dir), paths)
  evidence$href <- unname(links[evidence$path])
  evidence_cols <- list(href = hidden_id, path = reactable::colDef(minWidth = 400,
    cell = reactable::JS("function(cell) {
      var row = cell.row;
      return row.exists ? React.createElement('a', {href: row.href, target: '_blank', rel: 'noopener'}, cell.value) : cell.value;
    }")))
  checks <- qc_display_identity(x$checks, inventory)
  regions <- qc_display_identity(x$regions, inventory)
  region_first <- intersect(c("roi", "proportion_usable", "retained", "exclusion_reason",
    "sub_id", "ses_id", "task", "run", "stream", "atlas"), names(regions))
  regions <- regions[c(region_first, setdiff(names(regions), region_first))]
  check_fields <- intersect(c("step", "status", "message", "sub_id", "ses_id", "task", "run",
    "stage", "stream", "warnings", "audited_at", "elapsed_seconds", "record_id"), names(checks))
  settings <- x$metadata$settings
  if (!is.data.frame(settings)) settings <- data.frame(stage = character(), stream = character(),
    setting = character(), value = character())
  controls <- lapply(c("overview", "derivatives", "metrics", "workflow"), function(page) qc_dashboard_controls(x, page))
  validation_settings <- settings$value[settings$setting == "Validation enabled"]
  validation_note <- if (length(validation_settings) && all(validation_settings == "FALSE")) {
    "Validation not enabled in selected settings; any historical checks remain visible."
  } else if (is.null(x$metadata$selected_steps)) {
    "Stage selection was not recorded in this legacy snapshot; absent checks are not a pass."
  } else if (!"postprocess" %in% x$metadata$selected_steps) {
    "Postprocessing validation not selected; existing saved evidence remains visible."
  } else "No saved validation checks were collected. Absence is not a pass."
  region_note <- if (is.null(x$metadata$selected_steps)) {
    "Stage selection was not recorded in this legacy snapshot; consult saved diagnostics."
  } else if (!"extract_rois" %in% x$metadata$selected_steps) {
    "ROI extraction not selected; any existing saved evidence remains visible."
  } else "No saved regional diagnostics were collected. Check save_diagnostics and evidence availability."
  exceptions <- x$manifest_files[x$manifest_files$status != "matches_size_and_mtime", , drop = FALSE]
  exception_note <- paste("Preview:", min(200L, nrow(exceptions)), "of", nrow(exceptions),
    "exception associations. Shared filters do not apply. Table export contains only this preview; download manifest_files.tsv for every association.")
  list(summary = summary, summary_cards = qc_summary_cards(summary$totals),
    scope_banner = qc_scope_banner(x$metadata), validation_note = validation_note, region_note = region_note,
    exception_note = exception_note,
    payload = qc_dashboard_payload(x, inventory, metrics, output_dir),
    controls = controls[[1L]], controls_derivatives = controls[[2L]],
    controls_metrics = controls[[3L]], controls_workflow = controls[[4L]],
    participants = qc_reactable(summary$participants, list(sub_id = participant_cell), id = "bg-qc-participants"),
    outputs = qc_reactable(summary$outputs, id = "bg-qc-outputs"),
    coverage = qc_reactable(coverage, list(record_ids = hidden_id, sub_id = participant_cell),
      id = "bg-qc-coverage", empty_message = "No selected acquisition coverage expectations; consult workflow scope."),
    attention = qc_reactable(attention, list(record_ids = hidden_id, sub_id = participant_cell,
      category = reactable::colDef(minWidth = 180)),
      qc_lazy_details("attention"), id = "bg-qc-attention"),
    inventory = qc_reactable(inventory[visible], columns, qc_lazy_details("record"), id = "bg-qc-inventory"),
    acquisitions = qc_reactable(x$acquisitions, id = "bg-qc-acquisitions"),
    workflow = qc_reactable(x$workflow, list(status = status_column), id = "bg-qc-workflow"),
    workflow_plot = htmlwidgets::onRender(qc_workflow_plot(x$workflow),
      "function(el) { if (window.bgQC) window.bgQC.plot(el, 'workflow'); }"),
    metrics = qc_reactable(review[metric_fields], list(record_id = hidden_id,
      sub_id = reactable::colDef(cell = reactable::JS("function(cell) {
        return React.createElement('button', {className: 'qc-participant', type: 'button',
          'data-qc-record': cell.row.record_id}, 'sub-' + cell.value);
      }"))), qc_lazy_details("record"), id = "bg-qc-metric-review"),
    metric_plot = qc_metrics_plot(x$metrics, inventory),
    evidence = qc_reactable(evidence, evidence_cols, id = "bg-qc-evidence"),
    checks = qc_reactable(checks[check_fields], list(status = status_column, record_id = hidden_id),
      details = qc_lazy_details("check"), id = "bg-qc-checks",
      empty_message = if (!nrow(checks)) validation_note else "No saved checks match these filters"),
    regions = qc_reactable(regions, list(record_id = hidden_id), id = "bg-qc-regions",
      empty_message = if (!nrow(regions)) region_note else "No regional diagnostics match these filters"),
    jobs = qc_reactable(x$jobs[job_fields], details = qc_lazy_details("job"), id = "bg-qc-jobs"),
    manifests = qc_reactable(qc_manifest_summary(x$manifest_files), id = "bg-qc-manifests"),
    manifest_exceptions = qc_reactable(utils::head(exceptions, 200L),
      id = "bg-qc-manifest-exceptions"),
    settings = qc_reactable(settings, id = "bg-qc-settings"),
    issues = qc_reactable(x$issues, id = "bg-qc-issues"),
    active = qc_reactable(x$active, id = "bg-qc-active"),
    reconciliation = qc_reactable(x$reconciliation, id = "bg-qc-reconciliation"))
}
