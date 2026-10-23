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
#' @param title Optional dashboard title. Defaults to the project name.
#' @param quarto Path to the Quarto executable; defaults to the executable on
#'   `PATH`. Quarto 1.4 or newer is required.
#' @param quiet Suppress Quarto progress output.
#' @return Invisibly returns the absolute path to `index.html`.
#' @details Rendering requires optional packages `reactable`, `plotly`,
#'   `htmltools`, `htmlwidgets`, `knitr`, and `rmarkdown`. Collection and TSV/RDS
#'   export do not require these packages or Quarto. JavaScript and styles are
#'   embedded in the HTML; search, filtering, row details, and plots work without
#'   a server or network connection. Open the saved HTML in a web browser.
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
  if (is.null(title)) title <- paste(x$metadata$project, "| Study QC")
  widgets <- withCallingHandlers(qc_dashboard_widgets(x, output_dir), error = error_handler)
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
  target <- strsplit(normalizePath(path, winslash = "/", mustWork = FALSE), "/", fixed = TRUE)[[1L]]
  base <- strsplit(normalizePath(output_dir, winslash = "/", mustWork = FALSE), "/", fixed = TRUE)[[1L]]
  common <- 0L
  while (common < min(length(target), length(base)) &&
      identical(target[common + 1L], base[common + 1L])) common <- common + 1L
  parts <- c(rep("..", length(base) - common), target[seq_along(target) > common])
  paste(vapply(parts, utils::URLencode, character(1), reserved = TRUE), collapse = "/")
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
#' @return Reactable widget, optionally registered with dashboard controls.
#' @keywords internal
#' @noRd
qc_reactable <- function(data, columns = NULL, details = NULL, page_size = 12L, id = NULL) {
  rownames(data) <- NULL
  labels <- c(sub_id = "Participant", ses_id = "Session", run_id = "Workflow run",
    run = "Scan run", sequence_id = "Pipeline run", record_role = "Role",
    output_status = "File", diagnostics_status = "Diagnostics", mean_fd = "Mean FD (mm)",
    validation_status = "Saved audit", attention_reasons = "Needs attention because")
  filter_fields <- intersect(names(data), c("sub_id", "ses_id", "task", "stage", "stream", "space"))
  defaults <- stats::setNames(lapply(names(data), function(name) {
    label <- if (name %in% names(labels)) labels[[name]] else tools::toTitleCase(gsub("_", " ", name))
    definition <- reactable::colDef(name = label, na = "Unavailable", minWidth = 110,
      cell = if (is.character(data[[name]])) qc_text_cell else NULL,
      style = list(whiteSpace = "nowrap", overflow = "hidden", textOverflow = "ellipsis"),
      headerStyle = list(whiteSpace = "nowrap"))
    if (name == "ses_id") {
      definition$na <- "No session label"
      # Render a session value as text, explaining absent BIDS session labels.
      definition$cell <- function(value) {
        if (is.na(qc_scalar(value))) "No session label" else qc_text_cell(value)
      }
    }
    if (name %in% c("message", "definition", "attention_reasons", "warnings", "value")) {
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
      noData = "No records match this view"))
  if (!is.null(id)) widget <- htmlwidgets::onRender(widget,
    "function(el, x, data) { if (window.bgQC) window.bgQC.register(el.id, data.fields); }",
    # Lists retain array shape when htmlwidgets unboxes length-one vectors.
    data = list(fields = unname(as.list(filter_fields))))
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
  if (!nrow(workflow)) return(htmltools::tags$p("No configured or tracked workflow units yet."))
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

#' Plot comparable metric groups with an explicit selector and denominators
#' @keywords internal
#' @noRd
qc_metrics_plot <- function(metrics, inventory) {
  if (!nrow(metrics)) return(htmltools::tags$p("No numeric QC evidence available. Missing diagnostics are listed in the inventory."))
  joined <- merge(metrics, inventory[c("record_id", "sub_id", "ses_id", "run", "stage",
    "stream", "input_stream", "atlas", "task", "space", "resolution")],
    by = "record_id", all.x = TRUE, sort = FALSE)
  joined <- joined[is.finite(joined$value), , drop = FALSE]
  if (!nrow(joined)) return(htmltools::tags$p("Metric files were found, but all values are unavailable."))
  group_columns <- c("metric", "stage", "stream", "input_stream", "task", "space", "resolution", "atlas")
  group_fields <- joined[group_columns]
  # Archived upstream inputs can leave observed ROI products without a known
  # atlas path. Keep their diagnostic directories separate instead of pooling
  # every unknown atlas into one distribution.
  unknown_atlas <- joined$stage == "extract_rois" & is.na(joined$atlas)
  group_fields$atlas[unknown_atlas] <- dirname(joined$source_file[unknown_atlas])
  # Mean FD uses the same definition across tasks and comes from original
  # confounds reused across output spaces. Keep each scan's value/identity.
  mean_fd <- joined$metric == "mean_fd"
  group_fields[mean_fd, setdiff(group_columns, "metric")] <- ""
  group_fields[] <- lapply(group_fields, function(x) ifelse(is.na(x), "", x))
  # Shared confounds support multiple derivative products. Show each source
  # once in the pooled FD view; other metrics keep their comparison groups.
  keep <- !duplicated(cbind(source_file = joined$source_file, group_fields))
  joined <- joined[keep, , drop = FALSE]
  group_fields <- group_fields[keep, , drop = FALSE]
  groups <- do.call(paste, c(group_fields, sep = " / "))
  fd_groups <- unique(groups[joined$metric == "mean_fd"])
  labels <- c(fd_groups, setdiff(unique(groups), fd_groups))
  # Keep full paths in grouping keys, but shorten the visible selector labels.
  # Distinct full atlas identities still select distinct traces.
  display_labels <- vapply(labels, function(label) {
    if (label %in% fd_groups) return("Mean FD (all tasks)")
    parts <- strsplit(label, " / ", fixed = TRUE)[[1L]]
    paste(basename(parts[nzchar(parts)]), collapse = " / ")
  }, character(1))
  display_labels <- make.unique(display_labels)
  plot <- plotly::plot_ly()
  x_categories <- vector("list", length(labels))
  for (i in seq_along(labels)) {
    df <- joined[groups == labels[i], , drop = FALSE]
    hover <- paste0("sub-", df$sub_id, " | task ", df$task,
      " | session ", ifelse(is.na(df$ses_id), "none", df$ses_id), " | scan run ", df$run,
      "<br>", df$value, " ", df$unit, "<br>Observed: ", df$n_observations,
      "; missing: ", df$n_missing)
    scan_labels <- paste0("sub-", df$sub_id,
      ifelse(is.na(df$ses_id), "", paste0(" / ses-", df$ses_id)),
      ifelse(df$metric == "mean_fd" & !is.na(df$task), paste0(" / task-", df$task), ""),
      ifelse(is.na(df$run), "", paste0(" / run-", df$run)))
    x_categories[[i]] <- unname(as.list(unique(scan_labels)))
    plot <- plotly::add_trace(plot, x = scan_labels, y = df$value,
      type = "scatter", mode = "markers", name = display_labels[i], text = hover,
      hoverinfo = "text", visible = i == 1L,
      marker = list(color = "#168b80", size = 10, opacity = 0.85))
  }
  buttons <- lapply(seq_along(labels), function(i) list(method = "update",
    args = list(list(visible = seq_along(labels) == i),
      list(yaxis = list(title = joined$unit[groups == labels[i]][1L], autorange = TRUE),
        xaxis = list(categoryorder = "array", categoryarray = x_categories[[i]], autorange = TRUE))),
    label = display_labels[i]))
  plotly::layout(plot, showlegend = FALSE,
    xaxis = list(title = "Scan (participant / session / task / run)", type = "category", automargin = TRUE,
      categoryorder = "array", categoryarray = x_categories[[1L]], autorange = TRUE),
    yaxis = list(title = joined$unit[groups == labels[1L]][1L], autorange = TRUE),
    updatemenus = list(list(buttons = buttons, x = 0, y = 1.22, xanchor = "left")),
    margin = list(t = 85, b = 55, l = 65, r = 15),
    paper_bgcolor = "transparent", plot_bgcolor = "transparent")
}

#' Build all dashboard components from one immutable inventory snapshot
#' @keywords internal
#' @noRd
qc_dashboard_widgets <- function(x, output_dir) {
  inventory <- x$inventory
  if (!"record_role" %in% names(inventory)) inventory$record_role <- rep("output", nrow(inventory))
  if (!"attention_reasons" %in% names(inventory)) inventory$attention_reasons <- rep("", nrow(inventory))
  summary <- qc_dashboard_summary(x)
  status_column <- reactable::colDef(cell = qc_status_badge, minWidth = 135)
  visible <- c("sub_id", "ses_id", "task", "run", "stage", "stream", "space", "resolution",
    "record_role", "output_status", "validation_status", "diagnostics_status", "attention_reasons")
  columns <- list(record_role = reactable::colDef(cell = function(value) {
      if (value == "upstream_input") "Upstream input" else "Selected output"
    }), output_status = status_column, validation_status = status_column,
    diagnostics_status = status_column)
  # Expand a display row using its stable ID; complete evidence remains exported.
  # Input is the table row index, output is a readable linked evidence panel.
  details_for <- function(rows) function(index) {
    row <- rows[index, , drop = FALSE]
    evidence <- x$evidence[!is.na(x$evidence$record_id) & x$evidence$record_id == row$record_id, ]
    metrics <- x$metrics[x$metrics$record_id == row$record_id, , drop = FALSE]
    checks <- x$checks[x$checks$record_id == row$record_id, , drop = FALSE]
    manifests <- x$manifest_files[x$manifest_files$path == row$derivative_file, , drop = FALSE]
    htmltools::tags$div(class = "qc-detail",
      htmltools::tags$h4(basename(row$derivative_file)),
      htmltools::tags$p("Source: ", row$source_file),
      htmltools::tags$p("Atlas: ", row$atlas, " | Modified: ", row$modified_at),
      htmltools::tags$p("Input stream: ", row$input_stream,
        " | Destination association: ", row$association_status),
      htmltools::tags$p("Workflow context: ", row$workflow_status, " | Scope: ", row$workflow_scope,
        " | Pipeline run: ", row$workflow_run_id,
        ". This context does not identify the attempt that produced the current file."),
      qc_evidence_links(evidence[!duplicated(evidence[c("kind", "path")]), ], output_dir),
      if (nrow(metrics)) qc_reactable(metrics[c("metric", "value", "unit", "definition", "n_observations", "n_missing")], page_size = 5L),
      if (nrow(checks)) qc_check_panels(checks),
      if (nrow(manifests)) qc_reactable(manifests[c("job_id", "run_id", "attempt", "is_current_attempt", "status")], page_size = 5L))
  }
  attention <- inventory[inventory$attention, , drop = FALSE]
  job_fields <- intersect(c("job_id", "sequence_id", "sub_id", "ses_id", "stage", "stream",
    "job_role", "attempt", "lifecycle_status", "is_current_attempt", "failure_category", "exit_code"), names(x$jobs))
  # Resolve links for one job row. Missing targets are displayed as plain text.
  # Input is the job row index; output is an escaped evidence-link list.
  job_details <- function(index) {
    job <- x$jobs[index, , drop = FALSE]
    fields <- c("stdout_log", "stderr_log", "job_manifest_path", "runtime_receipt_path")
    paths <- vapply(fields, function(field) qc_scalar(job[[field]]), character(1))
    paths <- paths[!is.na(paths) & nzchar(paths)]
    if (!is.na(job$sequence_id)) paths <- c(paths, run_provenance = file.path(
      x$metadata$log_directory, "runs", job$sequence_id, "provenance.json"))
    qc_evidence_links(data.frame(kind = names(paths), path = unname(paths), exists = file.exists(paths)), output_dir)
  }
  evidence <- qc_display_identity(x$evidence, inventory)
  # Repeated associations within an acquisition identity are not separate files.
  evidence <- evidence[!duplicated(evidence[setdiff(names(evidence), c("evidence_id", "record_id"))]), ]
  evidence <- evidence[setdiff(names(evidence), c("evidence_id", "record_id"))]
  evidence_cols <- list(path = reactable::colDef(minWidth = 400,
    cell = function(value, index) {
      if (isTRUE(evidence$exists[index])) htmltools::tags$a(
        href = qc_evidence_href(value, output_dir), target = "_blank", rel = "noopener", value)
      else value
    }))
  checks <- qc_display_identity(x$checks, inventory)
  check_fields <- setdiff(names(checks), c("record_id", "details_json", "source_file", "atlas", "input_stream"))
  settings <- x$metadata$settings
  if (!is.data.frame(settings)) settings <- data.frame(stage = character(), stream = character(),
    setting = character(), value = character())
  list(summary = summary, controls = qc_dashboard_controls(x),
    participants = qc_reactable(summary$participants, list(sub_id = reactable::colDef(
      cell = function(value) htmltools::tags$button(type = "button", class = "qc-participant",
        `data-qc-participant` = value, paste0("sub-", value)))), id = "qc-participants"),
    outputs = qc_reactable(summary$outputs, columns["record_role"], id = "qc-outputs"),
    attention = qc_reactable(attention[visible], columns, details_for(attention), id = "qc-attention"),
    inventory = qc_reactable(inventory[visible], columns, details_for(inventory), id = "qc-inventory"),
    acquisitions = qc_reactable(x$acquisitions, id = "qc-acquisitions"),
    workflow = qc_reactable(x$workflow, list(status = status_column), id = "qc-workflow"),
    workflow_plot = qc_workflow_plot(x$workflow),
    metrics = qc_reactable(qc_display_identity(x$metrics, inventory), id = "qc-metrics"),
    metric_plot = qc_metrics_plot(x$metrics, inventory),
    evidence = qc_reactable(evidence, evidence_cols, id = "qc-evidence"),
    checks = qc_reactable(checks[check_fields], list(status = status_column),
      details = function(index) qc_check_panels(checks[index, , drop = FALSE]), id = "qc-checks"),
    regions = qc_reactable(qc_display_identity(x$regions, inventory), id = "qc-regions"),
    jobs = qc_reactable(x$jobs[job_fields], details = job_details, id = "qc-jobs"),
    manifests = qc_reactable(qc_manifest_summary(x$manifest_files), id = "qc-manifests"),
    manifest_exceptions = qc_reactable(x$manifest_files[x$manifest_files$status != "matches_size_and_mtime", ],
      id = "qc-manifest-exceptions"),
    settings = qc_reactable(settings, id = "qc-settings"),
    issues = qc_reactable(x$issues), active = qc_reactable(x$active),
    reconciliation = qc_reactable(x$reconciliation))
}
