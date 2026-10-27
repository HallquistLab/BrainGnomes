# Render a Quarto dashboard for a study QC snapshot

Render a Quarto dashboard for a study QC snapshot

## Usage

``` r
render_qc_dashboard(
  x,
  output_dir = NULL,
  title = NULL,
  quarto = Sys.which("quarto"),
  quiet = TRUE
)
```

## Arguments

- x:

  A `bg_qc_inventory` returned by
  [`collect_qc_inventory()`](https://hallquistlab.github.io/BrainGnomes/reference/collect_qc_inventory.md),
  or a path to a previously exported snapshot directory to resume
  rendering.

- output_dir:

  A new or empty directory for the HTML and its downloadable RDS/TSV
  snapshot. When `x` is an exported directory, defaults to that
  directory. Existing successful HTML reports cannot be replaced.

- title:

  Optional dashboard title. Defaults to project and project/run scope.

- quarto:

  Path to the Quarto executable; defaults to the executable on `PATH`.
  Quarto 1.4 or newer is required.

- quiet:

  Suppress Quarto progress output.

## Value

Invisibly returns the absolute path to `index.html`.

## Details

Rendering requires optional packages `reactable`, `plotly`, `htmltools`,
`htmlwidgets`, `knitr`, and `rmarkdown`. Collection and TSV/RDS export
do not require these packages or Quarto. JavaScript and styles are
embedded in the HTML; search, filtering, row details, and plots work
without a server or network connection. Open the saved HTML in a web
browser. Shared identity controls synchronize scoped counts, source
distributions, and tables where the corresponding identity fields are
recorded. Raw acquisitions do not have processing-stage identities, and
workflow units do not have scan-task identities; each table labels its
filter grain. Whole-snapshot context is explicitly labeled. Histogram
and ECDF views support large cohorts; select a participant in the source
review list for scan-level plots, computational checks, and original
report links. Row details are generated on demand from indexed embedded
evidence, without rereading project files. Table exports include all
matching rows across pages; neighboring TSV links always export the full
original snapshot. Large manifest exception tables display an explicitly
labeled preview of at most 200 rows; that table's matching-row export
contains only the preview. Full association history is preserved in
`manifest_files.tsv` and the RDS. Acquisition coverage and submission
review items expose unresolved work even without derivative paths. File
counters describe resolved paths, not cohort completion. Saved-run scope
and acknowledgements appear prominently. Downstream stage/stream filters
retain upstream FD through selected inputs; space filters still require
a matching spatial output association. TSV/RDS/JSON download links
require the exported files beside the HTML; those files are not needed
to view tables and plots. Linked evidence remains in its source
location, using paths relative to the dashboard. Moving only the
dashboard preserves its displayed snapshot but does not copy upstream
reports, logs, or images. No human ratings or analysis decisions are
collected. Exports survive missing rendering dependencies or rendering
failure. `render-status.json` records the result, and `render.log`
retains diagnostics even in quiet mode. An isolated writable Quarto
cache is used and removed after rendering. Retry with
`render_qc_dashboard("/path/to/export")`.

## See also

[`collect_qc_inventory()`](https://hallquistlab.github.io/BrainGnomes/reference/collect_qc_inventory.md),
[`write_qc_inventory()`](https://hallquistlab.github.io/BrainGnomes/reference/write_qc_inventory.md)
