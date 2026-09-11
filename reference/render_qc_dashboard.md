# Render a Quarto dashboard for a study QC snapshot

Render a Quarto dashboard for a study QC snapshot

## Usage

``` r
render_qc_dashboard(
  x,
  output_dir,
  title = NULL,
  quarto = Sys.which("quarto"),
  quiet = TRUE
)
```

## Arguments

- x:

  A `bg_qc_inventory` returned by
  [`collect_qc_inventory()`](https://hallquistlab.github.io/BrainGnomes/reference/collect_qc_inventory.md).

- output_dir:

  A new or empty directory for the HTML and its downloadable RDS/TSV
  snapshot. Keep these files together when sharing the dashboard.

- title:

  Optional dashboard title. Defaults to the project name.

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
without a server or network connection. Linked evidence remains in its
source location, using paths relative to the dashboard. Moving only the
dashboard preserves its displayed snapshot but does not copy upstream
reports, logs, or images. No human ratings or analysis decisions are
collected.

## See also

[`collect_qc_inventory()`](https://hallquistlab.github.io/BrainGnomes/reference/collect_qc_inventory.md),
[`write_qc_inventory()`](https://hallquistlab.github.io/BrainGnomes/reference/write_qc_inventory.md)
