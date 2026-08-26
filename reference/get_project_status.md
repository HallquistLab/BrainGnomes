# Get processing status for all subjects

Get processing status for all subjects

## Usage

``` r
get_project_status(scfg)
```

## Arguments

- scfg:

  a project configuration object as produced by `load_project` or
  `setup_project`

## Value

A data.frame with one row per subject/session containing completion
status columns for every configured stage and stream. When no subjects
are present, returns a zero-row `bg_status_df` with the same typed
columns, including character identifiers, logical completion flags, and
POSIXct completion times.
