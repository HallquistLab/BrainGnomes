# Get processing status for all subjects

Get processing status for all subjects

## Usage

``` r
get_project_status(scfg = getwd())
```

## Arguments

- scfg:

  A `bg_project_cfg` object, YAML configuration file, or project
  directory containing `project_config.yaml`. Defaults to the current
  working directory.

## Value

A data.frame with one row per subject/session containing completion
status columns for every configured stage and stream. When no subjects
are present, returns a zero-row `bg_status_df` with the same typed
columns, including character identifiers, logical completion flags, and
POSIXct completion times.

## Details

This function verifies completion markers on disk. Use
[`inspect_project()`](https://hallquistlab.github.io/BrainGnomes/reference/inspect_project.md)
instead to inspect queued, running, failed, blocked, and completed jobs
recorded in the tracking database.

## See also

[`inspect_project()`](https://hallquistlab.github.io/BrainGnomes/reference/inspect_project.md)
for scheduler-lifecycle status.
