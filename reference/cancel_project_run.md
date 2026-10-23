# Request cancellation of outstanding allocations from one run

Cancellation targets queued, running, or unconfirmed tracked
allocations, including dependency-blocked jobs without confirmed
scheduler termination. It does not delete project data, logs, or
completed outputs. Calling this function with `dry_run = FALSE`
immediately sends cancellation requests to the configured scheduler, so
preview the commands first. A successful client exit confirms only the
request, not that all allocation processes have stopped. Tracking
remains unchanged until scheduler accounting confirms
failure/cancellation through
[`reconcile_project_jobs()`](https://hallquistlab.github.io/BrainGnomes/reference/reconcile_project_jobs.md)
or the next relevant live
[`run_project()`](https://hallquistlab.github.io/BrainGnomes/reference/run_project.md)
preflight. Concurrent worker completion is preserved. Missing accounting
never establishes termination.

## Usage

``` r
cancel_project_run(input = getwd(), run_id = "latest", dry_run = TRUE)
```

## Arguments

- input:

  A project configuration object, YAML file, or project directory.
  Defaults to the current working directory.

- run_id:

  Run ID returned by
  [`run_project()`](https://hallquistlab.github.io/BrainGnomes/reference/run_project.md)
  or listed in `inspect_project(input)$runs`. An explicit ID is
  recommended for cancellation.

- dry_run:

  If `TRUE`, report the commands without contacting the scheduler. If
  `FALSE`, request cancellation immediately.

## Value

A data frame describing each cancellation attempt. `status` is
`would_cancel` for previews, `cancellation_requested` for successful
client requests, and `failed` for client errors. Requests do not mark
SQLite rows `CANCELLED` or authorize immediate duplicate launches.

## See also

[`inspect_project()`](https://hallquistlab.github.io/BrainGnomes/reference/inspect_project.md)
to inspect current job states before cancellation.

## Examples

``` r
if (FALSE) { # \dontrun{
cancel_project_run(scfg, run$run_id, dry_run = TRUE)
cancel_project_run(scfg, run$run_id, dry_run = FALSE)
} # }
```
