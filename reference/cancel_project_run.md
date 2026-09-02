# Cancel queued or running jobs from one run

Cancellation affects only tracked jobs that are still queued or running.
It does not delete project data, logs, or completed outputs. Calling
this function with `dry_run = FALSE` immediately sends cancellation
requests to the configured scheduler, so preview the commands first.

## Usage

``` r
cancel_project_run(input, run_id = "latest", dry_run = TRUE)
```

## Arguments

- input:

  A project configuration object, YAML file, or project directory.

- run_id:

  Run ID returned by
  [`run_project()`](https://hallquistlab.github.io/BrainGnomes/reference/run_project.md)
  or listed in `inspect_project(input)$runs`. An explicit ID is
  recommended for cancellation.

- dry_run:

  If `TRUE`, report the commands without contacting the scheduler. If
  `FALSE`, request cancellation immediately.

## Value

A data frame describing each cancellation attempt.

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
