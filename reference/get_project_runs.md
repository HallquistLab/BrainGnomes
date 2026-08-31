# List submitted runs for a project

A run is one submission from
[`run_project()`](https://hallquistlab.github.io/BrainGnomes/reference/run_project.md)
or
[`retry_project_run()`](https://hallquistlab.github.io/BrainGnomes/reference/retry_project_run.md).
Use this function to find the run ID needed by the status, log,
diagnosis, provenance, retry, and cancellation functions. The newest run
is listed first.

## Usage

``` r
get_project_runs(input)
```

## Arguments

- input:

  A project configuration object, YAML file, or project directory.

## Value

A data frame with one row per run, including its ID, submission and end
times, number of tracked jobs, and overall status.

## See also

[`get_run_jobs()`](https://hallquistlab.github.io/BrainGnomes/reference/get_run_jobs.md)
to inspect the jobs in one run.

## Examples

``` r
if (FALSE) { # \dontrun{
runs <- get_project_runs(scfg)
run_id <- runs$run_id[[1L]]
} # }
```
