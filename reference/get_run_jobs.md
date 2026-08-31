# Inspect the jobs submitted for one project run

Returns the job-tracking rows for one run. This is useful for checking
which processing step and subject each scheduler job belongs to and
whether it is queued, running, completed, failed, cancelled, or blocked
by an earlier failure.

## Usage

``` r
get_run_jobs(input, run_id = "latest")
```

## Arguments

- input:

  A project configuration object, YAML file, or project directory.

- run_id:

  Run ID returned by
  [`run_project()`](https://uncdependlab.github.io/BrainGnomes/reference/run_project.md)
  or
  [`get_project_runs()`](https://uncdependlab.github.io/BrainGnomes/reference/get_project_runs.md).
  Use `"latest"` for the most recently recorded run.

## Value

The tracking rows for the run.

## See also

[`diagnose_project()`](https://uncdependlab.github.io/BrainGnomes/reference/diagnose_project.md)
for a failure-focused summary and
[`find_run_logs()`](https://uncdependlab.github.io/BrainGnomes/reference/find_run_logs.md)
for log-file locations.

## Examples

``` r
if (FALSE) { # \dontrun{
jobs <- get_run_jobs(scfg, run$run_id)
jobs[, c("job_id", "job_name", "status")]
} # }
```
