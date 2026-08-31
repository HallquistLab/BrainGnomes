# Find output and error logs for one run

Matches tracked job IDs to scheduler output (`.out`) and error (`.err`)
files beneath the configured log directory. It returns file locations
without opening or changing the logs.

## Usage

``` r
find_run_logs(input, run_id = "latest", failed_only = FALSE)
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

- failed_only:

  If `TRUE`, include only failed, cancelled, or downstream jobs that
  could not run because an earlier job failed.

## Value

A data frame mapping jobs to stdout/stderr log files.

## See also

[`diagnose_project()`](https://uncdependlab.github.io/BrainGnomes/reference/diagnose_project.md)
for a run summary that includes these logs.

## Examples

``` r
if (FALSE) { # \dontrun{
failed_logs <- find_run_logs(scfg, run$run_id, failed_only = TRUE)
failed_logs[, c("job_name", "status", "type", "path")]
} # }
```
