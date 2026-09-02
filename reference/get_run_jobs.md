# Inspect the jobs submitted for one project run

`get_run_jobs()` is superseded by `inspect_project(input, run_id)$jobs`.
It remains available as a compatibility wrapper and returns a data-frame
subclass with compact printing.

## Usage

``` r
get_run_jobs(input, run_id = "latest")
```

## Arguments

- input:

  A project configuration object, YAML file, or project directory.

- run_id:

  Run ID returned by
  [`run_project()`](https://hallquistlab.github.io/BrainGnomes/reference/run_project.md)
  or listed in `inspect_project(input)$runs`. Use `"latest"` for the
  most recently recorded run.

## Value

The tracking rows for the run.

## See also

[`inspect_project()`](https://hallquistlab.github.io/BrainGnomes/reference/inspect_project.md)
for summarized progress,
[`diagnose_project()`](https://hallquistlab.github.io/BrainGnomes/reference/diagnose_project.md)
for a failure-focused summary, and
[`find_run_logs()`](https://hallquistlab.github.io/BrainGnomes/reference/find_run_logs.md)
for log-file locations.

## Examples

``` r
if (FALSE) { # \dontrun{
jobs <- get_run_jobs(scfg, run$run_id)
jobs[, c("job_id", "job_name", "status")]
} # }
```
