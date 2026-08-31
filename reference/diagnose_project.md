# Summarize failures and logs for one run without prompts

Use this function in scripts, reports, or an R session when you already
know which run to inspect. It reports all tracked jobs, separates
failed, cancelled, and blocked jobs, and locates their logs. For a
guided interactive browser, continue to use
[`diagnose_pipeline()`](https://uncdependlab.github.io/BrainGnomes/reference/diagnose_pipeline.md).

## Usage

``` r
diagnose_project(input, run_id = "latest")
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

A `bg_project_diagnosis` object containing `jobs`, `failures`, and
matching `logs` for the selected run.

## See also

[`diagnose_pipeline()`](https://uncdependlab.github.io/BrainGnomes/reference/diagnose_pipeline.md)
for interactive investigation and
[`retry_project_run()`](https://uncdependlab.github.io/BrainGnomes/reference/retry_project_run.md)
to preview a new run after correcting a failure.

## Examples

``` r
if (FALSE) { # \dontrun{
diagnosis <- diagnose_project(scfg, run$run_id)
diagnosis$failures
diagnosis$logs
} # }
```
