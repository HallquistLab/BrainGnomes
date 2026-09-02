# List submitted runs for a project

`get_project_runs()` is superseded by `inspect_project()$runs`. It
remains available as a compatibility wrapper for code that needs only
the run table.

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

[`inspect_project()`](https://hallquistlab.github.io/BrainGnomes/reference/inspect_project.md)
for project and run-level progress.

## Examples

``` r
if (FALSE) { # \dontrun{
runs <- get_project_runs(scfg)
run_id <- runs$run_id[[1L]]
} # }
```
