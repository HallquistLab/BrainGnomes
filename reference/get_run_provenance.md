# Read the complete provenance record for a project run

Each submitted run records the selected stages, streams, and subjects;
an exact copy of the project configuration; requested computing
resources and job order; BrainGnomes, R, software, and
submission-computer details; the scheduler; and checksums that identify
containers and other files that controlled the run. The returned object
also includes the currently tracked jobs when available. Use it to
confirm exactly what BrainGnomes submitted or to compare an original run
with a later retry.

## Usage

``` r
get_run_provenance(input, run_id = "latest")
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

A `bg_run_provenance` object.

## See also

[`diagnose_project()`](https://hallquistlab.github.io/BrainGnomes/reference/diagnose_project.md)
to inspect failures and
[`retry_project_run()`](https://hallquistlab.github.io/BrainGnomes/reference/retry_project_run.md)
to create a new run from failed work.

## Examples

``` r
if (FALSE) { # \dontrun{
provenance <- get_run_provenance(scfg, run$run_id)
provenance$request
provenance$execution$subjects
provenance$configuration$snapshot_file
} # }
```
