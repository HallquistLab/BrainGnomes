# Diagnose failed project work

Reports unresolved failed, cancelled, and blocked work and locates
matching logs. By default, diagnosis follows the current project state
assembled by
[`inspect_project()`](https://hallquistlab.github.io/BrainGnomes/reference/inspect_project.md)
across runs. Select a run for a historical post-mortem. Set
`interactive = TRUE` to open the guided dependency and log browser.

## Usage

``` r
diagnose_project(input, run_id = NULL, interactive = FALSE)
```

## Arguments

- input:

  A project configuration object, YAML file, project directory, or a
  `bg_project_inspection` object returned by
  [`inspect_project()`](https://hallquistlab.github.io/BrainGnomes/reference/inspect_project.md).

- run_id:

  Optional run ID. Use `NULL` for current project failures across runs,
  `"latest"` for the newest run, or an explicit historical run ID.

- interactive:

  If `TRUE`, open the guided interactive browser. This mode retains the
  behavior formerly provided by
  [`diagnose_pipeline()`](https://hallquistlab.github.io/BrainGnomes/reference/diagnose_pipeline.md).

## Value

A `bg_project_diagnosis` object containing the underlying `inspection`,
its `jobs`, unresolved `failures`, and matching `logs` for the current
project state or selected run.

## See also

[`inspect_project()`](https://hallquistlab.github.io/BrainGnomes/reference/inspect_project.md)
for routine progress monitoring and
[`retry_project_run()`](https://hallquistlab.github.io/BrainGnomes/reference/retry_project_run.md)
to preview a new run after correcting a failure.

## Examples

``` r
if (FALSE) { # \dontrun{
diagnosis <- diagnose_project(scfg)
diagnosis$failures
diagnosis$logs

old_run <- diagnose_project(scfg, run$run_id)
} # }
```
