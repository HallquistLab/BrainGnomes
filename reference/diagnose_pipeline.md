# Deprecated interactive pipeline diagnosis

`diagnose_pipeline()` has been superseded by
`diagnose_project(..., interactive = TRUE)`. It remains as a
compatibility wrapper for the established guided dependency and log
browser.

## Usage

``` r
diagnose_pipeline(
  input = getwd(),
  run_id = NULL,
  subject_id = NULL,
  job_id = NULL
)
```

## Arguments

- input:

  A project configuration object, YAML file, or project directory.
  Defaults to the current working directory.

- run_id:

  Optional run ID to diagnose.

- subject_id:

  Optional subject identifier to focus.

- job_id:

  Optional exact scheduler job identifier to open.

## Value

The value returned by `diagnose_project(input, interactive = TRUE)`.

## See also

[`inspect_project()`](https://hallquistlab.github.io/BrainGnomes/reference/inspect_project.md)
for routine progress monitoring and
[`diagnose_project()`](https://hallquistlab.github.io/BrainGnomes/reference/diagnose_project.md)
for current or historical failure investigation.
