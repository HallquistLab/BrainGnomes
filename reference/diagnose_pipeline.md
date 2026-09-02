# Deprecated interactive pipeline diagnosis

`diagnose_pipeline()` has been superseded by
`diagnose_project(..., interactive = TRUE)`. It remains as a
compatibility wrapper for the established guided dependency and log
browser.

## Usage

``` r
diagnose_pipeline(input)
```

## Arguments

- input:

  A project configuration object or project directory.

## Value

The value returned by `diagnose_project(input, interactive = TRUE)`.

## See also

[`inspect_project()`](https://hallquistlab.github.io/BrainGnomes/reference/inspect_project.md)
for routine progress monitoring and
[`diagnose_project()`](https://hallquistlab.github.io/BrainGnomes/reference/diagnose_project.md)
for current or historical failure investigation.
