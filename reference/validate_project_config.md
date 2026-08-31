# Validate a BrainGnomes project configuration without changing it

This optional inspection entry point is useful for scripts, continuous
integration, and configuration review. It is not required before
[`run_project()`](https://hallquistlab.github.io/BrainGnomes/reference/run_project.md),
which retains its selected-stage checks. Unlike the historical repair
path in
[`validate_project()`](https://hallquistlab.github.io/BrainGnomes/reference/validate_project.md),
this function never opens the setup wizard and never writes the
configuration.

## Usage

``` r
validate_project_config(input, quiet = FALSE)
```

## Arguments

- input:

  A project configuration object, YAML file, or project directory.

- quiet:

  Suppress the printed validation summary.

## Value

A `bg_project_validation` object containing `valid`, `issues`,
`messages`, and the parsed `config`.
