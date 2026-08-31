# Load a project configuration from a file

Load a project configuration from a file

## Usage

``` r
load_project(input = NULL, validate = TRUE)
```

## Arguments

- input:

  A path to a YAML file, or a project directory containing
  `project_config.yaml`.

- validate:

  Logical indicating whether to validate the configuration after
  loading. Validation is non-interactive and never changes or saves the
  configuration. The structured validation result is attached as the
  `validation` attribute. Default: TRUE.

## Value

A list representing the project configuration (class
`"bg_project_cfg"`). If `validate` is TRUE, the returned object has a
`validation` attribute produced by
[`validate_project_config()`](https://hallquistlab.github.io/BrainGnomes/reference/validate_project_config.md).
