# Project preflight shorthand

Project preflight shorthand

## Usage

``` r
doctor(input, steps = NULL, deep = FALSE, quiet = FALSE)
```

## Arguments

- input:

  A project configuration object, YAML file, or project directory.

- steps:

  Optional stages to check. By default all enabled stages are used.

- deep:

  Also initialize Python and check optional postprocessing modules.

- quiet:

  Suppress the printed report.

## Value

A `bg_project_doctor` object.
