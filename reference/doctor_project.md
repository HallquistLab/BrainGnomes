# Run non-mutating project and runtime preflight checks

This optional comprehensive preflight is useful on a new cluster, after
the submission environment changes, or before an expensive run.
`doctor_project()` checks configuration, scheduler commands, container
runtime, enabled-stage files, project storage, and the job-tracking
database. It is not required before
[`run_project()`](https://uncdependlab.github.io/BrainGnomes/reference/run_project.md)
and does not submit work, create directories, or modify the
configuration.

## Usage

``` r
doctor_project(input, steps = NULL, deep = FALSE, quiet = FALSE)
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

A `bg_project_doctor` object with an `ok` flag and a checks data frame.
