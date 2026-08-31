# Inspect or persist the resolved project execution model

`plan_project()` is optional inspection and automation tooling. It
exposes the stages, streams, subject/session scope, resources,
dependencies, and implicit setup work resolved for a request.
[`run_project()`](https://uncdependlab.github.io/BrainGnomes/reference/run_project.md)
resolves the same execution model internally, so creating or submitting
a plan is not required for a direct run.

## Usage

``` r
plan_project(
  input,
  steps = "all",
  subject_filter = NULL,
  postprocess_streams = NULL,
  extract_streams = NULL,
  force = FALSE,
  allow_invalid = FALSE,
  quiet = FALSE
)
```

## Arguments

- input:

  A project configuration object, YAML file, or project directory.

- steps:

  Pipeline stages or `"all"`.

- subject_filter:

  Optional subject IDs or a data frame with `sub_id` and optionally
  `ses_id`.

- postprocess_streams:

  Optional postprocessing streams.

- extract_streams:

  Optional ROI-extraction streams.

- force:

  Include work whose completion markers would otherwise skip it.

- allow_invalid:

  Build the plan despite configuration validation errors.

- quiet:

  Suppress the printed plan.

## Value

A serializable `bg_project_plan` object.

## See also

[`run_project()`](https://uncdependlab.github.io/BrainGnomes/reference/run_project.md)
for the standard direct execution path.
