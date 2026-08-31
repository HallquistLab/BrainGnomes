# Submit a saved or in-memory execution plan

Submit a saved or in-memory execution plan

## Usage

``` r
submit_project_plan(plan, debug = FALSE, log_level = "INFO")
```

## Arguments

- plan:

  A `bg_project_plan` object or YAML plan path.

- debug:

  Enable debug submission mode.

- log_level:

  Pipeline log threshold.

## Value

A `bg_project_run` object returned by
[`run_project()`](https://uncdependlab.github.io/BrainGnomes/reference/run_project.md).
