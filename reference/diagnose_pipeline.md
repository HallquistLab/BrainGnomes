# Interactively investigate jobs and logs from pipeline runs

Opens the established guided diagnosis browser. You can start with one
subject across runs or select one run, follow its job relationships, and
inspect output or error logs. Diagnosis does not submit jobs or change
the project. Use
[`diagnose_project()`](https://hallquistlab.github.io/BrainGnomes/reference/diagnose_project.md)
instead when a script or report needs a prompt-free summary.

## Usage

``` r
diagnose_pipeline(input)
```

## Arguments

- input:

  A project configuration object or project directory.

## Value

Depending on the selected action, the chosen run's job tree, log
contents, or invisibly `NULL`.

## See also

[`get_project_runs()`](https://hallquistlab.github.io/BrainGnomes/reference/get_project_runs.md)
to find run IDs,
[`diagnose_project()`](https://hallquistlab.github.io/BrainGnomes/reference/diagnose_project.md)
for a prompt-free summary, and
[`retry_project_run()`](https://hallquistlab.github.io/BrainGnomes/reference/retry_project_run.md)
after correcting a failure.

## Author

Zach Vig & Dan Shallal
