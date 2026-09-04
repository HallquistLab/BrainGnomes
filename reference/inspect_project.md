# Inspect current project progress

Provides a compact, non-interactive view of tracked work from submission
through queueing, execution, completion, failure, or cancellation. By
default, the current state is integrated across runs by retaining the
most recent attempt for each project or subject-level work unit. Set
`run_id` to inspect only one submission.

## Usage

``` r
inspect_project(input = getwd(), run_id = NULL)
```

## Arguments

- input:

  A project configuration object, YAML file, or project directory.
  Defaults to the current working directory.

- run_id:

  Optional run ID. Use `"latest"` for the most recently recorded run, an
  explicit run ID for an older submission, or `NULL` (the default) for
  current project status across all runs.

## Value

A `bg_project_inspection` object. Its `overview`, `stages`, `subjects`,
`subject_stages`, `runs`, `attempts`, and `jobs` elements are data
frames suitable for programmatic queries.

## Details

The `overview` table contains one row for the selected scope. `stages`
and `subjects` aggregate its current work units; `subject_stages`
retains the stage and stream detail; `runs` summarizes submissions; and
`attempts` retains both current and superseded logical attempts. `jobs`
contains the underlying tracking rows and marks the rows contributing to
current project status with `is_current_attempt`. Printing the object or
its `jobs` component deliberately omits long scheduler, path, and
manifest fields, but those columns remain available for ordinary
data-frame access. Subject-wide stages use `NA` for `ses_id`; stages
that run separately by session retain their session identifier.

## See also

[`diagnose_project()`](https://hallquistlab.github.io/BrainGnomes/reference/diagnose_project.md)
for failure and log investigation.

## Examples

``` r
if (FALSE) { # \dontrun{
status <- inspect_project(scfg)
status
summary(status, by = "subject")
subset(status$subject_stages, sub_id == "014")

latest <- inspect_project(scfg, run_id = "latest")
subset(latest$jobs, lifecycle_status == "FAILED")
} # }
```
