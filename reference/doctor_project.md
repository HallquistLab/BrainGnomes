# Run non-mutating project and runtime preflight checks

This optional comprehensive preflight is useful on a new cluster, after
the submission environment changes, or before an expensive run.
`doctor_project()` checks configuration, scheduler commands, container
runtime, enabled-stage files, project storage, and the job-tracking
database. It is not required before
[`run_project()`](https://hallquistlab.github.io/BrainGnomes/reference/run_project.md)
and does not submit work, create directories, or modify the
configuration.

## Usage

``` r
doctor_project(input = getwd(), steps = NULL, deep = FALSE, quiet = FALSE)
```

## Arguments

- input:

  A project configuration object, YAML file, or project directory.
  Defaults to the current working directory.

- steps:

  Optional stages to check. By default all enabled stages are used.

- deep:

  Also initialize Python and check optional postprocessing modules.

- quiet:

  Suppress the printed report.

## Value

A `bg_project_doctor` object with an `ok` flag and a checks data frame.

## Details

Doctor also reports uncertain submissions, missing recovery evidence,
and BIDS conversion locks without resolving them. It is distinct from
the automatic, request-scoped submission gate in
[`run_project()`](https://hallquistlab.github.io/BrainGnomes/reference/run_project.md),
which can repair confirmed scheduler failures/cancellations. Use
[`diagnose_project()`](https://hallquistlab.github.io/BrainGnomes/reference/diagnose_project.md)
for the recovery decision guide and current submission issues.

## See also

[`reconcile_project_jobs()`](https://hallquistlab.github.io/BrainGnomes/reference/reconcile_project_jobs.md)
and
[`resolve_project_submission()`](https://hallquistlab.github.io/BrainGnomes/reference/resolve_project_submission.md)
for explicit recovery. See
[`vignette("diagnosing_pipeline", package = "BrainGnomes")`](https://hallquistlab.github.io/BrainGnomes/articles/diagnosing_pipeline.md)
for how environment review, automatic preflight, and recovery fit
together.
