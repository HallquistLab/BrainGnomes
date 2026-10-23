# Run the project workflow

This remains the standard execution path after
[`setup_project()`](https://hallquistlab.github.io/BrainGnomes/reference/setup_project.md).
It resolves the same stages, streams, subject/session scope, and force
setting exposed by
[`plan_project()`](https://hallquistlab.github.io/BrainGnomes/reference/plan_project.md)
before submission; calling
[`plan_project()`](https://hallquistlab.github.io/BrainGnomes/reference/plan_project.md)
first is optional. Plans, dry runs, and submission apply the same
selected-stage configuration checks before creating directories, writing
provenance, or submitting jobs. When stages are selected interactively,
only omitted arguments are prompted for. Explicit choices, including
`dry_run = TRUE`, are always retained.

## Usage

``` r
run_project(
  scfg = getwd(),
  steps = NULL,
  subject_filter = NULL,
  postprocess_streams = NULL,
  extract_streams = NULL,
  debug = FALSE,
  force = FALSE,
  dry_run = FALSE,
  log_level = c("INFO", "DEBUG", "WARN", "ERROR", "TRACE", "FATAL")
)
```

## Arguments

- scfg:

  A `bg_project_cfg` object, YAML configuration file, or project
  directory containing `project_config.yaml`. Defaults to the current
  working directory.

- steps:

  Character vector of project stages to execute. Supported stages are
  `"flywheel_sync"`, `"bids_conversion"`, `"mriqc"`, `"fmriprep"`,
  `"aroma"`, `"postprocess"`, and `"extract_rois"`. Use `"all"` to run
  all enabled stages. If `NULL`, the user will be prompted for which
  stages to run. BIDS validation is configured with the project but
  submitted separately through
  [`run_bids_validation()`](https://hallquistlab.github.io/BrainGnomes/reference/run_bids_validation.md);
  it is not a `run_project()` stage.

- subject_filter:

  Optional character vector or data.frame specifying which subjects (and
  optionally sessions) to process. When omitted and run interactively,
  the user will be prompted to enter space-separated subject IDs (press
  ENTER to process all subjects). Explicit `NULL` selects all subjects
  without prompting. When a data.frame is provided, it must contain a
  `sub_id` column and may include a `ses_id` column to filter on
  specific subject/session combinations.

- postprocess_streams:

  Optional character vector specifying which postprocessing streams
  should run. When `"postprocess"` is included in `steps`, `NULL`
  selects every configured postprocessing stream.

- extract_streams:

  Optional character vector specifying which ROI extraction streams
  should run. When `"extract_rois"` is included in `steps`, `NULL`
  selects every configured extraction stream.

- debug:

  Logical. Enable verbose worker debugging and suppress imaging
  commands. Debug mode still submits scheduler jobs and writes
  tracking/log records; use `dry_run = TRUE` for a submission-free
  preview.

- force:

  Logical. Rerun selected work even when completion markers exist. Does
  not bypass active-job or uncertain-submission blockers.

- dry_run:

  A logical value indicating whether to perform a dry run. Dry runs
  validate settings and report subject/session scope plus resolved
  postprocessing and extraction stream settings without submitting any
  jobs.

- log_level:

  Character string controlling log verbosity. One of `TRACE`, `DEBUG`,
  `INFO`, `WARN`, `ERROR`, or `FATAL`.

## Value

For submitted work, an invisible `bg_project_run` object containing the
run UUID, scheduler job IDs known at submission time, and the path to
the complete run provenance record. Dry runs invisibly return a
`bg_project_plan` with concrete work units, paths, resources, and
current completion-marker decisions after printing the preview. No jobs
are submitted. Returned runs and dry-run plans include a
`submission_preflight` report. A blocking live preflight raises a
`bg_submission_preflight_error` whose `report` contains the full
`blockers` and `repairs` data frames.

## Details

Before submission, BrainGnomes reports when it is checking project
folders, finding matching subjects, and saving the run record. The first
use of a large container in a project may take longer because
BrainGnomes reads the complete file once to identify the exact copy
used. During large submissions, periodic messages report progress
through the subject list. A submission preflight runs before saving
provenance or submitting setup jobs. It checks pending SQLite metadata
and batches accounting queries for relevant active jobs, reports
automatic failure/cancellation repairs, and stops on unresolved blockers
even when `force = TRUE`. It does not read imaging files, scan output
trees, verify manifests, or hash containers. Dry runs report pending
metadata without querying the scheduler or repairing records, and do not
save a preflight report file. Their returned plan includes
`submission_preflight`. A dry-run plan can contain blockers and is not a
live readiness certificate. An early live blocker can stop the call
before a new run record exists; inspect the error's `report` rather than
assuming a new run ID was created.

With Flywheel synchronization, initial subject/session checks are
explicitly deferred. Project-level blockers are checked before
submission; the dependent controller checks the discovered
subject/session scope before submitting any subject jobs. Its own
project dependencies are allowed. Final per-submission guards remain in
place to catch concurrent launches after the preflight. The controller
saves its small report as `submission-preflight.json` in the run
directory, including when blockers prevent subject submission. Read the
saved report through
[`inspect_project()`](https://hallquistlab.github.io/BrainGnomes/reference/inspect_project.md)
or
[`get_run_provenance()`](https://hallquistlab.github.io/BrainGnomes/reference/get_run_provenance.md)
after the controller executes; the original run handle may still contain
the initial deferred assessment. Saved assessments are historical
evidence, not a live readiness verdict. Missing reports do not establish
readiness.

## See also

[`inspect_project()`](https://hallquistlab.github.io/BrainGnomes/reference/inspect_project.md)
to monitor current progress;
[`get_run_provenance()`](https://hallquistlab.github.io/BrainGnomes/reference/get_run_provenance.md)
to read the recorded configuration, execution context, and artifact
fingerprints;
[`plan_project()`](https://hallquistlab.github.io/BrainGnomes/reference/plan_project.md)
for optional inspection or persistence of the resolved request;
[`run_bids_validation()`](https://hallquistlab.github.io/BrainGnomes/reference/run_bids_validation.md)
to submit the BIDS validation configured with the project;
[`diagnose_project()`](https://hallquistlab.github.io/BrainGnomes/reference/diagnose_project.md)
and
[`retry_project_run()`](https://hallquistlab.github.io/BrainGnomes/reference/retry_project_run.md)
for optional recovery after a submitted run fails;
[`reconcile_project_jobs()`](https://hallquistlab.github.io/BrainGnomes/reference/reconcile_project_jobs.md)
and
[`resolve_project_submission()`](https://hallquistlab.github.io/BrainGnomes/reference/resolve_project_submission.md)
for explicit tracking/submission recovery. See
[`vignette("diagnosing_pipeline", package = "BrainGnomes")`](https://hallquistlab.github.io/BrainGnomes/articles/diagnosing_pipeline.md)
for the recovery decision guide.

## Examples

``` r
  if (FALSE) { # \dontrun{
    # Assuming you have a valid project configuration list named `study_config`
    run_project(study_config, steps = "fmriprep", force = FALSE)

    preview <- run_project(study_config, steps = "fmriprep", dry_run = TRUE)
    preview$submission_preflight$blockers
    preview$submission_preflight$scope_deferred

    # Live submission if ready; report blockers and rethrow if not.
    run <- tryCatch(
      run_project(study_config, steps = "fmriprep"),
      bg_submission_preflight_error = function(error) {
        print(error$report$repairs)
        print(error$report$blockers)
        stop(error)
      }
    )
  } # }
```
