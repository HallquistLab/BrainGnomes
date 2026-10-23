# Recover stale active job records using scheduler accounting

Preview scheduler observations with the default `dry_run = TRUE`. With
`dry_run = FALSE`, only confirmed scheduler failures and cancellations
update active database rows. Missing accounting records and unavailable
schedulers never authorize retries. Scheduler completion alone does not
establish that required outputs or completion manifests are valid; these
rows are returned with action `VERIFY_OUTPUTS` and remain unchanged.

## Usage

``` r
reconcile_project_jobs(input = getwd(), dry_run = TRUE)
```

## Arguments

- input:

  Project configuration, YAML file, or project directory.

- dry_run:

  Preview changes without updating SQLite. Defaults to `TRUE`.

## Value

A data frame with database and scheduler states, proposed actions, and
an `applied` flag for each active tracked job. `UPDATE_TERMINAL_FAILURE`
proposes a confirmed failure/cancellation repair; `CONFIRM_OUTCOME` and
`VERIFY_OUTPUTS` require further investigation and leave records
unchanged. Other records have action `UNCHANGED`.

## Details

The recorded end time for a recovered failure is the reconciliation time
when no worker end time exists. This function submits and cancels no
jobs. Unbound uncertain submissions require
[`resolve_project_submission()`](https://hallquistlab.github.io/BrainGnomes/reference/resolve_project_submission.md).

Unlike a
[`run_project()`](https://hallquistlab.github.io/BrainGnomes/reference/run_project.md)
preflight, this explicit operation reviews all active tracked jobs in
the project, not just the scope of a new request. Its preview contacts
the scheduler even though it does not update SQLite. Neither mode reads
imaging files or verifies completed outputs. Unknown legacy tracking
states and dependency-blocked allocations without confirmed scheduler
termination are included. For a dependency-blocked row, confirmed
failure/cancellation records `scheduler_terminal_state` but retains
logical status `FAILED_BY_EXT`; run-based retries still require
`include_blocked = TRUE` to select that work.

For future runs, tracked submission errors stop the submission loop at
the first failure, retaining scheduler stdout and stderr beside the job
manifest. Explicit supported Slurm policy/request rejections are
retryable; transport errors remain uncertain. Retries automatically
reconcile matching active attempts against confirmed scheduler failures
before checking duplicates.

Keep `logs/runs` with the tracking database: it contains the evidence
needed for recovery. For input problems, finish conversion before
preprocessing, run
[`run_bids_validation()`](https://hallquistlab.github.io/BrainGnomes/reference/run_bids_validation.md)
and inspect its report before launching a cohort. Changing the output
directory does not repair shared BIDS inputs. Never remove or rename
anatomical files solely to suppress validator errors; determine which
acquisitions and derived files should be retained first.

## See also

[`inspect_project()`](https://hallquistlab.github.io/BrainGnomes/reference/inspect_project.md)
for status,
[`diagnose_project()`](https://hallquistlab.github.io/BrainGnomes/reference/diagnose_project.md)
for recovery guidance, and
[`resolve_project_submission()`](https://hallquistlab.github.io/BrainGnomes/reference/resolve_project_submission.md)
for uncertain acceptance. See
[`vignette("diagnosing_pipeline", package = "BrainGnomes")`](https://hallquistlab.github.io/BrainGnomes/articles/diagnosing_pipeline.md)
for the recovery decision guide and automatic preflight workflow.

## Examples

``` r
if (FALSE) { # \dontrun{
scfg <- load_project("/project/my_study")
recovery <- reconcile_project_jobs(scfg)
recovery[, c("job_id", "status", "scheduler_status", "action", "applied")]
# After reviewing the observations, explicitly apply confirmed repairs.
reconcile_project_jobs(scfg, dry_run = FALSE)
} # }
```
