# Inspecting and Diagnosing Pipeline Runs

## Overview

BrainGnomes separates routine progress inspection, diagnosis, and
recovery:

- [`inspect_project()`](https://hallquistlab.github.io/BrainGnomes/reference/inspect_project.md)
  answers, “What is happening now?”
- [`diagnose_project()`](https://hallquistlab.github.io/BrainGnomes/reference/diagnose_project.md)
  answers, “What needs attention, and what evidence is available?”
- [`reconcile_project_jobs()`](https://hallquistlab.github.io/BrainGnomes/reference/reconcile_project_jobs.md)
  checks known allocations for stale tracking records.
- [`resolve_project_submission()`](https://hallquistlab.github.io/BrainGnomes/reference/resolve_project_submission.md)
  records a confirmed outcome for one uncertain submission.

Inspection and diagnosis use stored tracking state and saved run
evidence by default. An optional read-only inspection refresh can
compare active records with the scheduler. Neither inspection nor
diagnosis repairs records or submits work. The two recovery functions
make changes only when explicitly requested, as described below.

``` r

library(BrainGnomes)
scfg <- load_project("/project/my_study")
```

Both functions also accept the project directory or configuration YAML
path directly. If the project root is the current working directory, the
input can be omitted:

``` r

setwd("/project/my_study")
inspect_project()
diagnose_project() # guided browser in an interactive R session
```

Automatic discovery checks only the current directory. It does not
search parent directories, and reports a clear error if
`project_config.yaml` is not there.

## Inspect current project progress

The default inspection integrates all tracked runs:

``` r

status <- inspect_project(scfg)
status
```

The printed dashboard gives overall counts, stage/stream progress, and a
bounded list of subjects that are active or need attention. Long
tracking fields such as scheduler options, paths, and manifests remain
available in the returned object but are not printed automatically.

The object contains ordinary data frames at several resolutions:

``` r

status$overview
status$active
status$reconciliation
status$stages
status$subjects
status$subject_stages
status$runs
status$attempts
status$submissions
status$jobs
```

The same tables are available through
[`summary()`](https://rdrr.io/r/base/summary.html):

``` r

summary(status)                         # one-row overview
summary(status, by = "stage")
summary(status, by = "subject")
summary(status, by = "subject_stage")
summary(status, by = "active")
```

Because these are data frames, ordinary R queries work without another
API:

``` r

subset(status$subjects, status != "COMPLETED")
subset(status$subject_stages, sub_id == "540296")
subset(status$jobs, lifecycle_status %in% c("RUNNING", "QUEUED"))
```

### Focus on one subject

Use `subject_id` with or without the `sub-` prefix to restrict every
resolution, including the run summaries and active-job table:

``` r

subject_status <- inspect_project(scfg, subject_id = "540294")
subject_status$overview
subject_status$runs
subject_status$subject_stages
subject_status$active
```

This is a true inspection scope rather than a display filter, so the
returned counts and summaries all describe that subject.

### Check active jobs against the scheduler

`status$active` is available without contacting the scheduler. It
reports database status, time in the current state, requested wall time,
and flags a running record that has exceeded that request. To compare
queued and running database records with their current scheduler states,
request a refresh:

``` r

refreshed <- inspect_project(scfg, refresh = TRUE)
refreshed$active
refreshed$reconciliation
```

The refresh is read-only. It preserves the database status, places the
scheduler observation beside it, and flags unconfirmed or mismatched
records without updating tracking data. Internally this uses the same
scheduler query path as
[`wait_for_job()`](https://hallquistlab.github.io/BrainGnomes/reference/wait_for_job.md),
including the existing Slurm, TORQUE, and local process status
functions.

### How status is integrated across runs

Each subject/session/stage/stream combination is treated as a logical
work unit. Related controller, array, and sentinel jobs are grouped into
that unit. For current project status, BrainGnomes uses the **newest
attempt** for every unit rather than the best status ever observed.

Consequently, a successful retry supersedes an older failure, while a
failed forced rerun is not hidden by an older success. Work omitted from
a later partial retry retains its newest earlier state. Historical
attempts remain available in `status$attempts` and `status$runs`.

`status$submissions` also includes submission attempts whose acceptance
was never confirmed, even when there is no scheduler job ID or worker
log. These are different from jobs that started and failed; an empty
`$jobs` or `$failures` table alone does not establish that the project
is ready to run.

Accepted attempts whose contract/allocation binding is missing or
inconsistent are also unconfirmed. Invalid JSON or nonscalar identity
metadata is exposed in `status$submissions$metadata_error`, rather than
crashing inspection. Preserve the original evidence and investigate that
exact attempt; do not guess its scope to clear a blocker.

The newest logical attempt does not erase an older outstanding
allocation. `status$active` includes older attempts and unconfirmed
dependency-blocked or unknown rows; `is_current_attempt` distinguishes
them. If the latest work is complete but an older allocation still lacks
termination evidence, the overview reports `ACTIVE_PRIOR_ATTEMPTS`
rather than declaring the project complete.

New tracking timestamps include an explicit UTC designator. Older
timestamps without a timezone are interpreted in the current R session’s
timezone; BrainGnomes cannot reconstruct a timezone that was never
recorded. When reviewing legacy history on a different computer, use the
original cluster’s timezone so run ordering remains meaningful, for
example:

``` r

Sys.setenv(TZ = "America/New_York")
status <- inspect_project(scfg)
```

Explicit UTC/offset timestamps retain their recorded meaning regardless
of this setting. Inspection does not rewrite historical timestamps.

Project-wide inspection describes work represented in the tracking
database. It does not invent a status for configured work that has never
been submitted.

### Interpreting statuses

Raw database statuses are retained in `status$jobs$status`. Summarized
views use friendlier lifecycle labels:

- `COMPLETED`: all jobs in the work unit completed.
- `RUNNING`: at least one job has started and no failure takes
  precedence.
- `QUEUED`: submitted work has not started.
- `FAILED`: the work itself failed.
- `BLOCKED`: the database records `FAILED_BY_EXT`, meaning required
  upstream work failed.
- `CANCELLED`: the work was cancelled.
- `UNKNOWN`: the recorded state is missing or unrecognized.

Counts are always retained alongside the convenience status, so mixed
states remain visible.

## Choose the run you mean

Use the run table to find historical IDs:

``` r

status$runs
run_id <- status$runs$run_id[[1L]]
```

For retry or cancellation, an explicit run ID is safer than `"latest"`
because another submission could become the newest run.

## Inspect one run without prompts

Supply `"latest"` or an explicit ID to restrict every inspection view to
one submission:

``` r

latest <- inspect_project(scfg, run_id = "latest")
selected <- inspect_project(scfg, run_id = run_id)

selected$overview
selected$subjects
selected$subject_stages
selected$jobs
```

The older
[`get_project_runs()`](https://hallquistlab.github.io/BrainGnomes/reference/get_project_runs.md)
and
[`get_run_jobs()`](https://hallquistlab.github.io/BrainGnomes/reference/get_run_jobs.md)
functions remain as deprecated compatibility wrappers. New code should
use `$runs` and `$jobs` from
[`inspect_project()`](https://hallquistlab.github.io/BrainGnomes/reference/inspect_project.md).

## Diagnose failures

Routine monitoring belongs in
[`inspect_project()`](https://hallquistlab.github.io/BrainGnomes/reference/inspect_project.md).
When current work has failed, call
[`diagnose_project()`](https://hallquistlab.github.io/BrainGnomes/reference/diagnose_project.md)
from an interactive R session to open the guided dependency and log
browser:

``` r

diagnose_project(scfg)
```

When submission issues or a saved preflight report exist, the browser
first offers a readiness and recovery review. It can show an uncertain
submission even if no scheduler job was recorded. Continuing to tracked
jobs shows unresolved failures grouped by stage and stream. A single-job
group opens that job directly; a repeated problem first narrows to the
affected subjects. Subject, run, and job selections are retained during
the drill-down, so the browser never returns to a full run-wide job list
unless you explicitly choose that broader view.

If you already know the subject or scheduler job ID, start at that
scope:

``` r

diagnose_project(scfg, subject_id = "540294")
diagnose_project(scfg, job_id = "66273010")
```

Scripts, tests, and reports are non-interactive, so the same call
returns a structured diagnosis there. Use `interactive = FALSE`
explicitly when you want that result in an interactive R session:

``` r

diagnosis <- diagnose_project(scfg, interactive = FALSE)
diagnosis$failures
diagnosis$logs
diagnosis$submission_issues
diagnosis$active_issues
diagnosis$submission_preflight
```

`submission_issues` identifies uncertain acceptance, confirmed
rejections that have not been superseded, and accepted attempts missing
their tracking binding. It includes each attempt’s `contract_id`, scope,
recorded detail, and suggested action. `active_issues` highlights timing
or accounting attention states in the inspection snapshot. To add fresh,
read-only scheduler observations, diagnose an inspection returned by
`inspect_project(scfg, refresh = TRUE)`.

An existing inspection snapshot can be reused:

``` r

diagnosis <- diagnose_project(status, interactive = FALSE)
```

For a historical post-mortem, select the run explicitly:

``` r

diagnosis <- diagnose_project(
  scfg, run_id = run_id, interactive = FALSE
)
```

Pass `interactive = TRUE` when code must open the guided browser
regardless of how R was started:

``` r

diagnose_project(scfg, run_id = run_id, interactive = TRUE)
```

[`diagnose_project()`](https://hallquistlab.github.io/BrainGnomes/reference/diagnose_project.md)
is the single diagnosis entry point for both the guided browser and
structured, non-interactive results.

## Understand automatic submission checks

Calling
[`run_project()`](https://hallquistlab.github.io/BrainGnomes/reference/run_project.md)
again is not a way around an uncertain earlier launch. Every live call
checks pending tracking metadata for the selected stages, subjects,
sessions, and streams before preparing directories, fingerprinting
containers, or submitting setup jobs. Confirmed scheduler failures and
cancellations repair stale active records automatically; the console
reports which records changed. Active allocations and uncertain
acceptance block the relevant request. Neither `force = TRUE` nor an
existing completion marker bypasses these safeguards. Per-submission
guards remain in place for launches that race with the initial check.

New tracked submission errors stop the submission loop at the first
error instead of repeating a failing launch across the cohort. Scheduler
stdout and stderr are retained beside the job manifest. Recognized
explicit Slurm policy/request rejections can be corrected and retried;
transport errors or lost acknowledgements remain uncertain because a job
might have been accepted. This safeguard concerns submission failures,
not a guarantee that later worker jobs will succeed.

The check is deliberately lightweight: it reads pending SQLite metadata
and batches accounting queries for relevant active jobs. It does not
read imaging files, scan output trees, load job manifests, or hash
containers. This is a submission-safety check, not full BIDS validation
or verification of outputs.

``` r

preview <- run_project(scfg, steps = "fmriprep", dry_run = TRUE)
preview$submission_preflight$blockers
preview$submission_preflight$scope_deferred
```

A dry run returns a plan even when pending metadata indicates blockers.
It does not query the scheduler, repair tracking records, or save a
preflight report file; a plan with no reported blockers is not a live
readiness certificate. `debug = TRUE`, in contrast, still submits jobs.

For unattended code, the live blocking condition carries the complete
report:

``` r

# This is a live submission if the check passes, not a preview.
run <- tryCatch(
  run_project(scfg, steps = "fmriprep"),
  bg_submission_preflight_error = function(error) {
    print(error$report$repairs)
    print(error$report$blockers)
    stop(error)
  }
)
```

An early blocker can stop submission before a new run record exists; use
the error’s `$report` and current diagnosis rather than assuming a new
run ID was created. Other submission errors are not necessarily this
condition class.

### When Flywheel can change the scope

With `flywheel_sync`, the initial preflight checks project-level setup
and controller blockers and labels subject/session scope as deferred.
Setup and sync jobs can be submitted after that gate; the provisional
local subject list is not certified. After synchronization, the
dependent controller discovers the actual scope, including newly arrived
subjects or sessions, and repeats preflight before any subject jobs are
launched.

The controller saves its assessment to
`<log_directory>/runs/<run_id>/submission-preflight.json`, including
when it blocks downstream submission. Read it through the existing
workflow APIs:

``` r

selected <- inspect_project(scfg, run_id = run_id)
recorded <- selected$submission_preflight
recorded$checked_at
recorded$scope_deferred
recorded$blockers
recorded$repairs

provenance <- get_run_provenance(scfg, run_id)
provenance$submission_preflight
```

Inspection, diagnosis, and provenance prefer this controller report over
the initial assessment when it exists. The original returned run handle
may still contain the initial deferred report: read the run again after
the controller has executed. A saved report is **historical evidence**,
not current readiness; its blockers remain part of that run’s history
after you resolve them. A missing report (`NULL`) is not proof of
readiness either. Use current `submission_issues`, a refreshed
inspection, and the next live preflight to assess unresolved problems. A
report read warning or `read_error` means the saved evidence could not
be read, not that the gate passed.

## Resolve submission and tracking problems

The exported recovery functions address two different kinds of evidence:

| Situation | Appropriate action |
|----|----|
| Known allocation, database still queued/running, scheduler confirms failure or cancellation | Preview [`reconcile_project_jobs()`](https://hallquistlab.github.io/BrainGnomes/reference/reconcile_project_jobs.md); apply with `dry_run = FALSE`, or let the next relevant live submission repair it. |
| Submission is `UNKNOWN` or `SUBMITTING` without a confirmed allocation | Investigate the exact attempt; use [`resolve_project_submission()`](https://hallquistlab.github.io/BrainGnomes/reference/resolve_project_submission.md) only with confirmed acceptance or rejection evidence. |
| Confirmed scheduler rejection | Correct the request, resources, account, or other rejection cause, then run again. Do not manually declare it accepted. |
| Scheduler still queued/running | Wait, or explicitly cancel when appropriate; do not launch a duplicate. |
| Accounting is missing/unavailable, or scheduler says completed but tracking remains active | Confirm the outcome or verify outputs; these observations do not automatically authorize a retry. |

### Reconcile a known scheduler job

``` r

# Contacts the scheduler, but does not change SQLite by default.
recovery <- reconcile_project_jobs(scfg)
recovery[, c("job_id", "status", "scheduler_status", "action", "applied")]
```

After reviewing the observations, explicitly apply confirmed failure or
cancellation repairs:

``` r

recovery <- reconcile_project_jobs(scfg, dry_run = FALSE)
```

`UPDATE_TERMINAL_FAILURE` identifies a proposed terminal repair;
`applied` records whether it was made. `CONFIRM_OUTCOME` leaves missing
or uncertain accounting untouched. `VERIFY_OUTPUTS` means scheduler
completion alone did not establish that required outputs are valid, so
the row remains unchanged. Other records are `UNCHANGED`. Neither
reconciliation mode submits or cancels jobs. Unlike the scoped automatic
gate, this manual operation reviews the project’s active tracked jobs.

Unknown legacy states and `FAILED_BY_EXT` allocations without confirmed
termination are included. A dependency-blocked status describes logical
work, not whether the scheduler has stopped the allocation.
Reconciliation records confirmed failure/cancellation in
`scheduler_terminal_state` while retaining `FAILED_BY_EXT`; run-based
retries still require `include_blocked = TRUE` for that work. Legacy
missing session metadata is treated as potentially overlapping a
requested session, not as permission to launch another job.

### Resolve one uncertain acceptance

Find the exact attempt, not just the participant or a row position:

``` r

diagnosis <- diagnose_project(scfg, interactive = FALSE)
diagnosis$submission_issues
attempts <- inspect_project(scfg)$submissions
contract_id <- "REPLACE_WITH_EXACT_CONTRACT_UUID"
attempt <- attempts[attempts$contract_id == contract_id, , drop = FALSE]
attempt
```

Review the attempt’s run, stage, subject/session, submission detail,
manifest, and saved scheduler stdout/stderr under the run directory.
Match scheduler accounting or administrator evidence to **that exact
attempt**. If acceptance is confirmed, bind its allocation:

``` r

resolve_project_submission(scfg, contract_id, scheduler_job_id = "12345")
```

Alternatively, **only if authoritative evidence establishes that no job
was accepted**, record that rejection instead:

``` r

resolve_project_submission(
  scfg, contract_id,
  rejection_reason = "Scheduler administrator confirmed this exact request was rejected."
)
```

These are mutually exclusive alternatives, not consecutive steps. Supply
exactly one of `scheduler_job_id` and `rejection_reason`. This function
has no dry-run mode: it updates the database immediately and preserves
the recorded evidence. Binding retains worker-reported state; it does
not verify outputs or make an active allocation safe to duplicate.
Rejection is allowed only for unbound `UNKNOWN` or `SUBMITTING`
attempts.

Queue absence, elapsed time, and missing logs are **not evidence of
rejection**. If the outcome cannot be confirmed, leave the attempt
unresolved and seek scheduler support rather than deleting tracking rows
or starting in a new directory to evade the guard. Keep the configured
log directory’s `runs/` tree together with SQLite when backing up or
moving a project.

### Keep input and environment problems separate

A BIDS-root `heudiconv.lock` blocks relevant work. BrainGnomes does not
remove it automatically: confirm whether conversion is still active and
investigate the lock before any manual cleanup. For input problems,
finish conversion, run
[`run_bids_validation()`](https://hallquistlab.github.io/BrainGnomes/reference/run_bids_validation.md)
separately, and review its report before launching preprocessing.
Changing output directories does not repair shared BIDS inputs; do not
rename or delete anatomical acquisitions just to silence validation.

[`doctor_project()`](https://hallquistlab.github.io/BrainGnomes/reference/doctor_project.md)
is an optional, read-only environment and recovery-evidence review. It
can help diagnose changed modules, permissions, containers, or missing
records, but neither replaces the automatic submission gate nor performs
either recovery action.

## Inspect run provenance

Before deciding what to rerun, inspect the settings and scope that
produced the selected run:

``` r

provenance <- get_run_provenance(scfg, run_id)
provenance$request
provenance$execution$subjects
provenance$configuration$snapshot_file
provenance$submission_preflight
```

The provenance bundle records the resolved stages and subjects,
configuration, resources, software, host, and fingerprints of
execution-driving files.

## Retry failed work after correcting the cause

Resolve uncertain acceptance before deciding what to rerun. A submission
that never acquired a tracked job may not appear in a run-based retry
plan; after confirming rejection and fixing its cause, repeat the
intended
[`run_project()`](https://hallquistlab.github.io/BrainGnomes/reference/run_project.md)
request. Use the run-based retry below for recorded failed or cancelled
work.

A retry creates a **new run**. It does not restart scheduler jobs in
place and does not change the original run. Always preview it first:

``` r

retry_plan <- retry_project_run(scfg, run_id, dry_run = TRUE)
retry_plan$jobs
retry_plan$subjects
```

After correcting the underlying cause, submit the retry explicitly:

``` r

retry_run <- retry_project_run(scfg, run_id, dry_run = FALSE)
```

Jobs marked `FAILED_BY_EXT` were blocked by an earlier failure and are
excluded by default. Include them when appropriate:

``` r

retry_plan <- retry_project_run(
  scfg, run_id, include_blocked = TRUE, dry_run = TRUE
)
```

## Cancel work that is still active

Cancellation targets outstanding tracked allocations, including
unconfirmed dependency-blocked jobs that may still be queued or running.
It does not delete data, logs, or completed outputs. Preview the
scheduler commands before making a separate explicit cancellation
request:

``` r

cancel_project_run(scfg, run_id, dry_run = TRUE)
cancel_project_run(scfg, run_id, dry_run = FALSE)
```

A successful cancellation command returns `cancellation_requested`, not
`cancelled`: acknowledgement does not prove all allocation processes
have stopped. SQLite remains unchanged, preserving any concurrent worker
completion. Use a refreshed inspection to observe progress, then
preview/apply
[`reconcile_project_jobs()`](https://hallquistlab.github.io/BrainGnomes/reference/reconcile_project_jobs.md)
once accounting confirms failure or cancellation. The next relevant live
[`run_project()`](https://hallquistlab.github.io/BrainGnomes/reference/run_project.md)
performs this reconciliation automatically. Missing accounting or a
still-active allocation continues to block a duplicate launch; a
cancellation request itself never bypasses the gate.

## Command-line inspection and recovery

The command line exposes the same inspection resolutions:

``` bash
# Replace this value with the run ID reported by BrainGnomes status.
run_id="REPLACE_WITH_RUN_ID"
BrainGnomes status /project/my_study
BrainGnomes status /project/my_study --view=stages
BrainGnomes status /project/my_study --view=subjects
BrainGnomes status /project/my_study --sub-id=540294
BrainGnomes status /project/my_study --view=active --refresh
BrainGnomes status /project/my_study --view=reconciliation
BrainGnomes status /project/my_study --run=latest --watch
BrainGnomes status /project/my_study --run="$run_id" --view=jobs --format=csv

BrainGnomes diagnose /project/my_study
BrainGnomes diagnose /project/my_study --run="$run_id" --interactive
BrainGnomes diagnose /project/my_study --subject-id=540294 --interactive
BrainGnomes diagnose /project/my_study --job-id=66273010 --interactive
BrainGnomes logs /project/my_study --run="$run_id" --failed-only --tail=50
BrainGnomes provenance /project/my_study --run="$run_id" --format=json

BrainGnomes retry /project/my_study --run="$run_id" --dry-run
BrainGnomes retry /project/my_study --run="$run_id" --yes
BrainGnomes cancel /project/my_study --run="$run_id" --dry-run
BrainGnomes cancel /project/my_study --run="$run_id" --yes
```

Retry and cancellation require either a dry-run preview or explicit
confirmation. Neither routine inspection nor diagnosis changes scheduler
state. The two explicit reconciliation/resolution functions above are R
APIs; this guide does not require separate CLI recovery subcommands.
