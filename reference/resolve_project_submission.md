# Resolve an uncertain submission after confirming its outcome

Supply either the confirmed scheduler allocation ID or a rejection
reason describing authoritative evidence that no job was accepted. Queue
absence, elapsed time, and missing logs are not evidence of rejection.
This is an explicit administrative recovery operation; it never guesses
an outcome. Binding an allocation preserves any worker-reported status.
Rejection is allowed only for `UNKNOWN` or `SUBMITTING` attempts with no
bound job.

## Usage

``` r
resolve_project_submission(
  input = getwd(),
  contract_id,
  scheduler_job_id = NULL,
  rejection_reason = NULL
)
```

## Arguments

- input:

  Project configuration, YAML file, or project directory.

- contract_id:

  Exact UUID from `inspect_project(input)$submissions`.

- scheduler_job_id:

  Confirmed root allocation ID in the recorded scheduler's format, or
  `NULL`. Slurm task IDs and diagnostic text are not allocation IDs;
  TORQUE element IDs are canonicalized to their array root.

- rejection_reason:

  Evidence of confirmed rejection, or `NULL`.

## Value

The resolved submission record, invisibly.

## Details

There is no dry-run mode: this function updates the database
immediately. Supply exactly one of `scheduler_job_id` and
`rejection_reason`, for the exact recorded attempt. Binding does not
verify outputs or authorize a duplicate launch. If acceptance cannot be
confirmed or ruled out, leave the attempt unresolved and seek scheduler
support. Do not delete tracking rows or move to a new output directory
to bypass the submission guard.

After resolution, inspect the current state again. Correct confirmed
rejection causes before repeating the intended
[`run_project()`](https://hallquistlab.github.io/BrainGnomes/reference/run_project.md)
request. An unbound attempt may have no tracked job for
[`retry_project_run()`](https://hallquistlab.github.io/BrainGnomes/reference/retry_project_run.md)
to select.

## See also

[`diagnose_project()`](https://hallquistlab.github.io/BrainGnomes/reference/diagnose_project.md)
for submission issues and
[`reconcile_project_jobs()`](https://hallquistlab.github.io/BrainGnomes/reference/reconcile_project_jobs.md)
for known scheduler allocations. See
[`vignette("diagnosing_pipeline", package = "BrainGnomes")`](https://hallquistlab.github.io/BrainGnomes/articles/diagnosing_pipeline.md)
for evidence requirements and the end-to-end recovery workflow.

## Examples

``` r
if (FALSE) { # \dontrun{
scfg <- load_project("/project/my_study")
diagnosis <- diagnose_project(scfg, interactive = FALSE)
diagnosis$submission_issues
contract_id <- "REPLACE_WITH_EXACT_CONTRACT_UUID"

# Only after confirming this exact attempt was accepted:
resolve_project_submission(scfg, contract_id, scheduler_job_id = "12345")
} # }
if (FALSE) { # \dontrun{
# Alternative to binding: only after authoritative confirmation of rejection.
# Do not run this after binding the same attempt above.
resolve_project_submission(scfg, contract_id,
  rejection_reason = "Administrator confirmed this exact request was rejected.")
} # }
```
