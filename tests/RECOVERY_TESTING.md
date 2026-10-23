# Job tracking and recovery verification

Regular tests are isolated from production projects. They create temporary
SQLite databases and small synthetic files; they never contact a real scheduler
unless a dedicated integration guard is enabled.

## Fast local regression suite

```sh
Rscript -e 'devtools::test(filter = "recovery-audit|project-recovery|submission-preflight|job_tracking|job-submission-durability|job-contracts|worker-status-persistence|hpc-job-status|inspect-project|lifecycle-functions|submit-subject-progress")'
```

The suite deliberately mixes pure logic tests, actual SQLite transactions,
independent processes, and executable scheduler stand-ins:

| Boundary | Regression coverage |
| --- | --- |
| Durable submission | Two independent submitters compete for the same unit; only one reaches a scheduler executable. Registration failures and binding failures cannot leave partial acceptance. |
| Lost acknowledgements | A real shell/R worker binds and completes before a failing client returns. Accepted worker evidence survives; genuinely unconfirmed launches block duplicates. |
| Cohort fan-out | A 49-participant controller encounters an actual failing `sbatch` stand-in. Both policy rejection and transport failure stop after one invocation and preserve stderr. |
| Recovery races | An executable `sacct` stand-in launches a separate SQLite writer during observation. Compare-and-set repairs preserve concurrent worker completion. |
| Scope | Stage, subject, session, stream, exact retry combinations, unknown legacy sessions, and data discovered after Flywheel sync are checked without broadening unrelated work. |
| Dependency tracking | Cascades preserve started/completed/cancelled/failed descendants, terminate on legacy cycles, and honor excluded branches. Blocked allocations remain guarded and cancellable until accounting confirms termination. |
| Cancellation | Command success means a request, not terminal evidence. Immediate retries remain blocked while the allocation is active. |
| Inspection | Mixed timestamp formats, accepted attempts missing bindings, active older attempts, unknown states, corrupt metadata, unreadable reports, and missing databases cannot silently imply readiness. |
| Scheduler adapters | Array tasks and steps, held/running/failed/cancelled states, missing/expired accounting, command errors, shared Slurm owners, and TORQUE exit codes are represented explicitly. |
| Cost boundaries | Pending scalar metadata only; no image/container/manifest reads or job-object deserialization in preflight. SQLite query-plan checks prove pending scans use a covering index rather than completed BLOB history. Accounting batches and timeouts are tested. |

Scheduler stand-ins validate subprocess boundaries and arguments, not a site's
Slurm/TORQUE installation, retention policy, permissions, or filesystem behavior.
Regular unit tests must remain safe on a laptop and on an HPC login node.
The Slurm adapter's array expansion and owner-filter options follow the
[SchedMD sacct interface](https://slurm.schedmd.com/sacct.html); executable
stand-ins do not reproduce accounting-daemon lag or site privacy policy.

## Opt-in Slurm integration

Use an interactive allocation first, with the same R/dependency environment as
production and a writable scratch directory shared with compute nodes:

```sh
srun --pty -N 1 -n 1 --mem=16g --time=01:00:00 bash
export BG_TEST_RECOVERY_SLURM=1
export BG_RECOVERY_TEST_ROOT=/path/to/shared/test-scratch
Rscript -e 'devtools::test(filter = "recovery-integration-srun")'
```

This explicitly submits two tiny jobs through `cluster_job_submit()`: one exits
with code 7, and one is held and cancelled before it can run. It checks real
accounting, default read-only previews, applied recovery, and cancellation
request semantics. Polling is bounded; cancellation cleanup is attempted even
after an assertion fails. Each test retains its temporary run directory for
inspection. Delete it only after checking the printed path and confirming its
allocations have stopped. No imaging data or containers are involved.

The test intentionally skips without **both** the opt-in flag and an interactive
allocation. It also requires the explicit shared scratch path and scheduler
commands. A skipped integration test is not evidence that site-specific behavior
has been verified. Follow the separate container integration instructions for
fMRIPrep/MRIQC/AROMA/FSL runtime problems; this test checks scheduling and
tracking, not scientific outputs.
