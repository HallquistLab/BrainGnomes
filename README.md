# BrainGnomes

<!-- badges: start -->
[![R-CMD-check](https://github.com/UNCDEPENdLab/BrainGnomes/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/UNCDEPENdLab/BrainGnomes/actions/workflows/R-CMD-check.yaml)
[![pkgdown](https://github.com/UNCDEPENdLab/BrainGnomes/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/UNCDEPENdLab/BrainGnomes/actions/workflows/pkgdown.yaml)
<!-- badges: end -->

BrainGnomes is an R package for configuring, submitting, and monitoring reproducible fMRI workflows on high-performance computing (HPC) systems. It coordinates containerized neuroimaging tools and scheduler jobs from one project configuration, while retaining logs and job-tracking information for each run.

The package supports the parts of a workflow that you need: optional Flywheel synchronization, DICOM-to-BIDS conversion with HeuDiConv, MRIQC, fMRIPrep, ICA-AROMA, postprocessing, and ROI time-series/connectivity extraction. BIDS validation is configured with the project but submitted separately through `run_bids_validation()`. You can begin with raw DICOMs or use it only for later steps when BIDS or fMRIPrep outputs already exist.

## Is BrainGnomes a good fit?

BrainGnomes is designed for studies run on an HPC cluster with a SLURM or TORQUE scheduler and containerized imaging software. It is especially useful when a project needs repeatable per-subject processing, configured resource requests, dependency-aware job submission, and a clear record of what completed or failed.

Installing and loading the R package does not require a cluster. Scheduler and
container requirements apply when you submit pipeline stages. Requirements are
stage-specific:

| Capability or stage | Additional requirements |
|---|---|
| Configuration inspection, BIDS filename helpers, status tables, and native image helpers | R 4.1 or later and the R dependencies installed with BrainGnomes; no scheduler or container |
| Any scheduled pipeline stage | SLURM or TORQUE/PBS, Bash, shared readable/writable project storage, and site-specific scheduler settings |
| Flywheel synchronization | Flywheel `fw` CLI and account access |
| DICOM-to-BIDS conversion | Singularity-compatible HeuDiConv image, source DICOMs, and a study-specific Python heuristic |
| BIDS validation | BIDS validator executable; configured with the project but submitted separately through `run_bids_validation()` |
| MRIQC | Singularity-compatible MRIQC image |
| fMRIPrep | Singularity-compatible fMRIPrep image, BIDS inputs, TemplateFlow cache, and a FreeSurfer license |
| ICA-AROMA | Singularity-compatible fMRIPost-AROMA image |
| Postprocessing | Singularity-compatible FSL image; Python 3 with `nibabel`, `nilearn`, and `templateflow` when template-mask resampling is used |
| ROI extraction | Postprocessed BOLD inputs and compatible atlas/mask NIfTI files; direct `extract_rois()` calls can run locally, while project-managed extraction uses the scheduler |

BrainGnomes scripts invoke `singularity`; an Apptainer installation is suitable
when it provides that compatibility command.

See the [Quickstart](https://uncdependlab.github.io/BrainGnomes/articles/braingnomes_quickstart.html) for the full configuration workflow, or start with [Local onboarding and prerequisites](https://uncdependlab.github.io/BrainGnomes/articles/local_onboarding.html) to inspect a miniature configuration and run examples without a cluster.

## Installation

BrainGnomes is installed from GitHub. In R:

```r
install.packages("remotes")  # once, if needed
remotes::install_github("UNCDEPENdLab/BrainGnomes")

library(BrainGnomes)
```

### Install a specific release

To install a particular tagged release rather than the latest development
version, supply its tag with `ref`. For example:

```r
remotes::install_github("UNCDEPENdLab/BrainGnomes", ref = "0.9")
```

See the [available tags](https://github.com/UNCDEPENdLab/BrainGnomes/tags)
to choose an available tag.

## Typical workflow

The installed command covers the lifecycle from a new project through recovery.
`doctor` is the submission-host preflight: it checks the configuration, scheduler
commands, container runtime, storage, stage files, and tracking database without
changing the project.

```bash
# Create portable defaults, then use `config edit` to enable and configure stages.
BrainGnomes init my_study /project/my_study --non-interactive
BrainGnomes config edit /project/my_study

# Validate first, inspect a serializable plan, then submit that exact plan.
BrainGnomes config validate /project/my_study
BrainGnomes doctor /project/my_study
BrainGnomes plan /project/my_study --steps=all --output=/project/my_study/run.yaml
BrainGnomes run /project/my_study/run.yaml

# Observe and recover by run ID (`latest` is accepted).
BrainGnomes status /project/my_study --runs
BrainGnomes status /project/my_study --run=latest --watch
BrainGnomes logs /project/my_study --run=latest --failed-only --tail=50
BrainGnomes diagnose /project/my_study --run=latest
BrainGnomes retry /project/my_study --run=latest --dry-run
BrainGnomes cancel /project/my_study --run=latest --dry-run
```

The same lifecycle is available as composable R functions:

```r
library(BrainGnomes)

# Guided setup remains available; initialize_project(..., interactive = FALSE)
# provides the scriptable equivalent used by `BrainGnomes init`.
scfg <- setup_project()

validation <- validate_project_config(scfg)
preflight <- doctor(scfg)
plan <- plan_project(scfg, steps = "all")
write_project_plan(plan, "run.yaml")

run <- submit_project_plan(plan)
get_project_runs(scfg)
get_run_jobs(scfg, run$run_id)
diagnose_project(scfg, run$run_id)
```

Plans can target selected subjects, stages, postprocessing streams, or
ROI-extraction streams. BIDS validation remains independently schedulable with
`run_bids_validation()` or `BrainGnomes validate-bids`. Destructive CLI actions
require either `--dry-run` or explicit `--yes`. The
[Quickstart](https://uncdependlab.github.io/BrainGnomes/articles/braingnomes_quickstart.html)
shows interactive and scripted variants.

## Documentation

The [package website](https://uncdependlab.github.io/BrainGnomes/) includes function reference pages, release notes, and the following guides:

- [BrainGnomes Quickstart](https://uncdependlab.github.io/BrainGnomes/articles/braingnomes_quickstart.html) — set up a project and run an end-to-end workflow.
- [Local onboarding and prerequisites](https://uncdependlab.github.io/BrainGnomes/articles/local_onboarding.html) — inspect an installed example configuration and run a first task without scheduler or container access.
- [Building Singularity containers for BrainGnomes](https://uncdependlab.github.io/BrainGnomes/articles/building_containers.html) — create the container images used by pipeline stages.
- [Postprocessing Walkthrough](https://uncdependlab.github.io/BrainGnomes/articles/postprocessing.html) — configure masking, smoothing, AROMA, filtering, scrubbing, intensity normalization, and confound regression.
- [Extracting ROI Timeseries and Connectivity](https://uncdependlab.github.io/BrainGnomes/articles/extract_rois.html) — configure atlas/mask ROI extraction and connectivity outputs.
- [Diagnosing Pipeline Runs](https://uncdependlab.github.io/BrainGnomes/articles/diagnosing_pipeline.html) — triage project or subject status and investigate failures from job-tracking records and logs.
- [Run-wise Intensity Normalization](https://uncdependlab.github.io/BrainGnomes/articles/intensity_normalization.html) — understand the robust reference-core approach, targets, provenance, quality checks, and troubleshooting.

## Getting help and contributing

Please [open an issue](https://github.com/UNCDEPENdLab/BrainGnomes/issues) for bugs, questions, or feature requests. Contributions are welcome; see [CONTRIBUTING.md](CONTRIBUTING.md) for the development workflow.
