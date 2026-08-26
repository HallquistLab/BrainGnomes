# BrainGnomes

BrainGnomes is an R package for configuring, submitting, and monitoring
reproducible fMRI workflows on high-performance computing (HPC) systems.
It coordinates containerized neuroimaging tools and scheduler jobs from
one project configuration, while retaining logs and job-tracking
information for each run.

The package supports the parts of a workflow that you need: optional
Flywheel synchronization, DICOM-to-BIDS conversion with HeuDiConv,
MRIQC, fMRIPrep, ICA-AROMA, postprocessing, and ROI
time-series/connectivity extraction. BIDS validation is configured with
the project but submitted separately through
[`run_bids_validation()`](https://uncdependlab.github.io/BrainGnomes/reference/run_bids_validation.md).
You can begin with raw DICOMs or use it only for later steps when BIDS
or fMRIPrep outputs already exist.

## Is BrainGnomes a good fit?

BrainGnomes is designed for studies run on an HPC cluster with a SLURM
or TORQUE scheduler and containerized imaging software. It is especially
useful when a project needs repeatable per-subject processing,
configured resource requests, dependency-aware job submission, and a
clear record of what completed or failed.

Installing and loading the R package does not require a cluster.
Scheduler and container requirements apply when you submit pipeline
stages. Requirements are stage-specific:

| Capability or stage | Additional requirements |
|----|----|
| Configuration inspection, BIDS filename helpers, status tables, and native image helpers | R 4.1 or later and the R dependencies installed with BrainGnomes; no scheduler or container |
| Any scheduled pipeline stage | SLURM or TORQUE/PBS, Bash, shared readable/writable project storage, and site-specific scheduler settings |
| Flywheel synchronization | Flywheel `fw` CLI and account access |
| DICOM-to-BIDS conversion | Singularity-compatible HeuDiConv image, source DICOMs, and a study-specific Python heuristic |
| BIDS validation | BIDS validator executable; configured with the project but submitted separately through [`run_bids_validation()`](https://uncdependlab.github.io/BrainGnomes/reference/run_bids_validation.md) |
| MRIQC | Singularity-compatible MRIQC image |
| fMRIPrep | Singularity-compatible fMRIPrep image, BIDS inputs, TemplateFlow cache, and a FreeSurfer license |
| ICA-AROMA | Singularity-compatible fMRIPost-AROMA image |
| Postprocessing | Singularity-compatible FSL image; Python 3 with `nibabel`, `nilearn`, and `templateflow` when template-mask resampling is used |
| ROI extraction | Postprocessed BOLD inputs and compatible atlas/mask NIfTI files; direct [`extract_rois()`](https://uncdependlab.github.io/BrainGnomes/reference/extract_rois.md) calls can run locally, while project-managed extraction uses the scheduler |

BrainGnomes scripts invoke `singularity`; an Apptainer installation is
suitable when it provides that compatibility command.

See the
[Quickstart](https://uncdependlab.github.io/BrainGnomes/articles/braingnomes_quickstart.html)
for the full configuration workflow, or start with [Local onboarding and
prerequisites](https://uncdependlab.github.io/BrainGnomes/articles/local_onboarding.html)
to inspect a miniature configuration and run examples without a cluster.

## Installation

BrainGnomes is installed from GitHub. In R:

``` r

install.packages("remotes")  # once, if needed
remotes::install_github("UNCDEPENdLab/BrainGnomes")

library(BrainGnomes)
```

### Install a specific release

To install a particular tagged release rather than the latest
development version, supply its tag with `ref`. For example:

``` r

remotes::install_github("UNCDEPENdLab/BrainGnomes", ref = "0.9")
```

See the [available
tags](https://github.com/UNCDEPENdLab/BrainGnomes/tags) to choose an
available tag.

## Typical workflow

1.  Create an interactive project configuration.
    [`setup_project()`](https://uncdependlab.github.io/BrainGnomes/reference/setup_project.md)
    records project paths, enabled pipeline stages, container locations,
    scheduler settings, and resource requests in `project_config.yaml`.
2.  Review or update that configuration with
    [`edit_project()`](https://uncdependlab.github.io/BrainGnomes/reference/edit_project.md),
    or reload it later with
    [`load_project()`](https://uncdependlab.github.io/BrainGnomes/reference/load_project.md).
3.  Submit the enabled stages with
    [`run_project()`](https://uncdependlab.github.io/BrainGnomes/reference/run_project.md).
    Jobs are submitted per subject/session with their dependencies
    tracked automatically.
4.  Check progress with
    [`get_project_status()`](https://uncdependlab.github.io/BrainGnomes/reference/get_project_status.md)
    or
    [`get_subject_status()`](https://uncdependlab.github.io/BrainGnomes/reference/get_subject_status.md),
    including per-stream postprocessing and ROI-extraction completion.
    Scheduled ROI extraction records and verifies the exact files
    produced by each job. Use
    [`diagnose_pipeline()`](https://uncdependlab.github.io/BrainGnomes/reference/diagnose_pipeline.md)
    to inspect the tracked job tree and logs when a run needs attention.

``` r

library(BrainGnomes)

# Creates and saves project_config.yaml after guided setup.
scfg <- setup_project()

# Validate the planned work without submitting jobs.
run_project(scfg, steps = "all", dry_run = TRUE)

# When ready, submit enabled processing stages.
run_project(scfg, steps = "all")
```

[`run_project()`](https://uncdependlab.github.io/BrainGnomes/reference/run_project.md)
can also target selected subjects, stages, postprocessing streams, or
ROI-extraction streams. The
[Quickstart](https://uncdependlab.github.io/BrainGnomes/articles/braingnomes_quickstart.html)
shows both interactive and scripted examples.

## Documentation

The [package website](https://uncdependlab.github.io/BrainGnomes/)
includes function reference pages, release notes, and the following
guides:

- [BrainGnomes
  Quickstart](https://uncdependlab.github.io/BrainGnomes/articles/braingnomes_quickstart.html)
  — set up a project and run an end-to-end workflow.
- [Local onboarding and
  prerequisites](https://uncdependlab.github.io/BrainGnomes/articles/local_onboarding.html)
  — inspect an installed example configuration and run a first task
  without scheduler or container access.
- [Building Singularity containers for
  BrainGnomes](https://uncdependlab.github.io/BrainGnomes/articles/building_containers.html)
  — create the container images used by pipeline stages.
- [Postprocessing
  Walkthrough](https://uncdependlab.github.io/BrainGnomes/articles/postprocessing.html)
  — configure masking, smoothing, AROMA, filtering, scrubbing, intensity
  normalization, and confound regression.
- [Extracting ROI Timeseries and
  Connectivity](https://uncdependlab.github.io/BrainGnomes/articles/extract_rois.html)
  — configure atlas/mask ROI extraction and connectivity outputs.
- [Diagnosing Pipeline
  Runs](https://uncdependlab.github.io/BrainGnomes/articles/diagnosing_pipeline.html)
  — triage project or subject status and investigate failures from
  job-tracking records and logs.
- [Run-wise Intensity
  Normalization](https://uncdependlab.github.io/BrainGnomes/articles/intensity_normalization.html)
  — understand the robust reference-core approach, targets, provenance,
  quality checks, and troubleshooting.

## Getting help and contributing

Please [open an
issue](https://github.com/UNCDEPENdLab/BrainGnomes/issues) for bugs,
questions, or feature requests. Contributions are welcome; see
[CONTRIBUTING.md](https://uncdependlab.github.io/BrainGnomes/CONTRIBUTING.md)
for the development workflow.
