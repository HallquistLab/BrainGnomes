# BrainGnomes: HPC Workflows for Functional MRI Preprocessing and Analysis

Configures, submits, and monitors reproducible functional magnetic
resonance imaging workflows on high-performance computing clusters using
containerized neuroimaging tools. The package can be installed and its
configuration, BIDS, status, and native imaging helpers can be used
without a cluster. Full pipeline execution requires a supported
scheduler and stage-specific external software; template-dependent
operations may also use Python and TemplateFlow.

## Details

![BrainGnomes: gnomes working together on brain-processing assembly
lines.](figures/braingnomes-header.png)

Start with
[`setup_project()`](https://hallquistlab.github.io/BrainGnomes/reference/setup_project.md)
and
[`run_project()`](https://hallquistlab.github.io/BrainGnomes/reference/run_project.md),
then monitor progress with
[`inspect_project()`](https://hallquistlab.github.io/BrainGnomes/reference/inspect_project.md).
Use
[`diagnose_project()`](https://hallquistlab.github.io/BrainGnomes/reference/diagnose_project.md)
to investigate failures and unresolved submissions, and
[`collect_qc_inventory()`](https://hallquistlab.github.io/BrainGnomes/reference/collect_qc_inventory.md)
to review a saved QC snapshot. The Quickstart introduces the complete
workflow:
[`vignette("braingnomes_quickstart", package = "BrainGnomes")`](https://hallquistlab.github.io/BrainGnomes/articles/braingnomes_quickstart.md).

## See also

Useful links:

- <https://hallquistlab.github.io/BrainGnomes/>

- Report bugs at <https://github.com/HallquistLab/BrainGnomes/issues>

## Author

**Maintainer**: Michael Hallquist <michael.hallquist@gmail.com>

Authors:

- Michael Hallquist <michael.hallquist@gmail.com>
