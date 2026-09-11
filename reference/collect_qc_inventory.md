# Collect a study QC inventory from existing diagnostics

Assemble a read-only snapshot of acquisitions, expected native
derivatives, workflow attempts, output-manifest checks, and available QC
evidence. No image processing, human review, or inclusion/exclusion
decisions are run.

## Usage

``` r
collect_qc_inventory(input = getwd(), subjects = NULL, refresh = FALSE)
```

## Arguments

- input:

  A project configuration object, YAML file, or project directory.

- subjects:

  Optional data frame with character `sub_id` and optional `ses_id`
  columns. Adds expected participants/sessions, including those whose
  data have not arrived. Discovered and tracked subjects are always
  retained.

- refresh:

  Query the scheduler through
  [`inspect_project()`](https://hallquistlab.github.io/BrainGnomes/reference/inspect_project.md)
  for active jobs. Defaults to `FALSE`; the snapshot then uses recorded
  database states.

## Value

A `bg_qc_inventory` list with `metadata`, `acquisitions`, `inventory`
(one row per derivative/stage/stream), `workflow`, `metrics`,
`evidence`, `checks`, `jobs`, `attempts`, `manifest_files`, `active`,
`reconciliation`, and `issues`. Missing diagnostics are explicit; an
available file is not evidence of scientific quality or a successful
producing attempt.

## Details

Expected postprocessing paths are resolved from existing fMRIPrep inputs
and the current stream configuration; ROI targets follow those
postprocessing paths. Before upstream inputs exist, configured work
remains visible in `workflow`, without guessing output spaces or echo
combinations. `acquisitions` inventories raw BOLD files separately,
including each echo. Historical jobs and exact manifest file
associations remain available. Subject/session workflow states provide
context rather than assigning a producing job to every acquisition.
File-manifest comparisons use size and modification time, not a content
hash. Diagnostics have their own file timestamps and can outlive the
derivative they describe.

## Examples

``` r
if (FALSE) { # \dontrun{
qc <- collect_qc_inventory("/path/to/project")
subset(qc$inventory, output_status == "missing")
write_qc_inventory(qc, "/path/to/qc-snapshot")
render_qc_dashboard(qc, "/path/to/qc-dashboard")
} # }
```
