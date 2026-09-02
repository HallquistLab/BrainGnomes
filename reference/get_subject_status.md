# Get processing status for a single subject

Get processing status for a single subject

## Usage

``` r
get_subject_status(scfg, sub_id, ses_id = NULL)
```

## Arguments

- scfg:

  a project configuration object as produced by `load_project` or
  `setup_project`

- sub_id:

  Subject identifier.

- ses_id:

  Optional session identifier. When `NULL`, all sessions found in the
  subject's directory are returned.

## Value

A data.frame with columns indicating completion status and times for
each enabled step. ROI-extraction streams use columns named
`extract_rois_<stream>_complete` and `extract_rois_<stream>_time`,
keeping them distinct from postprocessing streams with the same name.

## Details

This function verifies completion markers on disk. Use
[`inspect_project()`](https://hallquistlab.github.io/BrainGnomes/reference/inspect_project.md)
instead to inspect queued, running, failed, blocked, and completed jobs
recorded in the tracking database.

## See also

[`inspect_project()`](https://hallquistlab.github.io/BrainGnomes/reference/inspect_project.md)
for scheduler-lifecycle status.
