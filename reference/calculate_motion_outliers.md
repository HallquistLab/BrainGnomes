# Summarize framewise displacement outliers across runs

Calculates the percentage of framewise displacement (FD) values that
exceed one or more thresholds for each run (confounds file) in a
project. When requested, FD is recomputed after filtering the motion
parameters (notch or low-pass) to provide filtered outlier percentages
alongside the unfiltered values. This helper is intended for interactive
use and is not part of the postprocessing stream.

## Usage

``` r
calculate_motion_outliers(
  scfg = NULL,
  input_dir = NULL,
  confounds_files = NULL,
  thresholds = 0.3,
  include_filtered = FALSE,
  filter_method = c("notch", "lowpass"),
  tr = NULL,
  bandstop_min_bpm = 12,
  bandstop_max_bpm = 18,
  low_pass_hz = NULL,
  filter_order = NULL,
  motion_cols = c("rot_x", "rot_y", "rot_z", "trans_x", "trans_y", "trans_z"),
  rot_units = c("rad", "deg"),
  output_file = NULL
)
```

## Arguments

- scfg:

  Optional project configuration object produced by
  [`load_project()`](https://hallquistlab.github.io/BrainGnomes/reference/load_project.md)
  or
  [`setup_project()`](https://hallquistlab.github.io/BrainGnomes/reference/setup_project.md).
  If provided, the fMRIPrep directory is taken from
  `scfg$metadata$fmriprep_directory`.

- input_dir:

  Optional directory to search for fMRIPrep confounds files. Ignored
  when `confounds_files` is provided.

- confounds_files:

  Optional character vector of confounds TSV files to summarize
  directly. If supplied, no directory search is performed.

- thresholds:

  Numeric vector of FD thresholds (in mm). Percentages are returned for
  each threshold with column names like `fd_gt_0p5`.

- include_filtered:

  Logical; if `TRUE`, recompute FD after filtering the motion parameters
  and include filtered outlier percentages (columns prefixed with
  `fd_filt_`). If the requested filter cannot be applied, these columns
  are retained and filled with `NA` so unavailable results cannot be
  mistaken for filtered measurements.

- filter_method:

  Filtering strategy when `include_filtered = TRUE`. Either `"notch"`
  (band-stop, in breaths per minute) or `"lowpass"` (Hz).

- tr:

  Repetition time in seconds, required when `include_filtered = TRUE`.

- bandstop_min_bpm:

  Lower notch stop-band bound in breaths per minute (default 12; used
  when `filter_method = "notch"`).

- bandstop_max_bpm:

  Upper notch stop-band bound in breaths per minute (default 18; used
  when `filter_method = "notch"`).

- low_pass_hz:

  Low-pass cutoff in Hz (required for `filter_method = "lowpass"`).

- filter_order:

  Optional integer filter order. Defaults to 4 for notch filtering and 2
  for low-pass filtering.

- motion_cols:

  Motion parameter columns used to recompute FD.

- rot_units:

  Rotation unit for motion parameters (`"rad"` or `"deg"`).

- output_file:

  Optional path to write results as a tab-separated file. If provided,
  results are written using
  [`data.table::fwrite()`](https://rdrr.io/pkg/data.table/man/fwrite.html).

## Value

A data.frame with subject, session, task, run, confounds file location,
max FD, mean FD, and outlier percentages for each threshold (filtered
columns are included when requested). Filtered summary values are `NA`
when motion filtering was requested but could not be applied.

## See also

[`vignette("motion_qc", package = "BrainGnomes")`](https://hallquistlab.github.io/BrainGnomes/articles/motion_qc.md)
for a complete run-level QC workflow and its relationship to
postprocessing scrubbing.

## Examples

``` r
if (FALSE) { # \dontrun{
scfg <- load_project("/path/to/project_config.yaml")
out <- calculate_motion_outliers(scfg = scfg, thresholds = c(0.3, 0.5))

out_filt <- calculate_motion_outliers(
  scfg = scfg,
  thresholds = c(0.3, 0.5),
  include_filtered = TRUE,
  filter_method = "notch",
  tr = 2,
  bandstop_min_bpm = 18,
  bandstop_max_bpm = 24
)
} # }
```
