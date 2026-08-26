# Run the processing pipeline

Run the processing pipeline

## Usage

``` r
run_project(
  scfg,
  steps = NULL,
  subject_filter = NULL,
  postprocess_streams = NULL,
  extract_streams = NULL,
  debug = FALSE,
  force = FALSE,
  dry_run = FALSE,
  log_level = c("INFO", "DEBUG", "WARN", "ERROR", "TRACE", "FATAL")
)
```

## Arguments

- scfg:

  a project configuration object as produced by `load_project` or
  `setup_project`

- steps:

  Character vector of pipeline stages to execute. Supported stages are
  `"flywheel_sync"`, `"bids_conversion"`, `"mriqc"`, `"fmriprep"`,
  `"aroma"`, `"postprocess"`, and `"extract_rois"`. Use `"all"` to run
  all enabled stages. If `NULL`, the user will be prompted for which
  stages to run. BIDS validation is configured with the project but
  submitted separately through
  [`run_bids_validation()`](https://uncdependlab.github.io/BrainGnomes/reference/run_bids_validation.md);
  it is not a `run_project()` stage.

- subject_filter:

  Optional character vector or data.frame specifying which subjects (and
  optionally sessions) to process. When `NULL` and run interactively,
  the user will be prompted to enter space-separated subject IDs (press
  ENTER to process all subjects). When a data.frame is provided, it must
  contain a `sub_id` column and may include a `ses_id` column to filter
  on specific subject/session combinations.

- postprocess_streams:

  Optional character vector specifying which postprocessing streams
  should run. When `"postprocess"` is included in `steps`, `NULL`
  selects every configured postprocessing stream.

- extract_streams:

  Optional character vector specifying which ROI extraction streams
  should run. When `"extract_rois"` is included in `steps`, `NULL`
  selects every configured extraction stream.

- debug:

  A logical value indicating whether to run in debug mode (verbose
  output for debugging, no true processing).

- force:

  A logical value indicating whether to force the execution of all
  steps, regardless of their current status.

- dry_run:

  A logical value indicating whether to perform a dry run. Dry runs
  validate settings and report subject/session scope plus resolved
  postprocessing and extraction stream settings without submitting any
  jobs.

- log_level:

  Character string controlling log verbosity. One of `TRACE`, `DEBUG`,
  `INFO`, `WARN`, `ERROR`, or `FATAL`.

## Value

A logical value indicating whether the processing pipeline was
successfully run.

## See also

[`run_bids_validation()`](https://uncdependlab.github.io/BrainGnomes/reference/run_bids_validation.md)
to submit the BIDS validation configured with the project.

## Examples

``` r
  if (FALSE) { # \dontrun{
    # Assuming you have a valid project configuration list named `study_config`
    run_project(study_config, steps = "fmriprep", force = FALSE)
  } # }
```
