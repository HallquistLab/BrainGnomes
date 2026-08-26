# Capture output manifest for a completed step

Creates a JSON manifest containing output file paths, sizes, and
modification times. By default all files beneath `output_dir` are
scanned; `files` can instead identify an exact job-specific subset. The
manifest can be stored in the job tracking database and later used to
verify that outputs remain intact.

## Usage

``` r
capture_output_manifest(
  output_dir,
  recursive = TRUE,
  pattern = NULL,
  files = NULL
)
```

## Arguments

- output_dir:

  Root directory used to locate and verify output files.

- recursive:

  Scan subdirectories (default TRUE)

- pattern:

  Optional regex pattern to filter files.

- files:

  Optional character vector of exact output files to record. When
  supplied, only these files are included and they must exist beneath
  `output_dir`. This is useful when several jobs share an output
  directory.

## Value

JSON string containing the manifest, or NULL if directory doesn't exist
