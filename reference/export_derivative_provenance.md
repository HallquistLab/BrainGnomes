# Export a derivative with its portable processing record

Copies a derivative, its JSON sidecar, methods text, bibliography, and
saved upstream assets to a new directory. Reading the copy does not
require SQLite or access to the original study. The export is a portable
bundle, not a claim that all inputs or software environments have been
archived.

## Usage

``` r
export_derivative_provenance(file, output_dir, datalad = NULL)
```

## Arguments

- file:

  Path to a derivative with BrainGnomes provenance.

- output_dir:

  New directory for the exported bundle. It must not exist.

- datalad:

  Optional named list of externally supplied `dataset_id`, `commit`,
  and/or `annex_key` identifiers. These are recorded as user-supplied
  references; no DataLad command is executed or identifier verified.

## Value

Paths to the exported derivative, sidecar, methods, bibliography, and
dataset description, invisibly.
