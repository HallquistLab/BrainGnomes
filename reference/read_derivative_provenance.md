# Read the portable record associated with a derivative

Reads the JSON sidecar without consulting a project database. The
recorded derivative checksum must match the file; an unsupported schema
or stale sidecar is an error. Original source paths need not be
accessible.

## Usage

``` r
read_derivative_provenance(file)
```

## Arguments

- file:

  Path to a native postprocessed NIfTI or ROI TSV derivative.

## Value

A list containing standard metadata and the versioned `BrainGnomes`
extension, including methods text, source identities, and execution
records.
