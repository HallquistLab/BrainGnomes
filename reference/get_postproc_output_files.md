# List postprocessed output files for paired input specifications

Converts postprocess input specifications into patterns that target
postprocessed outputs by ensuring each `desc` entity matches its
corresponding `bids_desc`.

## Usage

``` r
get_postproc_output_files(input_dir, input_regex, bids_desc)
```

## Arguments

- input_dir:

  Directory containing postprocessed outputs.

- input_regex:

  One or more specifications used to match the input files. Each may be
  a space-separated set of BIDS entities or a regex prefixed with
  `"regex:"`.

- bids_desc:

  One `desc` value to apply to every `input_regex`, or a vector with the
  same length as `input_regex`. Equal-length vectors are paired by
  position; they are not combined as a cross-product.

## Value

A character vector of unique full paths to matching postprocessed
outputs.

## Details

A scalar `bids_desc` is recycled for backward compatibility. Any other
length mismatch is rejected because it would make the association
between an input specification and its postprocessing stream ambiguous.
