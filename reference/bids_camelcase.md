# Convert a string to BIDS-compatible camelCase

Treats periods, hyphens, and underscores as word boundaries, removes
them, and capitalizes the following letter. E.g., "task-ridl_name"
becomes "taskRidlName", and "cor.shrink" becomes "corShrink".

## Usage

``` r
bids_camelcase(x)
```

## Arguments

- x:

  A character string.

## Value

A character string in camelCase form.

## Examples

``` r
if (FALSE) { # \dontrun{
  bids_camelcase("task-ridl_name")
  bids_camelcase("echo_time-series")
  bids_camelcase("cor.shrink")
  bids_camelcase("space-mni152nlin2009casym")
} # }
```
