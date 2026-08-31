# Validate scrub timepoints by exact retained-volume replay

The post image must contain exactly the pre-image volumes whose censor
values are one, in their original order. Pass the censor vector read
**before** the step if the file is overwritten.

## Usage

``` r
validate_scrub_timepoints(
  pre_file,
  post_file,
  censor_vec,
  tolerance = 1e-05,
  chunk_size = 100L
)
```

## Arguments

- pre_file:

  Path to 4D BOLD before `scrub_timepoints`.

- post_file:

  Path to 4D BOLD after `scrub_timepoints`.

- censor_vec:

  Censor vector length = pre TRs (1 = keep, 0 = drop).

- tolerance:

  Maximum relative numerical error for retained volumes.

- chunk_size:

  Number of retained volumes compared at a time.

## Value

A logical scalar (`TRUE` if validation passed, `FALSE` if failed).
Attributes: `message`, `details` (`n_pre_t`, `n_post_t`, `n_removed`).
