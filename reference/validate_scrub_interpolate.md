# Validate scrub interpolation by exact preservation and sampled spline replay

The pre/post images must have the same shape, uncensored volumes must be
unchanged, and interpolated values must match the production
natural-spline implementation at deterministic, spatially distributed
voxels.

## Usage

``` r
validate_scrub_interpolate(
  pre_file,
  post_file,
  censor_file,
  n_sample = 100L,
  tolerance = 1e-05,
  chunk_size = 100L
)
```

## Arguments

- pre_file:

  Path to 4D BOLD before `scrub_interpolate`.

- post_file:

  Path to 4D BOLD after `scrub_interpolate`.

- censor_file:

  Censor file (1 = keep, 0 = interpolate).

- n_sample:

  Number of pre-step brain/data voxels used for spline replay.

- tolerance:

  Maximum relative numerical error for exact comparisons.

- chunk_size:

  Number of volumes compared at a time.

## Value

A logical scalar (`TRUE` if validation passed, `FALSE` if failed).
Attributes: `message`, `details`.
