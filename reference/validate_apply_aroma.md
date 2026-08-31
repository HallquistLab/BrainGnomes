# Validate AROMA by deterministic spatial replay

Selects approximately 100 pre-step voxels across normalized image space
and replays the AROMA regression via `lmfit_residuals_mat`; passes if
the maximum absolute difference is below 0.05. If there are no noise
ICs, validation instead requires the complete post-step image to be
unchanged.

## Usage

``` r
validate_apply_aroma(
  pre_file,
  post_file,
  mixing_file,
  noise_ics,
  nonaggressive = TRUE,
  n_sample = 100L,
  mask_file = NULL
)
```

## Arguments

- pre_file:

  Path to 4D BOLD before `apply_aroma`.

- post_file:

  Path to 4D BOLD after `apply_aroma`.

- mixing_file:

  MELODIC mixing matrix (no header).

- noise_ics:

  Noise IC indices (1-based), same as pipeline.

- nonaggressive:

  Same as `apply_aroma`.

- n_sample:

  Number of voxels to sample (default 100).

- mask_file:

  Optional 3D brain mask used to constrain pre-step sampling.

## Value

A logical scalar (`TRUE` if validation passed, `FALSE` if failed).
Attributes: `message`, `details`.
