# Validate that a brain mask was correctly applied to 4D fMRI data

Replays the masking operation in volume chunks and checks that the
post-mask image equals the pre-mask image inside the mask and is exactly
zero outside it. Voxels inside the mask that remain zero for the full
run are reported.

## Usage

``` r
validate_apply_mask(
  pre_file,
  post_file,
  mask_file,
  tolerance = 1e-05,
  chunk_size = 100L
)
```

## Arguments

- pre_file:

  Path to the 4D fMRI data before masking.

- post_file:

  Path to the 4D fMRI data after masking.

- mask_file:

  Path to the binary mask NIfTI file (1s = brain, 0s = non-brain).

- tolerance:

  Maximum relative numerical error allowed inside the mask.

- chunk_size:

  Number of volumes compared at a time.

## Value

A logical scalar (`TRUE` if validation passed, `FALSE` if failed).
Attributes:

- `message`: Character string describing the validation result.

- `external_violations`: Integer count of voxels outside mask with
  non-zero signal.

- `internal_zeros`: Integer count of voxels inside mask that are all
  zero.

## Details

Validation requires the complete post-mask image to equal the exact
expected transform within `tolerance`. This prevents an all-zero or
otherwise altered in-mask output from passing merely because nothing
leaked outside the mask.
