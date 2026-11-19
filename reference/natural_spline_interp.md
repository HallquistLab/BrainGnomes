# Cubic spline interpolation with natural spline and linear extrapolation

Performs natural cubic spline interpolation for given input values. This
function takes known data points `(x, y)` and evaluates the cubic spline
interpolation at specified output points `xout`. It uses a natural
spline formulation with zero second derivatives at the endpoints.

## Usage

``` r
natural_spline_interp(x, y, xout)
```

## Arguments

- x:

  A numeric vector of strictly increasing x-values (time or position).

- y:

  A numeric vector of y-values at each x (same length as x).

- xout:

  A finite numeric vector of points at which to interpolate.

## Value

A numeric vector of interpolated y-values at each point in `xout`.

## Details

The function performs cubic spline interpolation using a tridiagonal
system to solve for the spline coefficients. If `xout` values fall
outside the `x` range, the function uses linear extrapolation, mirror
R's `splinefun` with `method = "natural"`

## Examples

``` r
  x <- c(0, 1, 2, 3, 4)
  y <- c(0, 1, 0, 1, 0)
  xout <- seq(0, 4, by = 0.1)
  natural_spline_interp(x, y, xout)
#>  [1]  0.000000e+00  1.707143e-01  3.371429e-01  4.950000e-01  6.400000e-01
#>  [6]  7.678571e-01  8.742857e-01  9.550000e-01  1.005714e+00  1.022143e+00
#> [11]  1.000000e+00  9.372857e-01  8.411429e-01  7.210000e-01  5.862857e-01
#> [16]  4.464286e-01  3.108571e-01  1.890000e-01  9.028571e-02  2.414286e-02
#> [21] -2.220446e-16  2.414286e-02  9.028571e-02  1.890000e-01  3.108571e-01
#> [26]  4.464286e-01  5.862857e-01  7.210000e-01  8.411429e-01  9.372857e-01
#> [31]  1.000000e+00  1.022143e+00  1.005714e+00  9.550000e-01  8.742857e-01
#> [36]  7.678571e-01  6.400000e-01  4.950000e-01  3.371429e-01  1.707143e-01
#> [41]  0.000000e+00
```
