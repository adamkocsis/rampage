# Bathymetric color palettes with tiepoints

The object contains `data.frame`-class objects to be used with the
[`expand`](https://adamtkocsis.com/rampage/reference/expand.md) function
to produce full calibrated color ramps.

## Usage

``` r
data(bathymetry)
```

## Format

A `list` with 1 `data.frame` elements:

- `sandy`:

  : A color ramp resembling a sandy beach.

## Examples

``` r
data(bathymetry)
sandyExp <- expand(bathymetry$sandy, n=200)
plot(sandyExp)
```
