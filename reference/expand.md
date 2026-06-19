# Create a calibrated color ramp object from color-tiepoint data.frames

Create ramp palettes from fixed color positions.

## Usage

``` r
expand(x = NULL, n, color = "color", z = "z", ...)
```

## Arguments

- x:

  A `data.frame` object with two columns: `color` for hexadecimal color
  values, `z` for their position.

- n:

  A single `integer` number.

- color:

  A `chraracter` value, the column name of the colors in `x`, defaults
  to `"color"`. Alternatively, a `character` vector of hexadecimal color
  values, with the same length as `z`.

- z:

  A `character` value, the column name of the values in `x`, defaults to
  `"z"`. Alternatively, a `numeric` vector of color values (must have
  the same length as `color`.

- ...:

  Arguments passed to the `colorRampPalette` function.

## Value

A `calibramp`-class object (see description above).

## Details

The function creates objects of the S3 class `calibramp`. The
`calibramp`-class lists have three elements: `col` hexadecimal color
values, `mid`: z-values of midpoints (one for every color), and
`breaks`: separator borders between color values. Color interpolation
will be executed linearly, using
[`colorRampPalette`](https://rdrr.io/r/grDevices/colorRamp.html), the
order of the values will be forced to ascending, the values in `mid`
will be halfway between breaks.

## Examples

``` r
library(rampage)
data(topos)
ramp <- expand(topos$havanna2, n=200)
plot(ramp)

```
