# Trimming a calibrated color ramp object.

Modify the minimum and maximum values in a `calibramp`-class object
produced by the
[`expand`](https://adamtkocsis.com/rampage/reference/expand.md)
function.

## Usage

``` r
trimramp(x, low = NULL, high = NULL)
```

## Arguments

- x:

  A calibrated color ramp (e.g. `calibramp`-class object.

- low:

  A single `numeric` value, the minimum value in the calibrated ramp.

- high:

  A single `numeric` value, the maximum value in the calibrated ramp.

## Value

A trimmed version of `x`, another `calibramp`-class object.

## Examples

``` r
data(paleomap)
trimmed <- trimramp(paleomap, low=-500, high=1500)
plot(trimmed)
```
