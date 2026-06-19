# Topographic color palettes with tiepoints

The object contains `data.frame`-class objects to be used with the
[`expand`](https://adamtkocsis.com/rampage/reference/expand.md) function
to produce full calibrated color ramps.

## Usage

``` r
data(topos)
```

## Format

A `list` with 7 `data.frame` elements:

- `demcmap`:

  : The "demcmap" theme, based on MatLab's `demcmap`.

- `etopo`:

  : The "etopo" theme, approximate elevation-color assignments based on
  the the ETOPO Global Relief Model poster
  (https://www.ncei.noaa.gov/media/3340).

- `jakarta`:

  : The "Jakarta" theme, color values by Deviantart user
  *Arcanographia*.

- `havanna2`:

  : The "Havanna-2" theme, color values by Deviantart user
  *Arcanographia*.

- `ptolemy`:

  : The "Ptolemy" theme, color values extracted from the 'The world map
  after Ptolemy's first projection from a Greek manuscript edition of
  the Geography' (Burney MS 111).

- `tokio1`:

  : The "Tokio-1" theme, color values by Deviantart user
  *Arcanographia*.

- `zagreb`:

  : The "Zagreb" theme, color values by Deviantart user *Arcanographia*.

## Examples

``` r
data(topos)
jakExp <- expand(topos$jakarta, n=200)
plot(jakExp)
```
