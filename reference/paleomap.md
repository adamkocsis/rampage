# Topographic gradient map color map of the PALEOMAP project

The elevation to color bindings are the work of C. Scotese. Data from
Scotese, C. R., Vérard, C., Burgener, L., Elling, R. P., & Kocsis, A. T.
(2025). The Cretaceous World: Plate Tectonics, Paleogeography, and
Paleoclimate. Geological Society, London, Special Publications, 544(1),
SP544–2024..

## Usage

``` r
data(paleomap)
```

## Format

A `calibramp`-class `list` with 3 `numeric`s:

- `col`:

  : The color levels as hexadecimal RGB values.

- `breaks`:

  : The boundaries for the individual levels.

- `mid`:

  : The mid values of the color levels.

## Source

<https://zenodo.org/records/10659112>
