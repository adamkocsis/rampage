
# rampage<img src="man/figures/logo.png" align="right" />

[![](https://img.shields.io/badge/devel%20version-0.2.0-green.svg)](https://github.com/adamkocsis/rampage)
[![](https://www.r-pkg.org/badges/version/rampage?color=blue)](https://cran.r-project.org/package=rampage)
[![](http://cranlogs.r-pkg.org/badges/grand-total/rampage?color=yellow)](https://cran.r-project.org/package=rampage)
[![](https://img.shields.io/badge/doi-10.5281/zenodo.10546420.-blue.svg)](https://doi.org/10.5281/zenodo.10546420.)

## Stretchable Color Ramps

This package was written out of sheer anger. Many visulization tasks
involve plotting heatmaps, where colors represent numeric values. There
is a high number of extensions that have pre-defined components to make
such representations with color palettes, but having precise control
over the exact relationship between colors and values seems to be a
constant technical nuissance. The `rampage` extension aims to make this
control easier and allow users to construct and use color ramps based on
explicit relationships between colors and values.

## Example 1: Topographies

Consider this coarsened version of the [ETOPO topographic relief
model](https://www.ncei.noaa.gov/products/etopo-global-relief-model)
(0.25x0.25 degree resolution of the cell-registerd version of ETOPO1),
using the widely-used extension package `terra`.

``` r
library(rampage)
library(terra)

# load data
etopo <- rast("https://adamtkocsis.com/rampage/ETOPO1_ice_c_20110606_tiff_1.tif")

# use a built-in dataset to get color to elevation bindings
data(topos)

# the levels
levs<- topos$etopo

# expand and plot
ramp <- expand(levs, n=500)

# plotting
plot(etopo, col=ramp$col, breaks=ramp$breaks, legend=FALSE)
```

![](man/figures/etopo.png)

## Example 2: Heatmaps

The visual message of a heatmap-figure is highly influenced by the
dominance of specific colors in the heatmap. For instance, let’s
consider these example data that come form a slightly shifted (non-zero
mean) Gaussian distribution:

``` r
library(rampage)

set.seed(1)
# random values in a matrix
vals<- matrix(rnorm(100, mean=-0.7, sd=0.5), ncol=10)

# the histogram of the values
hist(vals, breaks=20)
abline(v=mean(vals), col="red")
```

![](man/figures/hist.png)

This small set of data is wrapped in a matrix to give the values some
positional context, similar to spatial data. When these data are
visualized, the automatic ramping will use the range of values provided
for the plotting function: for instance, the default plotting with the
`fields` package:

``` r
library(fields)
imagePlot(vals, col=rev(gradinv(100)))
```

![](man/figures/fields_default.png)

This solution is fine, if the goal of the heatmap is to visualize where
the values are relative to the overall distribution, but not not where
they are compared to some other value in the same dimension.

For example, assuming that the values represent changes from a previous
state, it might be important to highlight the `0` level, clearly
separating increases (positive) from decreases (negative values), for
which we can use the yellow color. This task can be solved by defining a
calbirated color ramp, that can be constructed with some value (`z`) to
color (`color`) tiepoints in a `data.frame`:

``` r
df <- data.frame(
  z=c(-2, -0.5, 0, 0.5, 2),
  color=rev(gradinv(5))
)
df
```

    ##      z   color
    ## 1 -2.0 #690720
    ## 2 -0.5 #E22C28
    ## 3  0.0 #FFF99A
    ## 4  0.5 #76ACCE
    ## 5  2.0 #33358A

This `data.frame` can than be expanded to a full, calibrated color ramp
with the `expand` function. The resulting object can be used to control
`fields::imagePlot` via its `breaks` argument:

``` r
# calibrated color ramp
ramp <- expand(df, n=100) # check: str(ramp)

# The modified color ramp 
imagePlot(vals, breaks=ramp$breaks, col=ramp$col)
```

![](man/figures/fields_ramped.png)

This second figure shows the same data, but the red color now
consistently indicates decreases over the visualized field.

## Connecting to additional R extensions

Using `fields` is just a single example. Many R packages rely on a
similar `breaks` and `col` argument pair to explicitly control heatmap
levels. These include (to mention a few), the `terra` and `sf` packages.
