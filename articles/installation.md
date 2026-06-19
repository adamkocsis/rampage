# 1. Installation

## Stable version from the CRAN

The stable version of the package is available on [CRAN
servers](https://cran.r-project.org/package=rampage), which means that
you can install it with the regular
[`install.packages()`](https://rdrr.io/r/utils/install.packages.html)
function.

``` r
install.packages("rampage")
```

If for some reason, the installlation from CRAN fails, you might need to
use an alternative method, such as
`devtools::install_github("adamkocsis/rampage@main")`

## Stable version from Zenodo (source)

For the sake of completeness and simplicity, the sources of the stable
version are also available on GitHub. The
[`install.packages()`](https://rdrr.io/r/utils/install.packages.html)
function can be used to install this from source - but you will have to
install dependencies of the package.

``` r
install.packages(
    "https://zenodo.org/records/20762149/files/rampage_0.2.1.tar.gz?download=1", 
    repos=NULL, type="source")
```

## Development version

If you encounter technical problems with the package that are not
described anywhere, you might want to take a look at the [development
version](https://github.com/adamkocsis/rampage/tree/devel).

If you wish to install the development version, I recommend a manual
installation:  
1. Clone the repository to your local hard drive.  
2. Open a terminal and navigate to the directory where you cloned. The
`rampage` directory should be visible from there.  
3. Run this line in the terminal

    R CMD INSTALL rampage

- *If you see an error suggesting that `R` is not found, you have to add
  it to your `PATH` environmental variable.*  
- *If the R packages that `rampage` depend on are not installed, you
  have to install them manually, or you will get an error.*

If for some reason, the installlation from CRAN fails, you might need to
use an alternative method, such as
`devtools::install_github("adamkocsis/rampage@devel")`
