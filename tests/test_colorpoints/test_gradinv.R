library(rampage)

# project
setwd("/mnt/sky/Dropbox/Software/rampage/")
source("rampage/tests/methods/test_plot.R")

# simple color points example
set.seed(1)
x <-rnorm(10000, 0, 1)
y <-rnorm(10000, 0, 1)
z <- x + y +rnorm(10000,0,0.25)

# make a rampage object
df <- data.frame(
	z=c(c(-6, -1,0, 1, 6)),
	color=gradinv(5)
)

ex <- expand(df, n=100)

# the tested function for now is here.
source("rampage/R/plotting.R")

# to make tests easier
dir <- "rampage/tests/results/colorpoints/gradinv"


globalUpdate <- FALSE
