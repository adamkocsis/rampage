# Tests for the appropriate expansion of the built-in topographic tiepoint objects
suppressPackageStartupMessages(library(terra))
suppressPackageStartupMessages(library(chronosphere))
library(tinytest)
library(rampage)
library(via)

setwd(wd)

dems <- chronosphere::fetch("paleomap", "dem", ver="20180801",
	datadir="/mnt/sky/Dropbox/Software/rampage/data/chronosphere/", verbose=FALSE)


# get the K-Pg
dem <- dems["65"]

# resample to avoid warnings
dem <- terra::resample(dem, terra::rast())

# load image testing functions
source("rampage/tests/methods/test_plot.R")

# Save test images here
dir <- "rampage/tests/results/expand/paleodems/"
dir.create(dir, showWarnings=FALSE)

globalUpdate <- FALSE

# load the topography objects
data(topos)

########################################----------------------------------------
# Havanna-2
# test expansion
expect_silent(ramp <- expand(topos$havanna2, n= 256))

expr <- expression({
	# canvas
	plot(dem, col=ramp$col, breaks=ramp$breaks, legend=FALSE)
})

test_plot(expr, path="65_havanna.png", dir=dir, width=2000, height=1000, update=(FALSE | globalUpdate))



########################################----------------------------------------
# Jakarta
# test expansion
expect_silent(ramp <- expand(topos$jakarta, n= 256))

expr <- expression({
	# canvas
	plot(dem, col=ramp$col, breaks=ramp$breaks, legend=FALSE)
})

test_plot(expr, path="65_jakarta.png", dir=dir, width=2000, height=1000, update=(FALSE | globalUpdate))

########################################----------------------------------------
# Tokio1
# test expansion
expect_silent(ramp <- expand(topos$tokio1, n= 256))

expr <- expression({
	# canvas
	plot(dem, col=ramp$col, breaks=ramp$breaks, legend=FALSE)
})

test_plot(expr, path="65_tokio.png", dir=dir, width=2000, height=1000, update=(FALSE | globalUpdate))

########################################----------------------------------------
# Zagreb
# test expansion
expect_silent(ramp <- expand(topos$zagreb, n= 256))

expr <- expression({
	# canvas
	plot(dem, col=ramp$col, breaks=ramp$breaks, legend=FALSE)
})

test_plot(expr, path="65_zagreb.png", dir=dir, width=2000, height=1000, update=(FALSE | globalUpdate))

################################################################################
# Limited versions

########################################----------------------------------------
# Havanna-2
# test expansion
expect_silent(ramp <- expand(topos$havanna2, n= 30))

expr <- expression({
	# canvas
	plot(dem, col=ramp$col, breaks=ramp$breaks, legend=FALSE)
})

test_plot(expr, path="65_havanna_limited.png", dir=dir, width=2000, height=1000, update=(FALSE | globalUpdate))


########################################----------------------------------------
# Jakarta
# test expansion
expect_silent(ramp <- expand(topos$jakarta, n= 30))

expr <- expression({
	# canvas
	plot(dem, col=ramp$col, breaks=ramp$breaks, legend=FALSE)
})

test_plot(expr, path="65_jakarta_limited.png", dir=dir, width=2000, height=1000, update=(FALSE | globalUpdate))

########################################----------------------------------------
# Tokio1
# test expansion
expect_silent(ramp <- expand(topos$tokio1, n= 30))

expr <- expression({
	# canvas
	plot(dem, col=ramp$col, breaks=ramp$breaks, legend=FALSE)
})

test_plot(expr, path="65_tokio_limited.png", dir=dir, width=2000, height=1000, update=(FALSE | globalUpdate))

########################################----------------------------------------
# Zagreb
# test expansion
expect_silent(ramp <- expand(topos$zagreb, n= 30))

expr <- expression({
	# canvas
	plot(dem, col=ramp$col, breaks=ramp$breaks, legend=FALSE)
})

test_plot(expr, path="65_zagreb_limited.png", dir=dir, width=2000, height=1000, update=(FALSE | globalUpdate))
