# load the package itself
library(rampage)
library(tinytest)

# project
# setwd(wd)
setwd("/mnt/sky/Dropbox/Software/rampage/")
source("rampage/tests/methods/test_plot.R")

# simple color points example
set.seed(1)
x <-rnorm(10000, 0, 1)
y <-rnorm(10000, 0, 1)
z <- x + y +rnorm(10000,0,0.25)
# the tested function for now is here.
source("rampage/R/plotting.R")

# to make tests easier
dir <- "rampage/tests/results/colorpoints/gradinv"

# create reference images or not?
globalUpdate <- FALSE


################################################################################
# A. Automatic ramping

expr <- expression({
	# canvas
	plot(x,y, col=NA)

	# color ramping
	expect_silent(colorpoints(x, y, z, legend=NULL))
})

test_plot(expr, path="autoramp.svg", dir=dir, update=(FALSE | globalUpdate))


################################################################################
# B. Manual ramping

# B1. Make the ramp match which we made earlier
col <- gradinv(256)
breaks <- seq(min(z), max(z), length.out=length(col)+1)

# the manually ramped plot
expr <- expression({
	# canvas
	plot(x,y, col=NA)

	# color ramping
	expect_silent(colorpoints(x, y, z, legend=NULL, col=col, breaks=breaks))
})

test_plot(expr, path="autoramp.svg", dir=dir, update=FALSE)

# B2. Missing input / points in the ramp
# make a rampage object
df <- data.frame(
	z=c(c(-2, -1,0, 1, 6)),
	color=rampage::gradinv(5)
)

ex <- rampage::expand(df, n=100)

# the manually ramped plot
expr <- expression({
	# canvas
	plot(x,y, col=NA)

	# color ramping
	expect_warning(colorpoints(x, y, z, legend=NULL, ramp=ex))
})

test_plot(expr, path="autoramp.svg", dir=dir, update=FALSE)

# B3. Symmetric wide
# A couple of reference plots
df <- data.frame(
	z=c(c(-6, -1,0, 1, 6)),
	color=rampage::gradinv(5)
)

ex <- rampage::expand(df, n=100)

# the manually ramped plot
expr <- expression({
	# canvas
	plot(x,y, col=NA)

	# color ramping
	expect_silent(colorpoints(x, y, z, legend=NULL, ramp=ex))
})

test_plot(expr, path="symmetric_wide.svg", dir=dir,  update=(FALSE | globalUpdate))

# B4. Symmetric narrower
# A couple of reference plots
df <- data.frame(
	z=c(c(-6, -0.5,0, 0.5, 6)),
	color=rampage::gradinv(5)
)

ex <- rampage::expand(df, n=100)

# the manually ramped plot
expr <- expression({
	# canvas
	plot(x,y, col=NA)

	# color ramping
	expect_silent(colorpoints(x, y, z, legend=NULL, ramp=ex))
})

test_plot(expr, path="symmetric_narrower.svg", dir=dir,  update=(FALSE | globalUpdate))

# B4. Symmetric very narrow
df <- data.frame(
	z=c(c(-6, -0.25,0, 0.25, 6)),
	color=rampage::gradinv(5)
)

ex <- rampage::expand(df, n=100)

# the manually ramped plot
expr <- expression({
	# canvas
	plot(x,y, col=NA)

	# color ramping
	expect_silent(colorpoints(x, y, z, legend=NULL, ramp=ex))
})

test_plot(expr, path="symmetric_very_narrow.svg", dir=dir,  update=(FALSE | globalUpdate))


# Asymmetric plots
# C1. Asymmetric wide
# A couple of reference plots
df <- data.frame(
	z=c(c(-6, 0, 1, 2, 6)),
	color=rampage::gradinv(5)
)

ex <- rampage::expand(df, n=100)

# the manually ramped plot
expr <- expression({
	# canvas
	plot(x,y, col=NA)

	# color ramping
	expect_silent(colorpoints(x, y, z, legend=NULL, ramp=ex))
})

test_plot(expr, path="asymmetric_wide.svg", dir=dir,  update=(FALSE | globalUpdate))

# C2. Symmetric narrower
# A couple of reference plots
df <- data.frame(
	z=c(c(-6, 0.5,1, 1.5, 6)),
	color=rampage::gradinv(5)
)

ex <- rampage::expand(df, n=100)

# the manually ramped plot
expr <- expression({
	# canvas
	plot(x,y, col=NA)

	# color ramping
	expect_silent(colorpoints(x, y, z, legend=NULL, ramp=ex))
})

test_plot(expr, path="asymmetric_narrower.svg", dir=dir,  update=(FALSE | globalUpdate))

# C3. Symmetric very narrow
df <- data.frame(
	z=c(c(-6, 0.75,1, 1.25, 6)),
	color=rampage::gradinv(5)
)

ex <- rampage::expand(df, n=100)

# the manually ramped plot
expr <- expression({
	# canvas
	plot(x,y, col=NA)

	# color ramping
	expect_silent(colorpoints(x, y, z, legend=NULL, ramp=ex))
})

test_plot(expr, path="asymmetric_very_narrow.svg", dir=dir,  update=(FALSE | globalUpdate))

################################################################################
# Include the legend


################################################################################
# A. Automatic ramping

expr <- expression({
	# canvas
	plot(x,y, col=NA)

	# color ramping
	expect_silent(colorpoints(x, y, z))
})

test_plot(expr, path="autoramp_legend.svg", dir=dir, update=(FALSE | globalUpdate))


################################################################################
# B. Manual ramping

# B1. Make the ramp match which we made earlier
col <- gradinv(256)
breaks <- seq(min(z), max(z), length.out=length(col)+1)

# the manually ramped plot
expr <- expression({
	# canvas
	plot(x,y, col=NA)

	# color ramping
	expect_silent(colorpoints(x, y, z, col=col, breaks=breaks))
})

test_plot(expr, path="autoramp_legend.svg", dir=dir, update=FALSE)

# B2. Missing input / points in the ramp
# make a rampage object
df <- data.frame(
	z=c(c(-2, -1,0, 1, 6)),
	color=rampage::gradinv(5)
)

ex <- rampage::expand(df, n=100)

# the manually ramped plot
expr <- expression({
	# canvas
	plot(x,y, col=NA)

	# color ramping
	expect_warning(colorpoints(x, y, z, ramp=ex))
})

test_plot(expr, path="autoramp_legend.svg", dir=dir, update=FALSE)

# B3. Symmetric wide
# A couple of reference plots
df <- data.frame(
	z=c(c(-6, -1,0, 1, 6)),
	color=rampage::gradinv(5)
)

ex <- rampage::expand(df, n=100)

# the manually ramped plot
expr <- expression({
	# canvas
	plot(x,y, col=NA)

	# color ramping
	expect_silent(colorpoints(x, y, z, ramp=ex))
})

test_plot(expr, path="symmetric_wide_legend.svg", dir=dir,  update=(FALSE | globalUpdate))

# B4. Symmetric narrower
# A couple of reference plots
df <- data.frame(
	z=c(c(-6, -0.5,0, 0.5, 6)),
	color=rampage::gradinv(5)
)

ex <- rampage::expand(df, n=100)

# the manually ramped plot
expr <- expression({
	# canvas
	plot(x,y, col=NA)

	# color ramping
	expect_silent(colorpoints(x, y, z, ramp=ex))
})

test_plot(expr, path="symmetric_narrower_legend.svg", dir=dir,  update=(FALSE | globalUpdate))

# B4. Symmetric very narrow
df <- data.frame(
	z=c(c(-6, -0.25,0, 0.25, 6)),
	color=rampage::gradinv(5)
)

ex <- rampage::expand(df, n=100)

# the manually ramped plot
expr <- expression({
	# canvas
	plot(x,y, col=NA)

	# color ramping
	expect_silent(colorpoints(x, y, z, ramp=ex))
})

test_plot(expr, path="symmetric_very_narrow_legend.svg", dir=dir,  update=(FALSE | globalUpdate))


# Asymmetric plots
# C1. Asymmetric wide
# A couple of reference plots
df <- data.frame(
	z=c(c(-6, 0, 1, 2, 6)),
	color=rampage::gradinv(5)
)

ex <- rampage::expand(df, n=100)

# the manually ramped plot
expr <- expression({
	# canvas
	plot(x,y, col=NA)

	# color ramping
	expect_silent(colorpoints(x, y, z, ramp=ex))
})

test_plot(expr, path="asymmetric_wide_legend.svg", dir=dir,  update=(FALSE | globalUpdate))

# C2. Symmetric narrower
# A couple of reference plots
df <- data.frame(
	z=c(c(-6, 0.5,1, 1.5, 6)),
	color=rampage::gradinv(5)
)

ex <- rampage::expand(df, n=100)

# the manually ramped plot
expr <- expression({
	# canvas
	plot(x,y, col=NA)

	# color ramping
	expect_silent(colorpoints(x, y, z, ramp=ex))
})

test_plot(expr, path="asymmetric_narrower_legend.svg", dir=dir,  update=(FALSE | globalUpdate))

# C3. Symmetric very narrow
df <- data.frame(
	z=c(c(-6, 0.75,1, 1.25, 6)),
	color=rampage::gradinv(5)
)

ex <- rampage::expand(df, n=100)

# the manually ramped plot
expr <- expression({
	# canvas
	plot(x,y, col=NA)

	# color ramping
	expect_silent(colorpoints(x, y, z, ramp=ex))
})

test_plot(expr, path="asymmetric_very_narrow_legend.svg", dir=dir,  update=(FALSE | globalUpdate))
