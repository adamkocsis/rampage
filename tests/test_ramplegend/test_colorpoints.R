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
# source("rampage/R/plotting.R")

# to make tests easier
dir <- "rampage/tests/results/ramplegend/colpoints"

globalUpdate <- FALSE

################################################################################
# Topleft position

# Topleft cex=1
# write the expression of the plot
expr <- expression({
	# canvas
	plot(x,y, col=NA)
	# actual plotting
	colorpoints(x,y, z=z, breaks=ex$breaks, col=ex$col, pch=16, legend=NULL)

	# standard legend size
	ramplegend(x="topleft", col=ex$col, breaks=ex$breaks, zlim=NULL, height=3,
		width=0.3,tick.length=0.15,cex=1, at=c(-5, 0, 5))

})

test_plot(expr, path="topleft_cex1.svg", dir=dir, update=(FALSE | globalUpdate))
test_plot(expr, path="topleft_cex1.png", dir=dir, width=1000, height=1000, update=(FALSE | globalUpdate))

# Topleft cex=0.7
# write the expression of the plot
expr <- expression({
	# canvas
	plot(x,y, col=NA)
	# actual plotting
	colorpoints(x,y, z=z, breaks=ex$breaks, col=ex$col, pch=16, legend=NULL)

	# standard legend size
	ramplegend(x="topleft", col=ex$col, breaks=ex$breaks, zlim=NULL, height=3,
		width=0.3,tick.length=0.15,cex=0.7,at=c(-5, 0, 5))

})

test_plot(expr, path="topleft_cex0.7.svg", dir=dir, update=(FALSE | globalUpdate))
test_plot(expr, path="topleft_cex0.7.png", dir=dir, width=1000, height=1000, update=(FALSE | globalUpdate))

# Topleft cex=0.5
# write the expression of the plot
expr <- expression({
	# canvas
	plot(x,y, col=NA)
	# actual plotting
	colorpoints(x,y, z=z, breaks=ex$breaks, col=ex$col, pch=16, legend=NULL)

	# standard legend size
	ramplegend(x="topleft", col=ex$col, breaks=ex$breaks, zlim=NULL, height=3,
		width=0.3,tick.length=0.15,cex=0.5, at=c(-5, 0, 5))

})

test_plot(expr, path="topleft_cex0.5.svg", dir=dir, update=(FALSE | globalUpdate))
test_plot(expr, path="topleft_cex0.5.png", dir=dir, width=1000, height=1000, update=(FALSE | globalUpdate))

################################################################################
# explicitly given x and y

# x=-3, y=2.5 cex=1
# write the expression of the plot
expr <- expression({
	# canvas
	plot(x,y, col=NA)
	# actual plotting
	colorpoints(x,y, z=z, breaks=ex$breaks, col=ex$col, pch=16, legend=NULL)

	# standard legend size
	ramplegend(x=-3, y=2.5, col=ex$col, breaks=ex$breaks, zlim=NULL, height=3,
		width=0.3,tick.length=0.15,cex=1, at=c(-5, 0, 5))

	abline(h=2.5)
	abline(v=-3)
})

test_plot(expr, path="x-3_y2.5_cex1.svg", dir=dir, update=(FALSE | globalUpdate))
test_plot(expr, path="x-3_y2.5_cex1.png", dir=dir, width=1000, height=1000, update=(FALSE | globalUpdate))

# x=-3, y=2.5 cex=0.5
# write the expression of the plot
expr <- expression({
	# canvas
	plot(x,y, col=NA)
	# actual plotting
	colorpoints(x,y, z=z, breaks=ex$breaks, col=ex$col, pch=16, legend=NULL)

	# standard legend size
	ramplegend(x=-3, y=2.5, col=ex$col, breaks=ex$breaks, zlim=NULL, height=3,
		width=0.3,tick.length=0.15,cex=0.5, at=c(-5, 0, 5))

	abline(h=2.5)
	abline(v=-3)
})

test_plot(expr, path="x-3_y2.5_cex0.5.svg", dir=dir, update=(FALSE | globalUpdate))
test_plot(expr, path="x-3_y2.5_cex0.5.png", dir=dir, width=1000, height=1000, update=(FALSE | globalUpdate))
