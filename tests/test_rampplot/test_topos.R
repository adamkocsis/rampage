library(tinytest)
library(rampage)

setwd(wd)

source("rampage/tests/methods/test_plot.R")

dir <- "rampage/tests/results/rampplot/topos/"

dir.create(dir, showWarnings=FALSE)
globalUpdate <- FALSE

# load the topography objects
data(topos)

########################################----------------------------------------
# Havanna-2
# test expansion
expect_silent(ramp <- expand(topos$havanna2, n= 256))
expr <- expression(plot(ramp))

test_plot(expr, path="havanna_256.png", dir=dir, width=500, height=1000, update=(FALSE | globalUpdate))


########################################----------------------------------------
# Jakarta
# test expansion
expect_silent(ramp <- expand(topos$jakarta, n= 256))
expr <- expression(plot(ramp))

test_plot(expr, path="jakarta_256.png", dir=dir, width=500, height=1000, update=(FALSE | globalUpdate))

########################################----------------------------------------
# Tokio1
# test expansion
expect_silent(ramp <- expand(topos$tokio1, n= 256))
expr <- expression(plot(ramp))

test_plot(expr, path="tokio_256.png", dir=dir, width=500, height=1000, update=(FALSE | globalUpdate))

########################################----------------------------------------
# Zagreb
# test expansion
expect_silent(ramp <- expand(topos$zagreb, n= 256))
expr <- expression(plot(ramp))

test_plot(expr, path="zagreb_256.png", dir=dir, width=500, height=1000, update=(FALSE | globalUpdate))



########################################----------------------------------------
# Havanna-2
# test expansion
expect_silent(ramp <- expand(topos$havanna2, n= 50))
expr <- expression(plot(ramp))

test_plot(expr, path="havanna_50.png", dir=dir, width=500, height=1000, update=(FALSE | globalUpdate))


########################################----------------------------------------
# Jakarta
# test expansion
expect_silent(ramp <- expand(topos$jakarta, n= 50))
expr <- expression(plot(ramp))

test_plot(expr, path="jakarta_50.png", dir=dir, width=500, height=1000, update=(FALSE | globalUpdate))

########################################----------------------------------------
# Tokio1
# test expansion
expect_silent(ramp <- expand(topos$tokio1, n= 50))
expr <- expression(plot(ramp))

test_plot(expr, path="tokio_50.png", dir=dir, width=500, height=1000, update=(FALSE | globalUpdate))

########################################----------------------------------------
# Zagreb
# test expansion
expect_silent(ramp <- expand(topos$zagreb, n= 50))
expr <- expression(plot(ramp))

test_plot(expr, path="zagreb_50.png", dir=dir, width=500, height=1000, update=(FALSE | globalUpdate))

################################################################################
# The same with breaks
########################################----------------------------------------
# Havanna-2
# test expansion
expect_silent(ramp <- expand(topos$havanna2, n= 50))
expr <- expression(plot(ramp, breaks=TRUE))

test_plot(expr, path="havanna_50_breaks.png", dir=dir, width=500, height=1000, update=(FALSE | globalUpdate))


########################################----------------------------------------
# Jakarta
# test expansion
expect_silent(ramp <- expand(topos$jakarta, n= 50))
expr <- expression(plot(ramp, breaks=TRUE))

test_plot(expr, path="jakarta_50_breaks.png", dir=dir, width=500, height=1000, update=(FALSE | globalUpdate))

########################################----------------------------------------
# Tokio1
# test expansion
expect_silent(ramp <- expand(topos$tokio1, n= 50))
expr <- expression(plot(ramp, breaks=TRUE))

test_plot(expr, path="tokio_50_breaks.png", dir=dir, width=500, height=1000, update=(FALSE | globalUpdate))

########################################----------------------------------------
# Zagreb
# test expansion
expect_silent(ramp <- expand(topos$zagreb, n= 50))
expr <- expression(plot(ramp, breaks=TRUE))

test_plot(expr, path="zagreb_50_breaks.png", dir=dir, width=500, height=1000, update=(FALSE | globalUpdate))
