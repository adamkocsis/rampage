# Testing script for the limit() function
library(rampage)
library(terra)
library(tinytest)

# generate a raster with random data
r <- terra::rast()
terra::values(r) <- rnorm(terra::ncell(r), 0, 1)

# A1. limit from above (reached) - proper limit
maxVal <- 1
expect_silent(r2 <- limit(r, max=maxVal))
expect_equivalent(class(r2), "SpatRaster")

# the maximum value is the given
expect_equal(max(values(r2)), maxVal)

# every other value is the same
iDiff <- which(values(r)< maxVal)
expect_equal(
	values(r)[iDiff],
	values(r2)[iDiff],
)

# A2. limit from above (not reached) - no change
maxVal <- 100
expect_silent(r2 <- limit(r, max=maxVal))
expect_equivalent(class(r2), "SpatRaster")

# THe values should be the same
expect_equal(r, r2)

# A3. limit beyond capacity - overlimitting
maxVal <- -100
expect_silent(r2 <- limit(r, max=maxVal))
expect_equivalent(class(r2), "SpatRaster")

# all values should be the same
r3 <- r
values(r3) <- maxVal
expect_equal(r3, r2)

################################################################################
#
# B1. limit from below (reached) - proper limit
minVal <- -1
expect_silent(r2 <- limit(r, min=minVal))
expect_equivalent(class(r2), "SpatRaster")

# the minimum value is the given
expect_equal(min(values(r2)), minVal)

# every other value is the same
iDiff <- which(values(r)> minVal)
expect_equal(
	values(r)[iDiff],
	values(r2)[iDiff],
)

# B2. limit from above (not reached) - no change
minVal <- -100
expect_silent(r2 <- limit(r, min=minVal))
expect_equivalent(class(r2), "SpatRaster")

# THe values should be the same
expect_equal(r, r2)

# B3. limit beyond capacity - overlimitting
minVal <- 100
expect_silent(r2 <- limit(r, min=minVal))
expect_equivalent(class(r2), "SpatRaster")

# all values should be the same
r3 <- r
values(r3) <- minVal
expect_equal(r3, r2)

################################################################################
# C. Both arguments given, implementation independent
# If one case works, all should.
minVal <- -1
maxVal <- 1
expect_silent(r2 <- limit(r, min=minVal, max=maxVal))
expect_equivalent(class(r2), "SpatRaster")

# the minimum value is the given
expect_equal(min(values(r2)), minVal)
expect_equal(max(values(r2)), maxVal)

# every other value is the same
iDiff <- which(values(r)> minVal & values(r)< maxVal)
expect_equal(
	values(r)[iDiff],
	values(r2)[iDiff]
)

################################################################################
# D. Both arguments given a single argument implementation independent
# If one case works, all should.
minVal <- -1
maxVal <- 1
expect_silent(r2 <- limit(r, y=c(minVal, maxVal)))
expect_equivalent(class(r2), "SpatRaster")

# the minimum value is the given
expect_equal(min(values(r2)), minVal)
expect_equal(max(values(r2)), maxVal)

# every other value is the same
iDiff <- which(values(r)> minVal & values(r)< maxVal)
expect_equal(
	values(r)[iDiff],
	values(r2)[iDiff]
)

################################################################################
# Wrong cases
minVal <- NA # non-finite
expect_error(limit(r, min=minVal))
minVal <- "a" # non-numeric
expect_error(limit(r, min=minVal))
minVal <- c(1,2) # multiple
expect_error(limit(r, min=minVal))

maxVal <- NA # non-finite
expect_error(limit(r, max=maxVal))
maxVal <- "a" # non-numeric
expect_error(limit(r, max=maxVal))
maxVal <- c(1,2) # multiple
expect_error(limit(r, max=maxVal))

################################################################################
# II. Raster including missing values

# generate a raster with random data
r <- terra::rast()
terra::values(r) <- rnorm(terra::ncell(r), 0, 1)
terra::values(r)[sample(1:ncell(r), 10000)]  <- NA

# A1. limit from above (reached) - proper limit
maxVal <- 1
expect_silent(r2 <- limit(r, max=maxVal))
expect_equivalent(class(r2), "SpatRaster")

# the maximum value is the given
expect_equal(max(values(r2), na.rm=TRUE), maxVal)

# every other value is the same
iDiff <- which(values(r)< maxVal)
expect_equal(
	values(r)[iDiff],
	values(r2)[iDiff],
)

# A2. limit from above (not reached) - no change
maxVal <- 100
expect_silent(r2 <- limit(r, max=maxVal))
expect_equivalent(class(r2), "SpatRaster")

# THe values should be the same
expect_equal(r, r2)

# A3. limit beyond capacity - overlimitting
maxVal <- -100
expect_silent(r2 <- limit(r, max=maxVal))
expect_equivalent(class(r2), "SpatRaster")

# all values should be the same
r3 <- r
values(r3) <- maxVal
values(r3)[is.na(values(r))] <- NA
expect_equal(r3, r2)
expect_equal(values(r3), values(r2))

################################################################################
#
# B1. limit from below (reached) - proper limit
minVal <- -1
expect_silent(r2 <- limit(r, min=minVal))
expect_equivalent(class(r2), "SpatRaster")

# the minimum value is the given
expect_equal(min(values(r2), na.rm=TRUE), minVal)

# every other value is the same
iDiff <- which(values(r)> minVal)
expect_equal(
	values(r)[iDiff],
	values(r2)[iDiff],
)

# B2. limit from above (not reached) - no change
minVal <- -100
expect_silent(r2 <- limit(r, min=minVal))
expect_equivalent(class(r2), "SpatRaster")

# THe values should be the same
expect_equal(r, r2)

# B3. limit beyond capacity - overlimitting
minVal <- 100
expect_silent(r2 <- limit(r, min=minVal))
expect_equivalent(class(r2), "SpatRaster")

# all values should be the same
r3 <- r
values(r3) <- minVal
values(r3)[is.na(values(r))] <- NA
expect_equal(r3, r2)
expect_equal(values(r3), values(r2))

################################################################################
# C. Both arguments given, implementation independent
# If one case works, all should.
minVal <- -1
maxVal <- 1
expect_silent(r2 <- limit(r, min=minVal, max=maxVal))
expect_equivalent(class(r2), "SpatRaster")

# the minimum value is the given
expect_equal(min(values(r2), na.rm=TRUE), minVal)
expect_equal(max(values(r2), na.rm=TRUE), maxVal)

# every other value is the same
iDiff <- which(values(r)> minVal & values(r)< maxVal)
expect_equal(
	values(r)[iDiff],
	values(r2)[iDiff]
)

################################################################################
# D. Both arguments given a single argument implementation independent
# If one case works, all should.
minVal <- -1
maxVal <- 1
expect_silent(r2 <- limit(r, y=c(minVal, maxVal)))
expect_equivalent(class(r2), "SpatRaster")

# the minimum value is the given
expect_equal(min(values(r2), na.rm=TRUE), minVal)
expect_equal(max(values(r2), na.rm=TRUE), maxVal)

# every other value is the same
iDiff <- which(values(r)> minVal & values(r)< maxVal)
expect_equal(
	values(r)[iDiff],
	values(r2)[iDiff]
)
