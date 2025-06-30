# test following the one based on individual values

library(rampage)
suppressPackageStartupMessages(library(terra))
library(tinytest)

# generate a raster with random data
r <- terra::rast()
terra::values(r) <- rnorm(terra::ncell(r), 0, 1)

# create a data.frame
df <- data.frame(
  z=c(-1, -0.5, 0, 0.5, 1),
  color=rev(gradinv(5))
)

# limitation with the data.frame
expect_silent(r2 <- limit(r, df))

minVal <- min(df$z)
maxVal <- max(df$z)

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


# Same test with the ramp
# and expand
ramp <- expand(df, n=100) # check: str(ramp)

expect_silent(r2 <- limit(r, ramp))

minVal <- min(df$z)
maxVal <- max(df$z)

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


# has no z column
df2 <- df
colnames(df2) <- c("wrong", "bullshit")

# z is no t numeric
expect_error(limit(r, df2))
