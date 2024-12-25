# Test scripts for the appropriate trimming of an expanded ramp object.
library(tinytest)
library(rampage)
data(paleomap, package="rampage")

################################################################################

# 1. Trimming within the bounds
# bottom trimming
trimval <- -4000
expect_silent(botrim <- trimramp(paleomap, low=trimval))
expect_true(all(botrim$breaks>=trimval))
expect_true(all(botrim$mid>trimval))
expect_equal(trimval, min(botrim$breaks))
expect_equal(max(paleomap$breaks), max(botrim$breaks))
expect_equal(
	botrim$mid[-1],
	paleomap$mid[paleomap$mid> trimval])
# new mid
expect_equal(botrim$mid[1], mean(botrim$breaks[1:2]))

#top trimming
trimval <- 4000
expect_silent(toptrim <- trimramp(paleomap, high=trimval))
expect_true(all(toptrim$breaks<=trimval))
expect_true(all(toptrim$mid<trimval))
expect_equal(trimval, max(toptrim$breaks))
expect_equal(min(paleomap$breaks), min(toptrim$breaks))

# which are matching
matchIndex <- which(paleomap$mid< trimval)

# the last value will not match
expect_equal(
	toptrim$mid[-length(toptrim$mid)],
	paleomap$mid[matchIndex[-length(matchIndex)]])

# new mid
expect_equal(toptrim$mid[length(toptrim$mid)],
	mean(toptrim$breaks[(length(toptrim$breaks)-1):length(toptrim$breaks)]))

################################################################################
# 2. break coinciding with trimming value
# bottom trimming
trimval <- -4060
expect_silent(botrim <- trimramp(paleomap, low=trimval))
expect_true(all(botrim$breaks>=trimval))
expect_true(all(botrim$mid>trimval))
expect_equal(trimval, min(botrim$breaks))
expect_equal(max(paleomap$breaks), max(botrim$breaks))
expect_equal(
	botrim$mid,
	paleomap$mid[paleomap$mid> trimval])
# new mid
expect_equal(botrim$mid[1], mean(botrim$breaks[1:2]))

#top trimming
trimval <- 4050
expect_silent(toptrim <- trimramp(paleomap, high=trimval))
expect_true(all(toptrim$breaks<=trimval))
expect_true(all(toptrim$mid<trimval))
expect_equal(trimval, max(toptrim$breaks))
expect_equal(min(paleomap$breaks), min(toptrim$breaks))

# which are matching
matchIndex <- which(paleomap$mid< trimval)

# the last value will not match
expect_equal(
	toptrim$mid[-length(toptrim$mid)],
	paleomap$mid[matchIndex[-length(matchIndex)]])

# new mid
expect_equal(toptrim$mid[length(toptrim$mid)],
	mean(toptrim$breaks[(length(toptrim$breaks)-1):length(toptrim$breaks)]))

################################################################################
# 3. Trimming beyond the bounds
# bottom trimming
trimval <- -16000
expect_silent(botrim <- trimramp(paleomap, low=trimval))
expect_true(all(botrim$breaks>=trimval))
expect_true(all(botrim$mid>trimval))
expect_equal(trimval, min(botrim$breaks))
expect_equal(max(paleomap$breaks), max(botrim$breaks))
expect_equal(
	botrim$mid[-1],
	paleomap$mid[-1])
# new mid
expect_equal(botrim$mid[1], mean(botrim$breaks[1:2]))


#top trimming
trimval <- 15000
expect_silent(toptrim <- trimramp(paleomap, high=trimval))
expect_true(all(toptrim$breaks<=trimval))
expect_true(all(toptrim$mid<trimval))
expect_equal(trimval, max(toptrim$breaks))
expect_equal(min(paleomap$breaks), min(toptrim$breaks))

# which are matching
matchIndex <- which(paleomap$mid< trimval)

# the last value will not match
expect_equal(
	toptrim$mid[-length(toptrim$mid)],
	paleomap$mid[matchIndex[-length(matchIndex)]])

# new mid
expect_equal(toptrim$mid[length(toptrim$mid)],
	mean(toptrim$breaks[(length(toptrim$breaks)-1):length(toptrim$breaks)]))


# 4. Trimming beyond the bounds
# bottom trimming
trimval <- 16000
expect_error(trimramp(paleomap, low=trimval))

# top trimming
trimval <- -16000
expect_error(trimramp(paleomap, high=trimval))

# top trimming
trimval <- 10500
expect_error(trimramp(paleomap, low=trimval))

# - low is higher than the highest value
# - high is lower than the lowest value

################################################################################
# Defense testing

trimval <- NA
expect_error(trimramp(paleomap, high=trimval))
expect_error(trimramp(paleomap, low=trimval))
trimval <- Inf
expect_error(trimramp(paleomap, high=trimval))
expect_error(trimramp(paleomap, low=trimval))
trimval <- c(1,2)
expect_error(trimramp(paleomap, high=trimval))
expect_error(trimramp(paleomap, low=trimval))

# both NULL
expect_error(trimramp(paleomap))
