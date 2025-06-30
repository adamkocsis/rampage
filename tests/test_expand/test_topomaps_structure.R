library(tinytest)

data(topos)

################################################################################
# Input data.frame
# Very high n.
n <- 1000

# the theme
expect_silent(jak <- rampage::expand(topos$jakarta, n=n))
expect_true(is.list(jak))
expect_true(inherits(jak, "calibramp"))
expect_equal(length(jak), 3)
expect_equal(names(jak), c("col", "breaks", "mid"))
expect_equivalent(unlist(lapply(jak, class)), c("character", "numeric","numeric" ))

expect_equal(length(jak[["col"]]), n)
expect_equal(length(jak[["mid"]]), n)
expect_equal(length(jak[["breaks"]]), n+1)

expect_equal(max(jak$mid), max(topos$jakarta$z))
expect_equal(min(jak$mid), min(topos$jakarta$z))
expect_equal(jak$mid, (jak$breaks[2:length(jak$breaks)-1]+jak$breaks[2:length(jak$breaks)])/2)


################################################################################
# Pair vector entry should give the exact same result
# A. Non-retarded order
expect_silent(jak2 <- rampage::expand(color=topos$jakarta$color,z=topos$jakarta$z,  n=n))
expect_equal(jak, jak2)

# B. shit order of values - enforce reordering
colorInput <- topos$jakarta$color
zInput <- topos$jakarta$z
set.seed(1)
reOrd <- sample(1:length(zInput))
colorInput <- colorInput[reOrd]
zInput <- zInput[reOrd]

expect_silent(jak3 <- rampage::expand(color=colorInput,z=zInput,  n=n))
expect_equal(jak, jak3)

################################################################################
# All the wrong inputs

################################################################################
# test with non-integer n
expect_error(rampage::expand(topos$jakarta, n=100.4))

# duplicates in z
colorInput <- topos$jakarta$color
zInput <- topos$jakarta$z
zInput[3] <- zInput[4]
expect_error(rampage::expand(color=colorInput,z=zInput, n=n))

################################################################################
#  Missing input
expect_error(expand(n=1000))

# wrong vector pair
expect_error(expand(n=100, z="a", color="#FF0000" ))
expect_error(expand(n=100, z=1:10, color="#FF0000" ))

# Missing values are provided in z
colorInput <- topos$jakarta$color
zInput <- topos$jakarta$z
zInput[3] <- NA
expect_error(expand(n=100, z=zInput, color=colorInput))

# mulitple ns
expect_error(expand(n=c(1,1000)))
