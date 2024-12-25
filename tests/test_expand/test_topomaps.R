library(tinytest)

data(topos)

n <- 1000

# the theme
expect_silent(jak <- rampage::expand(topos$jakarta, n=n))
expect_equal(class(jak), "list")
expect_equal(length(jak), 3)
expect_equal(names(jak), c("col", "breaks", "mid"))
expect_equivalent(unlist(lapply(jak, class)), c("character", "numeric","numeric" ))

expect_equal(length(jak[["col"]]), n)
expect_equal(length(jak[["mid"]]), n)
expect_equal(length(jak[["breaks"]]), n+1)

expect_equal(max(jak$mid), max(topos$jakarta$z))
expect_equal(min(jak$mid), min(topos$jakarta$z))
expect_equal(jak$mid, (jak$breaks[2:length(jak$breaks)-1]+jak$breaks[2:length(jak$breaks)])/2)

# test with non-integer n


# test for the strictly monotonically increasing z
# no duplicates in z
