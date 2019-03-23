library(quantmod)

# Ensure getSplits() returns the expected ratio
aapl.spl <- as.numeric(getSplits("AAPL")["/2018"])
expected <- c(0.5, 0.5, 0.5, 1/7)
stopifnot(isTRUE(all.equal(aapl.spl, expected)))
