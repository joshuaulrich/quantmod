library(quantmod)

test.yahoo.finance <- nzchar(Sys.getenv("QUANTMOD_TEST_YAHOO_FINANCE"))

# Ensure getSplits() returns the expected ratio
if (test.yahoo.finance) {
  aapl.spl <- as.numeric(getSplits("AAPL")["/2018"])
  expected <- c(0.5, 0.5, 0.5, 1/7)
  stopifnot(isTRUE(all.equal(aapl.spl, expected)))
}
