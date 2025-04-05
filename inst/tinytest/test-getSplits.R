library(quantmod)

test.web.endpoints <- Sys.getenv("QUANTMOD_TEST_WEB_ENDPOINTS")

# Ensure getSplits() returns the expected ratio
if (nzchar(test.web.endpoints)) {
  aapl.spl <- as.numeric(getSplits("AAPL")["/2018"])
  expected <- c(0.5, 0.5, 0.5, 1/7)
  expect_true(all.equal(aapl.spl, expected))
}
