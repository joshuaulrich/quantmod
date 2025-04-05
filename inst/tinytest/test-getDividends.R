library(quantmod)

test.web.endpoints <- Sys.getenv("QUANTMOD_TEST_WEB_ENDPOINTS")

# split-adjusted by default
#if (nzchar(test.web.endpoints)) {
if (FALSE) {
  cf.div.adj <- as.numeric(getDividends("CF")["2015"])
  expect_true(all.equal(cf.div.adj, rep(0.3, 4)))
  cf.div.raw <- as.numeric(getDividends("CF", split.adjust = FALSE)["2015"])
  expect_true(all.equal(cf.div.raw, c(1.5, 1.5, 0.3, 0.3)))
}
