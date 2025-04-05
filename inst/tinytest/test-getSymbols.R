library(quantmod)

# Tests for getSymbols
test.web.endpoints <- Sys.getenv("QUANTMOD_TEST_WEB_ENDPOINTS")

if (nzchar(test.web.endpoints)) {
  # Checks for Alpha Vantage
  apikey <- Sys.getenv("QUANTMOD_AV_API_KEY")
  if (nzchar(apikey)) {
    ibm_daily_unadj <- getSymbols("IBM", src = "av", api.key = apikey,
      adjusted = FALSE, periodicity = "daily", auto.assign = FALSE)
    ibm_daily_adj   <- getSymbols("IBM", src = "av", api.key = apikey,
      adjusted = TRUE, periodicity = "daily", auto.assign = FALSE)
    stopifnot(has.Ad(ibm_daily_adj))

    Sys.sleep(60) #AV throttles to 5 calls per minute

    ibm_weekly_unadj <- getSymbols("IBM", src = "av", api.key = apikey,
      adjusted = FALSE, periodicity = "weekly", auto.assign = FALSE)
    ibm_weekly_adj   <- getSymbols("IBM", src = "av", api.key = apikey,
      adjusted = TRUE, periodicity = "weekly", auto.assign = FALSE)
    stopifnot(has.Ad(ibm_weekly_adj))

    ibm_monthly_unadj <- getSymbols("IBM", src = "av", api.key = apikey,
      adjusted = FALSE, periodicity = "monthly", auto.assign = FALSE)
    ibm_monthly_adj   <- getSymbols("IBM", src = "av", api.key = apikey,
      adjusted = TRUE, periodicity = "monthly", auto.assign = FALSE)
    stopifnot(has.Ad(ibm_monthly_adj))
  }
}

# Checks to ensure caught errors do not prevent other symbols from loading.
# Use one symbol that always works (e.g. from disk) and another that fails.
data(sample_matrix, package = "xts")
IBM <- as.xts(sample_matrix)
cn <- c("Open", "High", "Low", "Close")

td <- tempdir()
tf <- file.path(td, "IBM.rda")
save(IBM, file = tf)
tf <- file.path(td, "IBM.csv")
write.zoo(IBM, file = tf, sep = ",")
rm(IBM)

e <- new.env()

# Test getSymbols() works if only passed one symbol that does not have data.
x <- try({
  getSymbols("IBM;WYSIWYG", env = e, src = "csv", dir = td, col.names = cn)
}, silent = TRUE)
expect_true(exists("IBM", e))
rm(IBM, pos = e)

x <- try({
  getSymbols("IBM;WYSIWYG", env = e, src = "rda", dir = td, col.names = cn)
}, silent = TRUE)
expect_true(exists("IBM", e))
rm(IBM, pos = e)

if (nzchar(test.web.endpoints)) {
  x <- try({
    getSymbols("IBM;WYSIWYG", env = e, src = "yahoo")
  }, silent = TRUE)
  expect_true(exists("IBM", e))
  rm(IBM, pos = e)

  if (nzchar(apikey)) {
    x <- try({
      getSymbols("IBM;WYSIWYG", env = e, src = "av", api.key = apikey)
    }, silent = TRUE)
    expect_true(exists("IBM", e))
    rm(IBM, pos = e)
  }

  x <- try({
    getSymbols("DGS10;WYSIWYG", env = e, src = "FRED")
  }, silent = TRUE)
  expect_true(exists("DGS10", e))
  rm(DGS10, pos = e)

  x <- try({
    getSymbols("EUR/USD;WYS/WYG", env = e, src = "oanda")
  }, silent = TRUE)
  expect_true(exists("EURUSD", e))
  rm(EURUSD, pos = e)

  # Ensure getSymbols() errors if only passed one symbol that does not have data.
  # "csv" and "rda" already skip missing symbols
  x <- try({
    getSymbols("WYSIWYG", env = e, src = "yahoo")
  }, silent = TRUE)
  expect_inherits(x, "try-error")

  x <- try({
    getSymbols("WYSIWYG", env = e, src = "FRED")
  }, silent = TRUE)
  expect_inherits(x, "try-error")

  if (nzchar(apikey)) {
    x <- try({
      getSymbols("WYSIWYG", env = e, src = "av", api.key = apikey)
    }, silent = TRUE)
    expect_inherits(x, "try-error")
  }

  x <- try({
    getSymbols("WYS/WYG", env = e, src = "oanda")
  }, silent = TRUE)
  expect_inherits(x, "try-error")

  # Individual getSymbols() "methods" should not error if only passed one symbol.
  setSymbolLookup(AAPL = "yahoo", DGS10 = "FRED")
  getSymbols("AAPL;DGS10", env = e)
  expect_true(exists("AAPL", e))
  expect_true(exists("DGS10", e))
}
