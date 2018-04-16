library(quantmod)

# Checks for Alpha Vantage
apikey <- Sys.getenv("AV_API_KEY")
if (apikey != "") {
  ibm_daily_unadj <- getSymbols("IBM", src = "av", api.key = apikey,
    adjusted = FALSE, periodicity = "daily", auto.assign = FALSE)
  ibm_daily_adj   <- getSymbols("IBM", src = "av", api.key = apikey,
    adjusted = TRUE, periodicity = "daily", auto.assign = FALSE)
  stopifnot(has.Ad(ibm_daily_adj))

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

x <- try({
  getSymbols("IBM;WYSIWYG", env = e, src = "csv", dir = td, col.names = cn)
}, silent = TRUE)
stopifnot(exists("IBM", e))
rm(IBM, pos = e)

x <- try({
  getSymbols("IBM;WYSIWYG", env = e, src = "rda", dir = td, col.names = cn)
}, silent = TRUE)
stopifnot(exists("IBM", e))
rm(IBM, pos = e)

x <- try({
  getSymbols("IBM;WYSIWYG", env = e, src = "yahoo")
}, silent = TRUE)
stopifnot(exists("IBM", e))
rm(IBM, pos = e)

x <- try({
  getSymbols("IBM;WYSIWYG", env = e, src = "av", api.key = apikey)
}, silent = TRUE)
stopifnot(exists("IBM", e))
rm(IBM, pos = e)

x <- try({
  getSymbols("DGS10;WYSIWYG", env = e, src = "FRED")
}, silent = TRUE)
stopifnot(exists("DGS10", e))
rm(DGS10, pos = e)

x <- try({
  getSymbols("EUR/USD;WYSIWYG", env = e, src = "oanda")
}, silent = TRUE)
stopifnot(exists("EURUSD", e))
rm(EURUSD, pos = e)
