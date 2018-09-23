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
