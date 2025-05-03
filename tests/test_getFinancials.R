library(quantmod)

# Tests for getFinancials

# Checks for tiingo
# tiingo allows access to dow30 symbols for testing/development
apikey <- Sys.getenv("QUANTMOD_TIINGO_API_KEY")
if (nzchar(apikey)) {
  aapl <- getFinancials("AAPL", src = "tiingo", api.key = apikey, 
    auto.assign = FALSE)
  stopifnot(inherits(aapl, "financials"))
  aapl.df <- as.data.frame(aapl)
  stopifnot(is.data.frame(aapl.df))
  #conversion to df shoudl test unpacing nested elements
  stopifnot(names(aapl.df) == c("date", "entry", "period", "value", "type"))

  #test multisymbol path with bad symbols
  retsym <- getFinancials("AAPL;BA;UNKNOWNSYMBOL", src = "tiingo", 
    api.key = apikey, auto.assign = TRUE)
  stopifnot(retsym == c("AAPL.f", "BA.f"))
}
