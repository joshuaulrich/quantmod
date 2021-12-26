library(quantmod)

# Tests for getFinancials

# Checks for tiingo
# tiingo allows access to dow30 symbols for testing/development
apikey <- Sys.getenv("QUANTMOD_TIINGO_API_KEY")
if (nzchar(apikey)) {
  aapl <- getFinancials("AAPL", src = "tiingo", api.key = apikey, 
    auto.assign = FALSE)
  stopifnot(inherits(aapl, "financials"))

  #test multisymbol path with bad symbols
  retsym <- getFinancials("AAPL;BA;UNKNOWNSYMBOL", src = "tiingo", 
    api.key = apikey, auto.assign = TRUE)
  stopifnot(retsym == c("AAPL.f", "BA.f"))
}
