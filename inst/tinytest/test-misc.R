av.key <- Sys.getenv("QUANTMOD_AV_API_KEY")
tiingo.key <- Sys.getenv("QUANTMOD_TIINGO_API_KEY")
test.web.endpoints <- Sys.getenv("QUANTMOD_TEST_WEB_ENDPOINTS")

if (nzchar(test.web.endpoints)) {
  # should throw an error
  errorKey <- "d116c846835e633aacedb1a31959dd2724cd67b8"
  x <- try(
           quantmod::getSymbols("AAPL", src = "tiingo", data.type = "csv", api.key = errorKey)
           , silent = TRUE)
  expect_inherits(x, "try-error")
  x <- try(
           quantmod::getSymbols("AAPL", src = "tiingo", data.type = "json", api.key = errorKey)
           , silent = TRUE)
  expect_inherits(x, "try-error")

  syms <- c("SPY", "WYSIWYG")
  symstr <- paste(syms, collapse = ";")
  x <- try(getQuote(symstr, src = "yahoo"), silent = TRUE)
  expect_inherits(x, "data.frame")
  expect_true(all(rownames(x) == syms))
  expect_true(!is.na(x["SPY", "Last"]) && is.na(x["WYSIWYG", "Last"]))

  #test batch handling
  x <- getQuote(c("SPY", paste0(LETTERS, 1:199), "IWM"), src = "yahoo")
  expect_inherits(x, "data.frame")
  expect_true(nrow(x) == 201L)

  if (av.key != "") {
    x <- try(getQuote(symstr, src = "av", api.key = av.key), silent = TRUE)
    expect_inherits(x, "data.frame") && all(rownames(x) == syms)
  }
  if (tiingo.key != "") {
    x <- try(getQuote(symstr, src = "tiingo", api.key = tiingo.key), silent = TRUE)
    expect_inherits(x, "data.frame")
    expect_true(all(rownames(x) == syms))
  }

  # ensure symbol order is preserved
  syms <- sample(c("SPY", "TLT", "IWM", "QQQ", "WYSIWYG"))
  x <- try(getQuote(syms, src = "yahoo"), silent = TRUE)
  expect_inherits(x, "data.frame")
  expect_true(all(rownames(x) == syms))
}
