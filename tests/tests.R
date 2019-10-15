# Call as.zoo before quantmod is loaded and registers its S3 method
dc <- c("2015-01-01", "2016-01-01", "2017-01-01")
dd <- as.Date(dc)

f <- data.frame(a = 1:3)
r <- f
rownames(r) <- dc

zz.f.date <- zoo::as.zoo(f, order.by = dd)
zz.f.char <- zoo::as.zoo(f, order.by = dc)
zz.f <- zoo::as.zoo(f)

zz.r.date <- zoo::as.zoo(r, order.by = dd)
zz.r.char <- zoo::as.zoo(r, order.by = dc)
zz.r <- zoo::as.zoo(r)

library(quantmod)

### quantmod:::as.zoo.data.frame

# should be the same as zoo:::as.zoo.data.frame when order.by is provided
stopifnot(identical(zz.f.char, as.zoo(f, order.by = dc)))
stopifnot(identical(zz.f.date, as.zoo(f, order.by = dd)))
stopifnot(identical(zz.r.char, as.zoo(r, order.by = dc)))
stopifnot(identical(zz.r.date, as.zoo(r, order.by = dd)))

# should throw an error
errorKey <- "d116c846835e633aacedb1a31959dd2724cd67b8"
x <- try(
  quantmod::getSymbols("AAPL", src = "tiingo", data.type = "csv", api.key = errorKey)
, silent = TRUE)
stopifnot(inherits(x, "try-error"))
x <- try(
  quantmod::getSymbols("AAPL", src = "tiingo", data.type = "json", api.key = errorKey)
, silent = TRUE)
stopifnot(inherits(x, "try-error"))