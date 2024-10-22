library(quantmod)
library(tinytest)

data(sample_matrix, package = "xts")
d <- as.xts(sample_matrix)
simple.colnames <- c("Open", "High", "Low", "Close")

#has.OHLC will test underlying has.op, hasCl, etc
colnames(d) <- simple.colnames
expect_equal(has.OHLC(d), rep(TRUE,4))
expect_equal(has.OHLC(d, which = T), c(1,2,3,4))

colnames(d) <- paste("OPEN", simple.colnames, sep = ".")
expect_equal(has.OHLC(d), rep(TRUE,4))
expect_equal(has.OHLC(d, which = T), c(1,2,3,4))

colnames(d) <- paste("HIGH", simple.colnames, sep = ".")
expect_equal(has.OHLC(d), rep(TRUE,4))
expect_equal(has.OHLC(d, which = T), c(1,2,3,4))

colnames(d) <- paste("LOW", simple.colnames, sep = ".")
expect_equal(has.OHLC(d), rep(TRUE,4))
expect_equal(has.OHLC(d, which = T), c(1,2,3,4))

colnames(d) <- paste("ILOW", simple.colnames, sep = ".")
expect_equal(has.OHLC(d), rep(TRUE,4))
expect_equal(has.OHLC(d, which = T), c(1,2,3,4))

colnames(d) <- paste("LOW.W", simple.colnames, sep = ".")
expect_equal(has.OHLC(d), rep(TRUE,4))
expect_equal(has.OHLC(d, which = T), c(1,2,3,4))

colnames(d) <- paste("My.LOW", simple.colnames, sep = ".")
expect_equal(has.OHLC(d), rep(TRUE,4))
expect_equal(has.OHLC(d, which = T), c(1,2,3,4))