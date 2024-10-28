library(quantmod)
library(tinytest)

data(sample_matrix, package = "xts")
stock <- as.xts(sample_matrix)
stock$Volume <- stock$Close
stock$Adjusted <- stock$Close
simple.colnames <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")

# basic functionality
colnames(stock) <- paste("MSFT", simple.colnames, sep = ".")
expect_true(has.Op(stock))
expect_true(has.Hi(stock))
expect_true(has.Lo(stock))
expect_true(has.Cl(stock))
expect_true(has.Vo(stock))
expect_true(has.Ad(stock))
expect_true(is.HLC(stock))
expect_true(all(has.HLC(stock)))
expect_true(is.OHLC(stock))
expect_true(all(has.OHLC(stock)))
expect_true(is.OHLCV(stock))
expect_true(all(has.OHLCV(stock)))

# Test which for has/OHLC functions.
expect_equal(has.Op(stock, which = TRUE), 1)
expect_equal(has.Hi(stock, which = TRUE), 2)
expect_equal(has.Lo(stock, which = TRUE), 3)
expect_equal(has.Cl(stock, which = TRUE), 4)
expect_equal(has.Vo(stock, which = TRUE), 5)
expect_equal(has.Ad(stock, which = TRUE), 6)
expect_equal(has.HLC(stock, which = TRUE), c(2,3,4))
expect_equal(has.OHLC(stock, which = TRUE), c(1,2,3,4))
expect_equal(has.OHLCV(stock, which = TRUE), c(1,2,3,4,5))

#has.OHLC will test underlying has.Op, has.Cl, etc. It will NOT test has.Ad
colnames(stock) <- simple.colnames
expect_true(all(has.OHLCV(stock)))
expect_equal(has.OHLCV(stock, which = T), c(1,2,3,4,5))

colnames(stock) <- paste("OPEN", simple.colnames, sep = ".")
expect_true(all(has.OHLCV(stock)))
expect_equal(has.OHLCV(stock, which = T), c(1,2,3,4,5))

colnames(stock) <- paste("HIGH", simple.colnames, sep = ".")
expect_true(all(has.OHLCV(stock)))
expect_equal(has.OHLCV(stock, which = T), c(1,2,3,4,5))

colnames(stock) <- paste("LOW", simple.colnames, sep = ".")
expect_true(all(has.OHLCV(stock)))
expect_equal(has.OHLCV(stock, which = T), c(1,2,3,4,5))

colnames(stock) <- paste("CLOSE", simple.colnames, sep = ".")
expect_true(all(has.OHLCV(stock)))
expect_equal(has.OHLCV(stock, which = T), c(1,2,3,4,5))

colnames(stock) <- paste("VOLUME", simple.colnames, sep = ".")
expect_true(all(has.OHLCV(stock)))
expect_equal(has.OHLCV(stock, which = T), c(1,2,3,4,5))

colnames(stock) <- paste("ADJUSTED", simple.colnames, sep = ".")
expect_true(all(has.OHLCV(stock)))
expect_equal(has.OHLCV(stock, which = T), c(1,2,3,4,5))

colnames(stock) <- paste("ILOW", simple.colnames, sep = ".")
expect_true(all(has.OHLCV(stock)))
expect_equal(has.OHLCV(stock, which = T), c(1,2,3,4,5))

colnames(stock) <- paste("LOW.W", simple.colnames, sep = ".")
expect_true(all(has.OHLCV(stock)))
expect_equal(has.OHLCV(stock, which = T), c(1,2,3,4,5))

colnames(stock) <- paste("LOW_A", simple.colnames, sep = ".")
expect_true(all(has.OHLCV(stock)))
expect_equal(has.OHLCV(stock, which = T), c(1,2,3,4,5))

colnames(stock) <- paste("^LOW", simple.colnames, sep = ".")
expect_true(all(has.OHLCV(stock)))
expect_equal(has.OHLCV(stock, which = T), c(1,2,3,4,5))

colnames(stock) <- paste("My.LOW", simple.colnames, sep = ".")
expect_true(all(has.OHLCV(stock)))
expect_equal(has.OHLCV(stock, which = T), c(1,2,3,4,5))

colnames(stock) <- paste("VLOWY", simple.colnames, sep = ".")
expect_true(all(has.OHLCV(stock)))
expect_equal(has.OHLCV(stock, which = T), c(1,2,3,4,5))

# low in colname returned by function TTR::stoch()
colnames(stock) <- paste("LOW", simple.colnames, sep = ".")
stock$slowD <- stock[,4]
expect_true(all(has.OHLCV(stock)))
expect_equal(has.OHLCV(stock, which = T), c(1,2,3,4,5))
stock$slowD <- NULL
