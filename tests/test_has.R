library(quantmod)
library(tinytest)

data(sample_matrix, package = "xts")
stock <- as.xts(sample_matrix)
stock$Volume <- stock$Close
stock$Adjusted <- stock$Close
simple.colnames <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")

#has.OHLC will test underlying has.op, hasCl, etc
colnames(stock) <- simple.colnames
expect_true(all(has.OHLC(stock)))
expect_equal(has.OHLC(stock, which = T), c(1,2,3,4))

colnames(stock) <- paste("OPEN", simple.colnames, sep = ".")
expect_true(all(has.OHLC(stock)))
expect_equal(has.OHLC(stock, which = T), c(1,2,3,4))

colnames(stock) <- paste("HIGH", simple.colnames, sep = ".")
expect_true(all(has.OHLC(stock)))
expect_equal(has.OHLC(stock, which = T), c(1,2,3,4))

colnames(stock) <- paste("LOW", simple.colnames, sep = ".")
expect_true(all(has.OHLC(stock)))
expect_equal(has.OHLC(stock, which = T), c(1,2,3,4))

colnames(stock) <- paste("ILOW", simple.colnames, sep = ".")
expect_true(all(has.OHLC(stock)))
expect_equal(has.OHLC(stock, which = T), c(1,2,3,4))

colnames(stock) <- paste("LOW.W", simple.colnames, sep = ".")
expect_true(all(has.OHLC(stock)))
expect_equal(has.OHLC(stock, which = T), c(1,2,3,4))

colnames(stock) <- paste("My.LOW", simple.colnames, sep = ".")
expect_true(all(has.OHLC(stock)))
expect_equal(has.OHLC(stock, which = T), c(1,2,3,4))

expect_true(has.Op(stock))
expect_true(has.Hi(stock))
expect_true(has.Lo(stock))
expect_true(has.Cl(stock))
expect_true(has.Vo(stock))
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
expect_equal(has.HLC(stock, which = TRUE), c(2,3,4))
expect_equal(has.OHLC(stock, which = TRUE), c(1,2,3,4))
expect_equal(has.OHLCV(stock, which = TRUE), c(1,2,3,4,5))

# Test return correct OHLC column(s).
expect_identical(colnames(Op(stock)), cols[1])
expect_identical(colnames(Hi(stock)), cols[2])
expect_identical(colnames(Lo(stock)), cols[3])
expect_identical(colnames(Cl(stock)), cols[4])
expect_identical(colnames(HLC(stock)), cols[c(2,3,4)])
expect_identical(colnames(OHLC(stock)), cols[c(1,2,3,4)])
expect_identical(colnames(OHLCV(stock)), cols[c(1,2,3,4,5)])

# Test sample matrix regression
data(sample_matrix, package = "xts")
sample <- as.xts(sample_matrix)
expect_equal(has.Op(sample, which = TRUE), 1)
expect_equal(has.Hi(sample, which = TRUE), 2)
expect_equal(has.Lo(sample, which = TRUE), 3)
expect_equal(has.Cl(sample, which = TRUE), 4)
expect_identical(colnames(Op(sample)), "Open")
expect_identical(colnames(Hi(sample)), "High")
expect_identical(colnames(Lo(sample)), "Low")
expect_identical(colnames(Cl(sample)), "Close")

# Test "Open" columns with symbol "OPEN".
colnames(stock) <- gsub("MSFT", "OPEN", cols)
expect_true(has.Op(stock))
expect_equal(has.Op(stock, which = TRUE), 1)
expect_identical(colnames(Op(stock)), colnames(stock)[1])

# Test "High" columns with symbol "HIGH".
colnames(stock) <- gsub("MSFT", "HIGH", cols)
expect_true(has.Hi(stock))
expect_equal(has.Hi(stock, which = TRUE), 2)
expect_identical(colnames(Hi(stock)), colnames(stock)[2])

# Test "Low" columns with symbol "LOW".
colnames(stock) <- gsub("MSFT", "LOW", cols)
expect_true(has.Lo(stock))
expect_equal(has.Lo(stock, which = TRUE), 3)
expect_identical(colnames(Lo(stock)), colnames(stock)[3])

# Test "Close" columns with symbol "CLOSE".
colnames(stock) <- gsub("MSFT", "CLOSE", cols)
expect_true(has.Cl(stock))
expect_equal(has.Cl(stock, which = TRUE), 4)
expect_identical(colnames(Cl(stock)), colnames(stock)[4])

# Test "Volume" columns with symbol "VOLUME".
colnames(stock) <- gsub("MSFT", "VOLUME", cols)
expect_true(has.Vo(stock))
expect_equal(has.Vo(stock, which = TRUE), 5)
expect_identical(colnames(Vo(stock)), colnames(stock)[5])

# Test "Adjusted" columns with symbol "ADJUSTED".
colnames(stock) <- gsub("MSFT", "ADJUSTED", cols)
expect_true(has.Ad(stock))
expect_equal(has.Ad(stock, which = TRUE), 6)
expect_identical(colnames(Ad(stock)), colnames(stock)[6])

colnames(stock) <- simple.colnames
stock$slowD <- stock[,4]
expect_true(all(has.OHLC(stock)))
expect_equal(has.OHLC(stock, which = T), c(1,2,3,4))
