## OHLC test-cases, including underlying supporting functions.

# Load stock test data (originating from getSymbols())
stock <- readRDS("stock.rda")
cols <- colnames(stock)

# Test is/has OHLC functions to be true.
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

# Test "Open" columns with column "Op".
colnames(stock) <- gsub("MSFT.Open", "Op", cols)
expect_true(has.Op(stock))
expect_equal(has.Op(stock, which = TRUE), 1)
expect_identical(colnames(Op(stock)), "Op")

# Test "High" columns with column "Hi".
colnames(stock) <- gsub("MSFT.High", "Hi", cols)
expect_true(has.Hi(stock))
expect_equal(has.Hi(stock, which = TRUE), 2)
expect_identical(colnames(Hi(stock)), "Hi")

# Test "Low" columns with column "Lo".
colnames(stock) <- gsub("MSFT.Low", "Lo", cols)
expect_true(has.Lo(stock))
expect_equal(has.Lo(stock, which = TRUE), 3)
expect_identical(colnames(Lo(stock)), "Lo")

# Test "Close" columns with column "Cl".
colnames(stock) <- gsub("MSFT.Close", "Cl", cols)
expect_true(has.Cl(stock))
expect_equal(has.Cl(stock, which = TRUE), 4)
expect_identical(colnames(Cl(stock)), "Cl")

# Test "Volume" columns with column "Vo".
colnames(stock) <- gsub("MSFT.Volume", "Vo", cols)
expect_true(has.Vo(stock))
expect_equal(has.Vo(stock, which = TRUE), 5)
expect_identical(colnames(Vo(stock)), "Vo")

# Test "Adjusted" columns with column "Ad".
colnames(stock) <- gsub("MSFT.Adjusted", "Ad", cols)
expect_true(has.Ad(stock))
expect_equal(has.Ad(stock, which = TRUE), 6)
expect_identical(colnames(Ad(stock)), "Ad")

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
