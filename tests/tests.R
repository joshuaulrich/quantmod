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

# should not throw a warning
op.warn <- getOption("warn")
options(warn = 2)
quantmod::getSymbols("SPY", src = "google", from = Sys.Date() - 10)
options(warn = op.warn)



#IEX tests
structure(list(symbol = c("VXX", "SNAP"), price = c(12.785, 17.27),
               size = c(100L, 200L), time = c(1498075199.954, 1498075184.411),
               .Names = c("symbol", "price", "size", "time"),
               row.names = 1:2, class = "data.frame") -> here.now.vxx.snap




      
       
          
#right shape
stopifnot( identical(
          dim(getQuote.IEX('cat')),
          c(1L,4L)
          )
)
          

#called the expected thing
stopifnot( identical(
                    c("symbol", "price", "size", "time"),
                    names(  getQuote.IEX('slb') )
          )
          
          
#IEX quotes are of the expected type
expected.types <- structure(list(symbol = "character", price = "numeric", size = "integer", 
    time = c("POSIXct", "POSIXt")), .Names = c("symbol", "price", "size", "time"))
          
stopifnot( identical(
                    lapply(getQuote.IEX('chd'), FUN=class),
                    expected.types
                    )
          )
