#IEX tests

structure(list(symbol = c("VXX", "SNAP"), price = c(12.785, 17.27),
               size = c(100L, 200L), time = c(1498075199.954, 1498075184.411),
               .Names = c("symbol", "price", "size", "time"),
               row.names = 1:2, class = "data.frame")
          ) -> here.now.vxx.snap







#net test       

try(  getQuote.IEX('vxx')  )

try(  getQuote.IEX( c('vxx','snap') ) )

try(  getQuote.IEX( list('vxx','snap') ) )

      

       

          

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
