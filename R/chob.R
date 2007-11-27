`.chob` <- list()

`write.chob` <-
function(x)
{
  env <- as.environment("package:quantmod")
  environment(x) <- env
  locked <- bindingIsLocked('.chob',env)
  if(locked)
    unlockBinding('.chob',env)
  orig.chob <- get.chob()
  x <- c(orig.chob,x)
  assign('.chob',x,env)
  if(locked) {
    ow <- options("warn")
    on.exit(options(ow))
    options(warn=-1)
    lockBinding('.chob',env)
   }
  invisible(1)
}

`get.chob` <-
function()
{
  x <- get('.chob',as.environment("package:quantmod"))
  attr(x,'.Environment') <- NULL
  return(x)
}

`release.chob` <-
function(n)
{
  if(missing(n)) {
    x <- list()  
  } else {
    x <- get.chob()[-n]
  }
  env <- as.environment("package:quantmod")
  environment(x) <- env
  locked <- bindingIsLocked('.chob',env)
  if(locked)
    unlockBinding('.chob',env)
  assign('.chob',x,env)
  if(locked) {
    ow <- options("warn")
    on.exit(options(ow))
    options(warn=-1)
    lockBinding('.chob',env)
   }
  invisible(1)
}


`print.chob` <-
function(x)
{
#  plot.chob(x)
}
setClass("chob",
         representation(
           device="ANY",
           call="call",
           passed.args="ANY",
           windows="numeric",
           xrange="numeric",
           yrange="numeric",
           length="numeric"
         )
)

setClass("chobTA",
         representation(
            new="logical",
            TA.values="ANY",
            name="character"
         )
)

##setMethod("show","chob",function(object) { plot.chob(object) })
