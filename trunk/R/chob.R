`.chob` <- list(NULL)

`write.chob` <-
function(x,pos)
{
  env <- as.environment("package:quantmod")
  environment(x) <- env
  locked <- bindingIsLocked('.chob',env)
  if(locked)
    unlockBinding('.chob',env)
  orig.chob <- get.chob()
  if(missing(pos)) pos <- length(orig.chob)+1
  orig.chob[[pos]] <- x
  assign('.chob',orig.chob,env)
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
    x <- list(NULL)  
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
           length="numeric",
           color.vol="logical",multi.col="logical",
           spacing="numeric",width="numeric",
           bp="numeric",x.labels="character",
           colors="ANY",time.scale="ANY"
         )
)

setClass("chobTA",
         representation(
            new="logical",
            TA.values="ANY",
            name="character",
            params="ANY"
         )
)

setMethod("show","chobTA",function(object) { 
    chob <- get.chob()[[dev.cur()]]
    TA <- chob@passed.args$TA
    chob@passed.args$TA <- c(TA,object)
    do.call('cS2',chob@passed.args)
  }
)
