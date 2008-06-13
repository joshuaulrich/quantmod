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


`plot.chobTA` <-
function(x,y,...)
{
    lchob <- get.chob()[[dev.cur()]]
    TA <- lchob@passed.args$TA
    lchob@passed.args$TA <- c(TA,x)
    lchob@windows <- lchob@windows + ifelse(x@new,1,0)
    do.call('chartSeries.chob',list(lchob))
}
setClass("chob",
         representation(
           device="ANY",
           call="call",
           xdata='ANY',
           xsubset='ANY',
           name="character",
           type="character",
           passed.args="ANY",
           windows="numeric",
           xrange="numeric",
           yrange="numeric",
           log.scale="logical",
           length="numeric",
           color.vol="logical",multi.col="logical",
           show.vol="logical",show.grid="logical",
           line.type="character",bar.type="character",
           xlab="character",ylab="character",
           spacing="numeric",width="numeric",
           bp="numeric",x.labels="character",
           colors="ANY",layout="ANY",time.scale="ANY",
           minor.ticks="logical",
           major.ticks="ANY"
         )
)

setClass("chobTA",
         representation(
            call="call",
            on="ANY",
            new="logical",
            TA.values="ANY",
            name="character",
            params="ANY"
         )
)

setMethod("show","chobTA",
          function(object) {
            cat(paste("<chobTA object: ",object@call[[1]],">",sep=""),"\n") 
            invisible(object)
          }
)
