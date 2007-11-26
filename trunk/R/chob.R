`.chob` <- list()

`write.chob` <-
function(x)
{
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

`get.chob` <-
function()
{
  x <- get('.chob',as.environment("package:quantmod"))
  attr(x,'.Environment') <- NULL
  return(x)
}
