"lags2" <-
function(x,k=1) { sapply(as.list(k), function(k.e) {
  if(k.e < 0 || k.e != as.integer(k.e))
    stop("k must be a non-negative integer")
  if(k.e == 0)
    return(x)
  c(rep(NA,k.e),x[-((length(x) - k.e + 1):length(x))])
  })
}

