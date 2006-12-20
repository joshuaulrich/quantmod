"papply" <-
function(x,INDEX,FUN,...)
{
  FUN <- match.fun(FUN);
  y <- NULL;
  for(i in 1:(length(INDEX)-1))
  {
    sindex <- (INDEX[i]+1):INDEX[i+1];
    dat <- x[sindex];
    y <- c(y,FUN(dat,...));
  }
  return(y);
}
