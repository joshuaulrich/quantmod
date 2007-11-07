`period.apply0` <-
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
`period.apply` <-
function(x,INDEX,FUN,...)
{
  FUN <- match.fun(FUN)
  sapply(1:(length(INDEX)-1),
    function(y) {
      FUN(x[(INDEX[y]+1):INDEX[y+1]],...)
    })
}
`apply.weekly` <-
function(x,FUN)
{
  bp <- breakpoints(x,weeks)
  period.apply(x,bp,FUN)
}

`apply.monthly` <-
function(x,FUN)
{
  bp <- breakpoints(x,months)
  period.apply(x,bp,FUN)
}

`apply.quarterly` <-
function(x,FUN)
{
  bp <- breakpoints(x,quarters)
  period.apply(x,bp,FUN)
}

`apply.yearly` <-
function(x,FUN)
{
  bp <- breakpoints(x,years)
  period.apply(x,bp,FUN)
}

`first` <-
function(x,...)
{
  UseMethod("first")
}

`first.default` <-
function(x,...)
{
  if(is.null(dim(x))) {
    x[1]
  } else {
    x[1,]
  }
}

`last` <-
function(x,...)
{
  UseMethod("last")
}

`last.default` <-
function(x,...)
{
  if(is.null(dim(x))) {
    x[NROW(x)]
  } else {
    x[NROW(x),]
  }
}

