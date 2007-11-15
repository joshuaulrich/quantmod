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

`last.zoo` <-
function(x,n=1,...)
{
  if(is.character(n)) {
    # n period set
    np <- strsplit(n," ",fixed=TRUE)[[1]]
    if(length(np) > 2 || length(np) < 1)
      stop(paste("incorrectly specified",sQuote("n"),sep=" "))
    # series periodicity
    sp <- periodicity(x)
    # requested periodicity$units
    rpu <- np[length(np)]
    rpf <- ifelse(length(np) > 1, as.numeric(np[1]), 1)
    if(rpu == sp$unit) {
      n <- rpf    
    } else {
      u.list <- list(secs=4,seconds=4,mins=3,minutes=3,hours=2,days=1,
                     weeks=1,months=1,quarters=1,years=1)
      dt.options <- c('seconds','secs','minutes','mins','hours','days',
                      'weeks','months','quarters','years')
      if(!rpu %in% dt.options)
        stop(paste("n must be numeric or use",paste(dt.options,collapse=',')))
      dt <- dt.options[pmatch(rpu,dt.options)]
      if(u.list[[dt]] > u.list[[sp$unit]]) {
        #  req is for higher freq data period e.g. 100 mins of daily data
        stop(paste("At present, without some sort of magic, it isn't possible",
             "to resolve",rpu,"from",sp$scale,"data"))
      }
      bp <- breakpoints(x,dt,TRUE)
      if(rpf > length(bp)-1) rpf <- length(bp)-1
      n <- bp[length(bp)-rpf]+1
      return(x[n:NROW(x)])
    }
  }
  if(length(n) != 1) stop("n must be of length 1")
  if(n > 0) {
    x[((NROW(x)-n+1):NROW(x))]
  } else {
    x[1:(NROW(x)+n)]
  }
}
`first.zoo` <-
function(x,n=1,...)
{
  if(is.character(n)) {
    # n period set
    np <- strsplit(n," ",fixed=TRUE)[[1]]
    if(length(np) > 2 || length(np) < 1)
      stop(paste("incorrectly specified",sQuote("n"),sep=" "))
    # series periodicity
    sp <- periodicity(x)
    # requested periodicity$units
    rpu <- np[length(np)]
    rpf <- ifelse(length(np) > 1, as.numeric(np[1]), 1)
    if(rpu == sp$unit) {
      n <- rpf    
    } else {
      u.list <- list(secs=4,seconds=4,mins=3,minutes=3,hours=2,days=1,
                     weeks=1,months=1,quarters=1,years=1)
      dt.options <- c('seconds','secs','minutes','mins','hours','days',
                      'weeks','months','quarters','years')
      if(!rpu %in% dt.options)
        stop(paste("n must be numeric or use",paste(dt.options,collapse=',')))
      dt <- dt.options[pmatch(rpu,dt.options)]
      if(u.list[[dt]] > u.list[[sp$unit]]) {
        #  req is for higher freq data period e.g. 100 mins of daily data
        stop(paste("At present, without some sort of magic, it isn't possible",
             "to resolve",rpu,"from",sp$scale,"data"))
      }
      bp <- breakpoints(x,dt,TRUE)
      if(rpf > length(bp)-1) rpf <- length(bp)-1
      n <- bp[rpf+1]
      return(x[1:n])
    }
  }
  if(length(n) != 1) stop("n must be of length 1")
  if(n > 0) {
    x[1:n]
  } else {
    x[((NROW(x)-n+1):NROW(x)),]
  }
}
