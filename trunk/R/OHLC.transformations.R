"Op" <-
function(x)
{
  op <- grep('Open',colnames(x))
  if(!identical(op,integer(0)))
    return(x[,grep('Open',colnames(x))])
}

"Hi" <-
function(x)
{
  hi <- grep('High',colnames(x))
  if(!identical(hi,integer(0)))
    return(x[,grep('High',colnames(x))])
}

"Lo" <-
function(x)
{
  lo <- grep('Low',colnames(x))
  if(!identical(lo,integer(0)))
    return(x[,grep('Low',colnames(x))])
}

"Cl" <-
function(x)
{
  cl <- grep('Close',colnames(x))
  if(!identical(cl,integer(0)))
    return(x[,grep('Close',colnames(x))])
}

"Vo" <-
function(x)
{
  vo <- grep('Volume',colnames(x))
  if(!identical(vo,integer(0)))
    return(x[,grep('Volume',colnames(x))])
}

"Ad" <-
function(x)
{
  ad <- grep('Adjusted',colnames(x))
  if(!identical(ad,integer(0)))
    return(x[,grep('Adjusted',colnames(x))])
}

"OpCl" <-
function(x)
{
    xx <- Delt(Op(x),Cl(x))
    colnames(xx) <- paste("OpCl",deparse(substitute(x)),sep='.')
    xx
}

"OpOp" <-
function(x)
{
    xx <- Delt(Op(x))
    colnames(xx) <- paste("OpOp",deparse(substitute(x)),sep='.')
    xx
}

"ClCl" <-
function(x)
{
    xx <- Delt(Cl(x))
    colnames(xx) <- paste("ClCl",deparse(substitute(x)),sep='.')
    xx
}
"OpLo" <-
function(x)
{
    xx <- Delt(Op(x),Lo(x))
    colnames(xx) <- paste("OpLo",deparse(substitute(x)),sep='.')
    xx
}
"OpHi" <-
function(x)
{
    xx <- Delt(Op(x),Hi(x))
    colnames(xx) <- paste("OpHi",deparse(substitute(x)),sep='.')
    xx
}
"LoHi" <-
function(x)
{
    xx <- Delt(Lo(x),Hi(x))
    colnames(xx) <- paste("LoHi",deparse(substitute(x)),sep='.')
    xx
}
"LoCl" <-
function(x)
{
    xx <- Delt(Lo(x),Cl(x))
    colnames(xx) <- paste("LoCl",deparse(substitute(x)),sep='.')
    xx
}
"HiCl" <-
function(x)
{
    xx <- Delt(Hi(x),Cl(x))
    colnames(xx) <- paste("HiCl",deparse(substitute(x)),sep='.')
    xx
}
"Next" <-
function(x,k=1)
{
  UseMethod("Next")
}
"Next.data.frame" <-
function(x,k=1)
{
    if(k<0||k!=as.integer(k)||length(k)>1) stop("k must be a non-negative integer")
    if(k==0) return(x);
    new.x <- as.data.frame(c(x[-(0:k),],rep(NA,k)))
    rownames(new.x) <- rownames(x)
    colnames(new.x) <- "Next"
    return(new.x)
}
"Next.quantmod.OHLC" <-
function(x,k=1)
{
    if(k<0||k!=as.integer(k)||length(k)>1) stop("k must be a non-negative integer")
    if(k==0) return(x);
    new.x <- as.matrix(c(as.numeric(x[-(0:k),]),rep(NA,k)))
    x.index <- index(x)
    new.x <- zoo(new.x,x.index)
    colnames(new.x) <- "Next"
    return(new.x)
}
"Next.zoo" <- Next.quantmod.OHLC
"Next.numeric" <-
function(x,k=1)
{
    if(k<0||k!=as.integer(k)||length(k)>1) stop("k must be a non-negative integer")
    if(k==0) return(x);
    new.x <- as.matrix(c(as.numeric(x[-(0:k)]),rep(NA,k)))
    colnames(new.x) <- "Next"
    return(new.x)
}
"Lag" <-
function(x,k=1)
{
  UseMethod("Lag")
}
"Lag.data.frame"<-
function(x,k=1)
{
    new.x <- sapply(as.list(k), function(k.e) {
        if(k.e<0||k.e!=as.integer(k.e)) stop("k must be a non-negative integer")
        if(k.e==0) return(x);
        c(rep(NA,k.e),x[-((nrow(x)-k.e+1):nrow(x)),])
    }
    )
    rownames(new.x) <- rownames(x)
    colnames(new.x) <- paste("Lag.",k,sep="")
    return(new.x)
}
"Lag.quantmod.OHLC" <-
function(x,k=1)
{
    new.x <- sapply(as.list(k), function(k.e) {
        if(k.e<0||k.e!=as.integer(k.e)) stop("k must be a non-negative integer")
        if(k.e==0) return(x);
        c(rep(NA,k.e),x[-((length(x)-k.e+1):length(x))])
    }
    )
    x.index <- index(x)
    new.x <- zoo(new.x,x.index)
    colnames(new.x) <- paste("Lag.",k,sep="")
    return(new.x)
}
"Lag.zoo" <- Lag.quantmod.OHLC

"Lag.numeric" <-
function(x,k=1)
{
    new.x <- sapply(as.list(k), function(k.e) {
        if(k.e<0||k.e!=as.integer(k.e)) stop("k must be a non-negative integer")
        if(k.e==0) return(x);
        c(rep(NA,k.e),x[-((length(x)-k.e+1):length(x))])
    }
    )
    colnames(new.x) <- paste("Lag.",k,sep="")
    return(new.x)
}

"Lag.default"<-
function(x,k=1)
{
    if(is.character(x)) stop("x must be a time series or numeric vector")
    lag(x,k)
}

"Delt" <-
function(x1,x2=NULL,k=0,type=c('log','arithmetic'))
{
    if(length(k) > 1) stop("k must be a single integer value")
    type <- match.arg(type)[1]
    if(length(x2)!=length(x1) && !is.null(x2)) stop('x1 and x2 must be of same length');
    if(is.null(x2)){
        x2 <- x1 #copy for same symbol deltas
        if(length(k) < 2) {
            k <- max(1,k)
        }
    }
    if(type=='log') {
        xx <- log(x2/Lag(x1,k))
    } else {
        xx <- (x2-Lag(x1,k))/Lag(x1,k)
    }
    colnames(xx) <- paste("Delt",k,type,sep=".")
    xx
}
`to.period` <-
function(x,period=months,...)
{
  UseMethod('to.period')
}

`to.period.quantmod.OHLC` <-
function(x,period,...) {
  bp <- breakpoints(x,period,TRUE)
  date <- index(x)[bp]
  op <- as.numeric(Op(x))[bp+1][-length(bp)]
  hi <- period.apply(Hi(x),bp,max)
  lo <- period.apply(Lo(x),bp,min)
  cl <- as.numeric(Cl(x)[bp])
  vo <- NULL
  has.Vo <- grep('Volume',colnames(x))
  if(!identical(has.Vo,integer(0)))
    vo <- period.apply(Vo(x),bp,sum)

  ad <- NULL
  has.Ad <- grep('Adjusted',colnames(x))
  if(!identical(has.Ad,integer(0)))
    ad <- as.numeric(Ad(x)[bp])

  x.out <- zoo(cbind(op,hi,lo,cl,vo,ad),date)
  colnames(x.out) <- colnames(x)
  class(x.out) <- class(x)
  x.out
}

`to.period.zoo` <-
function(x,period,name=NULL,...)
{
  if(is.null(name)) name <- deparse(substitute(x))
  if(NCOL(x)==1) {
    # for single dimension (or no?) data 
    # original breakpoints(x,period,TRUE)
    bp <- breakpoints(x,period,abbreviate=TRUE,...)
    date <- index(x)[bp]
    x.out <- period.apply(x,bp,
               function(k) c(as.numeric(first(k)),
                             max(k),min(k),as.numeric(last(k))))
    x.zoo <- zoo(matrix(x.out,ncol=4,byrow=TRUE),date)
    colnames(x.zoo) <- paste(name,c("Open","High","Low","Close"),sep='.')
    class(x.zoo) <- c('quantmod.OHLC','zoo')
    x.zoo
  }
  else {
    stop("'x' must be a single column or of class 'quantmod.OHLC'")
  }
}
`to.minutes` <-
function(x)
{
  name <- deparse(substitute(x))
  to.period(x,minutes,name)
}
`to.minutes3` <-
function(x)
{
  name <- deparse(substitute(x))
  to.period(x,minutes3,name)
}
`to.minutes5` <-
function(x)
{
  name <- deparse(substitute(x))
  to.period(x,minutes5,name)
}
`to.minutes10` <-
function(x)
{
  name <- deparse(substitute(x))
  to.period(x,minutes10,name)
}
`to.minutes15` <-
function(x)
{
  name <- deparse(substitute(x))
  to.period(x,minutes15,name)
}
`to.minutes30` <-
function(x)
{
  name <- deparse(substitute(x))
  to.period(x,minutes30,name)
}
`to.hourly` <-
function(x)
{
  name <- deparse(substitute(x))
  to.period(x,hours,name)
}
`to.daily` <-
function(x,drop.time=TRUE)
{
  name <- deparse(substitute(x))
  x <- to.period(x,days,name)
  if(drop.time) 
    index(x) <- as.Date(index(x))
  return(x)
}
`to.weekly` <-
function(x,drop.time=TRUE)
{
  name <- deparse(substitute(x))
  x <- to.period(x,weeks,name)
  if(drop.time) 
    index(x) <- as.Date(index(x))
  return(x)
}
`to.monthly` <-
function(x,drop.time=TRUE)
{
  name <- deparse(substitute(x))
  x <- to.period(x,months,name)
  if(drop.time) 
    index(x) <- as.Date(index(x))
  return(x)
}
`to.quarterly` <-
function(x,drop.time=TRUE)
{
  name <- deparse(substitute(x))
  x <- to.period(x,quarters,name)
  if(drop.time) 
    index(x) <- as.Date(index(x))
  return(x)
}
`to.yearly` <-
function(x,drop.time=TRUE)
{
  name <- deparse(substitute(x))
  x <- to.period(x,years,name)
  if(drop.time) 
    index(x) <- as.Date(index(x))
  return(x)
}
