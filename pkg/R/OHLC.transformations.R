`seriesHi` <-
function(x) {
  UseMethod("seriesHi")
}

`seriesHi.default` <-
function(x) {
  if(!is.null(dim(x)[2])) {
    if(dim(x)[2]==1) {
      # a univariate series - non-numeric
      return(x[which(max(x)==as.numeric(x))])
    } else {
      # a multivariate series
      return(x[which(max(Hi(x))==as.numeric(Hi(x)))])
    }    
  }   
  # a numeric vector
  max(x,na.rm=TRUE)
}

`seriesHi.timeSeries` <-
function(x) {
  x.Data <- x@Data
  if(!is.null(dim(x)[2])) {
    if(dim(x)[2]==1) {
      #univariate timeSeries
      return(x[which(max(as.numeric(x.Data))==as.numeric(x.Data))])
    } else {
      #multivariate timeSeries
      return(x[which(max(as.numeric(Hi(x)@Data))==as.numeric(Hi(x)@Data))])
    }
  }
}

`seriesHi.ts` <-
function(x) {
  if(!is.null(dim(x)[2])) {
    return(x[which(max(Hi(x),na.rm=TRUE)==Hi(x)),])
  }   
  # a numeric vector
  max(x,na.rm=TRUE)
}
`seriesLo` <-
function(x) {
  UseMethod("seriesLo")
}

`seriesLo.default` <-
function(x) {
  if(!is.null(dim(x)[2])) {
    if(dim(x)[2]==1) {
      # a univariate series - non-numeric
      return(x[which(min(x)==as.numeric(x))])
    } else {
      # a multivariate series
      return(x[which(min(Lo(x))==as.numeric(Lo(x)))])
    }    
  }   
  # a numeric vector
  min(x,na.rm=TRUE)
}
`seriesLo.timeSeries` <-
function(x) {
  x.Data <- x@Data
  if(!is.null(dim(x)[2])) {
    if(dim(x)[2]==1) {
      #univariate timeSeries
      return(x[which(min(as.numeric(x.Data))==as.numeric(x.Data))])
    } else {
      #multivariate timeSeries
      return(x[which(min(as.numeric(Lo(x)@Data))==as.numeric(Lo(x)@Data))])
    }
  }
}

`seriesLo.ts` <-
function(x) {
  if(!is.null(dim(x)[2])) {
    return(x[which(min(Lo(x),na.rm=TRUE)==Lo(x)),])
  }   
  # a numeric vector
  min(x,na.rm=TRUE)
}
`is.OHLC` <-
function(x)
{
  all(has.Op(x),has.Hi(x),has.Lo(x),has.Cl(x))
}
`has.OHLC` <-
function(x,which=FALSE)
{
  if(which) {
    c(has.Op(x,1),has.Hi(x,1),has.Lo(x,1),has.Cl(x,1))
  } else {
    c(has.Op(x),has.Hi(x),has.Lo(x),has.Cl(x))
  }
}

`Op` <-
function(x)
{
  if(has.Op(x))
    return(x[,grep('Open',colnames(x))])
  NULL
}

`has.Op` <-
function(x,which=FALSE)
{
  loc <- grep('Open',colnames(x))
  if(!identical(loc,integer(0)))
    return(ifelse(which,loc,TRUE))
  ifelse(which,loc,FALSE)
}

`Hi` <-
function(x)
{
  if(has.Hi(x))
    return(x[,grep('High',colnames(x))])
  NULL
}

`has.Hi` <-
function(x,which=FALSE)
{
  loc <- grep('High',colnames(x))
  if(!identical(loc,integer(0)))
    return(ifelse(which,loc,TRUE))
  ifelse(which,loc,FALSE)
}

`Lo` <-
function(x)
{
  if(has.Lo(x))
    return(x[,grep('Low',colnames(x))])
  NULL
}

`has.Lo` <-
function(x,which=FALSE)
{
  loc <- grep('Low',colnames(x))
  if(!identical(loc,integer(0)))
    return(ifelse(which,loc,TRUE))
  ifelse(which,loc,FALSE)
}

`Cl` <-
function(x)
{
  if(has.Cl(x))
    return(x[,grep('Close',colnames(x))])
  NULL
}
`has.Cl` <-
function(x,which=FALSE)
{
  loc <- grep('Close',colnames(x))
  if(!identical(loc,integer(0)))
    return(ifelse(which,loc,TRUE))
  ifelse(which,loc,FALSE)
}

`Vo` <-
function(x)
{
  #vo <- grep('Volume',colnames(x))
  #if(!identical(vo,integer(0)))
  if(has.Vo(x))
    return(x[,grep('Volume',colnames(x))])
  NULL
}
`has.Vo` <-
function(x,which=FALSE)
{
  loc <- grep('Volume',colnames(x))
  if(!identical(loc,integer(0)))
    return(ifelse(which,loc,TRUE))
  ifelse(which,loc,FALSE)
}

`Ad` <-
function(x)
{
  if(has.Ad(x))
    return(x[,grep('Adjusted',colnames(x))])
  NULL
}
`has.Ad` <-
function(x,which=FALSE)
{
  loc <- grep('Adjusted',colnames(x))
  if(!identical(loc,integer(0)))
    return(ifelse(which,loc,TRUE))
  ifelse(which,loc,FALSE)
}

`OpCl` <-
function(x)
{
    xx <- Delt(Op(x),Cl(x))
    colnames(xx) <- paste("OpCl",deparse(substitute(x)),sep='.')
    xx
}

`OpOp` <-
function(x)
{
    xx <- Delt(Op(x))
    colnames(xx) <- paste("OpOp",deparse(substitute(x)),sep='.')
    xx
}

`ClCl` <-
function(x)
{
    xx <- Delt(Cl(x))
    colnames(xx) <- paste("ClCl",deparse(substitute(x)),sep='.')
    xx
}
`OpLo` <-
function(x)
{
    xx <- Delt(Op(x),Lo(x))
    colnames(xx) <- paste("OpLo",deparse(substitute(x)),sep='.')
    xx
}
`OpHi` <-
function(x)
{
    xx <- Delt(Op(x),Hi(x))
    colnames(xx) <- paste("OpHi",deparse(substitute(x)),sep='.')
    xx
}
`LoHi` <-
function(x)
{
    xx <- Delt(Lo(x),Hi(x))
    colnames(xx) <- paste("LoHi",deparse(substitute(x)),sep='.')
    xx
}
`LoCl` <-
function(x)
{
    xx <- Delt(Lo(x),Cl(x))
    colnames(xx) <- paste("LoCl",deparse(substitute(x)),sep='.')
    xx
}
`HiCl` <-
function(x)
{
    xx <- Delt(Hi(x),Cl(x))
    colnames(xx) <- paste("HiCl",deparse(substitute(x)),sep='.')
    xx
}
`Next` <-
function(x,k=1)
{
  UseMethod("Next")
}
`Next.data.frame` <-
function(x,k=1)
{
    if(k<0||k!=as.integer(k)||length(k)>1) stop("k must be a non-negative integer")
    if(k==0) return(x);
    new.x <- as.data.frame(c(x[-(0:k),],rep(NA,k)))
    rownames(new.x) <- rownames(x)
    colnames(new.x) <- "Next"
    return(new.x)
}
`Next.quantmod.OHLC` <-
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
`Next.zoo` <- Next.quantmod.OHLC
`Next.numeric` <-
function(x,k=1)
{
    if(k<0||k!=as.integer(k)||length(k)>1) stop("k must be a non-negative integer")
    if(k==0) return(x);
    new.x <- as.matrix(c(as.numeric(x[-(0:k)]),rep(NA,k)))
    colnames(new.x) <- "Next"
    return(new.x)
}
`Lag` <-
function(x,k=1)
{
  UseMethod("Lag")
}
`Lag.data.frame`<-
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
`Lag.quantmod.OHLC` <-
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
`Lag.zoo` <- Lag.quantmod.OHLC

`Lag.numeric` <-
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

`Lag.default`<-
function(x,k=1)
{
    if(is.character(x)) stop("x must be a time series or numeric vector")
    lag(x,k)
}

`Delt` <-
function(x1,x2=NULL,k=0,type=c('log','arithmetic'))
{
    type <- pmatch(type[1],c('log','arithmetic'))
    if(length(x2)!=length(x1) && !is.null(x2)) stop('x1 and x2 must be of same length');
    if(is.null(x2)){
        x2 <- x1 #copy for same symbol deltas
        if(length(k) < 2) {
            k <- max(1,k)
        }
    }
    dim(x2) <- NULL  # allow for multiple k matrix math to happen
    if(type==1) {
        xx <- log(x2/Lag(x1,k))
    } else {
        xx <- (x2-Lag(x1,k))/Lag(x1,k)
    }
    colnames(xx) <- paste("Delt",k,type,sep=".")
    xx
}
`to.period` <-
function(x,period=months,name,...)
{
  UseMethod('to.period')
}

`to.period.timeSeries` <-
function(x,period,name,...) {
  if(missing(name)) name <- deparse(substitute(x))
  bp <- breakpoints(x,period,TRUE)
  date <- rownames(x)[bp]
  op <- as.numeric(seriesData(Op(x)))[bp+1][-length(bp)]
  hi <- period.apply(seriesData(Hi(x)),bp,max)
  lo <- period.apply(seriesData(Lo(x)),bp,min)
  cl <- as.numeric(seriesData(Cl(x))[bp])
  vo <- NULL
  has.Vo <- grep('Volume',colnames(x))
  if(!identical(has.Vo,integer(0)))
    vo <- period.apply(seriesData(Vo(x)),bp,sum)

  ad <- NULL
  has.Ad <- grep('Adjusted',colnames(x))
  if(!identical(has.Ad,integer(0)))
    ad <- as.numeric(seriesData(Ad(x))[bp])

  x.out <- zoo(cbind(op,hi,lo,cl,vo,ad),date)
  colnames(x.out) <- colnames(x)
  as.timeSeries(x.out)
}

`to.period.quantmod.OHLC` <-
function(x,period,name,...) {
  bp <- breakpoints(x,period,TRUE)

  tz <- as.double(as.matrix(x))
  hasvol <- ifelse(has.Vo(x), 1, 0)
  hasadj <- ifelse(has.Ad(x), 1, 0)
  q <- .Fortran("ohlcq", bp = as.integer(bp), lbp = as.integer(length(bp)), 
      ia = as.double(tz), lia = as.integer(length(tz)), nri = as.integer(NROW(x)), 
      hasvol = as.integer(hasvol), 
      hasadj = as.integer(hasadj), ret = as.double(rep(0, (length(bp) - 
          1) * (NCOL(x)))), PACKAGE = "quantmod")
  tz <- zoo(matrix(q$ret, nc = (4 + hasvol + hasadj), byrow = TRUE), 
      index(x)[bp[-1]])
  cnames <- c("Open", "High", "Low", "Close")
  if (hasvol == 1) 
      cnames <- c(cnames, "Volume")
  if (hasadj == 1) 
      cnames <- c(cnames, "Adjusted")
  if(missing(name)) name <- deparse(substitute(x))
  tz <- as.quantmod.OHLC(tz, col.names = cnames, name = name)
  tz

#    date <- index(x)[bp]
#    op <- as.numeric(Op(x))[bp+1][-length(bp)]
#    hi <- period.apply(Hi(x),bp,max)
#    lo <- period.apply(Lo(x),bp,min)
#    cl <- as.numeric(Cl(x)[bp])
#    vo <- NULL
#    has.Vo <- grep('Volume',colnames(x))
#    if(!identical(has.Vo,integer(0)))
#      vo <- period.apply(Vo(x),bp,sum)
#  
#    ad <- NULL
#    has.Ad <- grep('Adjusted',colnames(x))
#    if(!identical(has.Ad,integer(0)))
#      ad <- as.numeric(Ad(x)[bp])
#  
#    x.out <- zoo(cbind(op,hi,lo,cl,vo,ad),date)
#    colnames(x.out) <- colnames(x)
#    class(x.out) <- class(x)
#    x.out
}

`to.period.zoo` <-
function(x,period,name=NULL,...)
{
  if(is.null(name)) name <- deparse(substitute(x))
  if(is.OHLC(x)) {
    return(to.period.quantmod.OHLC(x=x,period=period,name=name,...))
  }
  if(NCOL(x)==1) {
    #### for single dimension (or no?) data 
    #### original breakpoints(x,period,TRUE)
    #bp <- breakpoints(x,period,abbreviate=TRUE,...)
    #date <- index(x)[bp]
    #x.out <- period.apply(x,bp,
    #           function(k) c(as.numeric(first(k)),
    #                         max(k),min(k),as.numeric(last(k))))
    #x.zoo <- zoo(matrix(x.out,ncol=4,byrow=TRUE),date)
    #colnames(x.zoo) <- paste(name,c("Open","High","Low","Close"),sep='.')
    #class(x.zoo) <- c('quantmod.OHLC','zoo')
    #x.zoo

    bp <- breakpoints(x, period, TRUE)
    tz <- as.double(as.matrix(x))
    q <- .Fortran("ohlcz", bp = as.integer(bp), lbp = as.integer(length(bp)), 
        ia = as.double(tz), lia = as.integer(length(tz)), 
        ret = as.double(rep(0, (length(bp) - 1) * 4)), PACKAGE = "quantmod")
    tz <- zoo(matrix(q$ret, nc = 4, byrow = TRUE), index(x)[bp[-1]])
    colnames(tz) = c("Open", "High", "Low", "Close")
    class(tz) <- c("quantmod.OHLC", "zoo")
    tz

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
  if(drop.time) x <- .drop.time(x)
  return(x)
}
`to.weekly` <-
function(x,drop.time=TRUE)
{
  name <- deparse(substitute(x))
  x <- to.period(x,weeks,name)
  if(drop.time) x <- .drop.time(x)
  return(x)
}
`to.monthly` <-
function(x,drop.time=TRUE)
{
  name <- deparse(substitute(x))
  x <- to.period(x,months,name)
  if(drop.time) x <- .drop.time(x)
  return(x)
}
`to.quarterly` <-
function(x,drop.time=TRUE)
{
  name <- deparse(substitute(x))
  x <- to.period(x,quarters,name)
  if(drop.time) x <- .drop.time(x)
  return(x)
}
`to.yearly` <-
function(x,drop.time=TRUE)
{
  name <- deparse(substitute(x))
  x <- to.period(x,years,name)
  if(drop.time) x <- .drop.time(x)
  return(x)
}
`.drop.time` <-
function(x) {
  # function to remove HHMMSS portion of time index
  if("timeSeries" %in% class(x)) {
    if("package:fSeries" %in% search() || require("fSeries",
        quietly=TRUE)) {
      x <- as.timeSeries(zoo(seriesData(x),as.Date(rownames(x),origin='1970-01-01')))
    }
  } else {
    index(x) <- as.Date(index(x),origin='1970-01-01')
  }
  return(x)
}
