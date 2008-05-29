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
`is.HLC` <-
function(x)
{
  all(has.Hi(x),has.Lo(x),has.Cl(x))
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
`has.HLC` <-
function(x,which=FALSE)
{
  if(which) {
    c(has.Hi(x,1),has.Lo(x,1),has.Cl(x,1))
  } else {
    c(has.Hi(x),has.Lo(x),has.Cl(x))
  }
}
`HLC` <-
function(x)
{
  if(is.HLC(x))
    return(x[,has.HLC(x,1)])
  NULL
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
    if(inherits(x,'xts')) {
      new.x <- xts(new.x,x.index)
    } else {
      new.x <- zoo(new.x,x.index)
    }
    dim(new.x) <- c(NROW(new.x),max(k,1))
    colnames(new.x) <- paste("Lag.",k,sep="")
    return(new.x)
}
`Lag.zoo` <- `Lag.xts` <- Lag.quantmod.OHLC

`Lag.numeric` <-
function(x,k=1)
{
    new.x <- sapply(as.list(k), function(k.e) {
        if(k.e<0||k.e!=as.integer(k.e)) stop("k must be a non-negative integer")
        if(k.e==0) return(x);
        c(rep(NA,k.e),x[-((length(x)-k.e+1):length(x))])
    }
    )
 #   dim(new.x) <- c(NROW(new.x),max(k,1))
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
    x1 <- try.xts(x1, error=FALSE)
    type <- pmatch(type[1],c('log','arithmetic'))
    if(length(x2)!=length(x1) && !is.null(x2)) stop('x1 and x2 must be of same length');
    if(is.null(x2)){
        x2 <- x1 #copy for same symbol deltas
        if(length(k) < 2) {
            k <- max(1,k)
        }
    }
    dim(x2) <- NULL  # allow for multiple k matrix math to happen
    if(is.zoo(x1)) x1 <- as.matrix(x1)
    if(type==1) {
        xx <- log(x2/Lag(x1,k))
    } else {
        xx <- (x2-Lag(x1,k))/Lag(x1,k)
    }
    colnames(xx) <- paste("Delt",k,type,sep=".")
    reclass(xx,x1)
}
