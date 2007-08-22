"Op" <-
function(x)
{
    return(x[,grep('Open',colnames(x))])
}

"Hi" <-
function(x)
{
    return(x[,grep('High',colnames(x))])
}

"Lo" <-
function(x)
{
    return(x[,grep('Low',colnames(x))])
}

"Cl" <-
function(x)
{
    return(x[,grep('Close',colnames(x))])
}

"Vo" <-
function(x)
{
    return(x[,grep('Volume',colnames(x))])
}

"Ad" <-
function(x)
{
    return(x[,grep('Adjusted',colnames(x))])
}

"OpCl" <-
function(x)
{
    return(quantmod::Delt(Op(x),Cl(x)))
}

"OpOp" <-
function(x)
{
    return(quantmod::Delt(Op(x)))
}

"ClCl" <-
function(x)
{
    return(quantmod::Delt(Cl(x)))
}
"OpLo" <-
function(x)
{
    return(quantmod::Delt(Op(x),Lo(x)))
}
"OpHi" <-
function(x)
{
    return(quantmod::Delt(Op(x),Hi(x)))
}
"LoHi" <-
function(x)
{
    return(quantmod::Delt(Lo(x),Hi(x)))
}
"LoCl" <-
function(x)
{
    return(quantmod::Delt(Lo(x),Cl(x)))
}
"HiCl" <-
function(x)
{
    return(quantmod::Delt(Hi(x),Cl(x)))
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
    type <- match.arg(type)[1]
    if(length(x2)!=length(x1) && !is.null(x2)) stop('x1 and x2 must be of same length');
    if(is.null(x2)){
        x2 <- x1 #copy for same symbol deltas
        if(length(k) < 2) {
            k <- max(1,k)
        }
    }
    if(type=='log') {
        log(x2/Lag(x1,k))
    } else {
        (x2-Lag(x1,k))/Lag(x1,k)
    }
}
`to.period` <-
function(x,period=months)
{
  UseMethod('to.period')
}

`to.period.quantmod.OHLC` <-
function(x,period=months) {
  bp <- breakpoints(x,period,TRUE)
  op <- as.numeric(Op(x)[bp+1][-length(bp)])
  hi <- period.apply(Hi(x),bp,max)
  lo <- period.apply(Lo(x),bp,min)
  cl <- as.numeric(Cl(x)[bp])
  vo <- period.apply(Vo(x),bp,sum)
  ad <- as.numeric(Ad(x)[bp])
  date <- index(x)[bp]
  x.out <- zoo(cbind(op,hi,lo,cl,vo,ad),date)
  colnames(x.out) <- colnames(x)
  x.out
}

`to.period.zoo` <- to.period.quantmod.OHLC
