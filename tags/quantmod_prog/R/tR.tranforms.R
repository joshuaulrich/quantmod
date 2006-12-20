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
    return(quantmod::Delta(Op(x),Cl(x)))
}

"OpOp" <-
function(x)
{
    return(quantmod::Delta(Op(x)))
}

"ClCl" <-
function(x)
{
    return(quantmod::Delta(Cl(x)))
}
"OpLo" <-
function(x)
{
    return(quantmod::Delta(Op(x),Lo(x)))
}
"OpHi" <-
function(x)
{
    return(quantmod::Delta(Op(x),Hi(x)))
}
"LoHi" <-
function(x)
{
    return(quantmod::Delta(Lo(x),Hi(x)))
}
"LoCl" <-
function(x)
{
    return(quantmod::Delta(Lo(x),Cl(x)))
}
"HiCl" <-
function(x)
{
    return(quantmod::Delta(Hi(x),Cl(x)))
}
"Next" <-
function(x,k=1)
{
    if(k<0||k!=as.integer(k)) stop("k must be a non-negative integer")
    if(k==0) return(x);
    c(x[-c(0:k)],rep(NA,k))
}

"Lag" <-
function(x,k=1)
{
    sapply(as.list(k), function(k.e) {
        if(k.e<0||k.e!=as.integer(k.e)) stop("k must be a non-negative integer")
        if(k.e==0) return(x);
        c(rep(NA,k.e),x[-((length(x)-k.e+1):length(x))])
    }
    )
}

"Delta" <-
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
