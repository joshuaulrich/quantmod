`qa.f` <-
function(x,by)
{
  bp <- breakpoints(x,by=by,TRUE)
  ndx <- as.double(as.matrix(x))
  hasvol <- hasadj <- ifelse(NCOL(x)==1,0,1)
  q <- .Fortran('toperiod',bp=as.integer(bp),lbp=as.integer(length(bp)),
                ia=as.double(ndx),lia=as.integer(length(ndx)),
                nri=as.integer(NROW(x)),nci=as.integer(NCOL(x)),
                hasvol=as.integer(hasvol),hasadj=as.integer(hasadj),
                ret=as.double(rep(0,(length(bp)-1)*(NCOL(x))))
                ,PACKAGE='quantmod')
  matrix(q$ret,nc=(4+hasvol+hasadj),byrow=TRUE)
}

`ohlcz` <- 
function(x,by)
{
  if(NCOL(x) > 1) stop("single col data only")
  name <- deparse(substitute(x))
  bp <- breakpoints(x,by=by,TRUE)
  ndx <- as.double(as.matrix(x))
  q <- .Fortran('ohlcz',bp=as.integer(bp),lbp=as.integer(length(bp)),
                ia=as.double(ndx),lia=as.integer(length(ndx)),
                nri=as.integer(NROW(x)),nci=as.integer(NCOL(x)),
                ret=as.double(rep(0,(length(bp)-1)*4))
                ,PACKAGE='quantmod')
  tz <- zoo(matrix(q$ret,nc=4,byrow=TRUE),index(x)[bp[-1]])
  colnames(tz)=c("Open","High","Low","Close")
  class(tz) <- c("quantmod.OHLC","zoo")
  tz
}
