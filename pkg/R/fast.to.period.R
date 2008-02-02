#`qa.f` <-
#function(x,by)
#{
#  bp <- breakpoints(x,by=by,TRUE)
#  ndx <- as.double(as.matrix(x))
#  hasvol <- hasadj <- ifelse(NCOL(x)==1,0,1)
#  q <- .Fortran('toperiod',bp=as.integer(bp),lbp=as.integer(length(bp)),
#                ia=as.double(ndx),lia=as.integer(length(ndx)),
#                nri=as.integer(NROW(x)),nci=as.integer(NCOL(x)),
#                hasvol=as.integer(hasvol),hasadj=as.integer(hasadj),
#                ret=as.double(rep(0,(length(bp)-1)*(NCOL(x))))
#                ,PACKAGE='quantmod')
#  matrix(q$ret,nc=(4+hasvol+hasadj),byrow=TRUE)
#}
#
`period.sum` <-
function(x,INDEX) {
  if(NCOL(x) > 1) stop("single col data only")
  bp <- INDEX
  xx <- as.double(as.matrix(x))
  q <- .Fortran('psumz',bp=as.integer(bp),lbp=as.integer(length(bp)),
                ia=as.double(xx),lia=as.integer(length(xx)),
                ret=as.double(rep(0,(length(bp)-1)))
                ,PACKAGE='quantmod')
  tz <- zoo(matrix(q$ret,nc=1,byrow=TRUE),index(x)[bp[-1]])
  tz

}
`period.prod` <-
function(x,INDEX) {
  if(NCOL(x) > 1) stop("single col data only")
  bp <- INDEX
  xx <- as.double(as.matrix(x))
  q <- .Fortran('pprodz',bp=as.integer(bp),lbp=as.integer(length(bp)),
                ia=as.double(xx),lia=as.integer(length(xx)),
                ret=as.double(rep(0,(length(bp)-1)))
                ,PACKAGE='quantmod')
  tz <- zoo(matrix(q$ret,nc=1,byrow=TRUE),index(x)[bp[-1]])
  tz
}
`period.max` <-
function(x,INDEX) {
  if(NCOL(x) > 1) stop("single col data only")
  bp <- INDEX
  xx <- as.double(as.matrix(x))
  q <- .Fortran('pmaxz',bp=as.integer(bp),lbp=as.integer(length(bp)),
                ia=as.double(xx),lia=as.integer(length(xx)),
                ret=as.double(rep(0,(length(bp)-1)))
                ,PACKAGE='quantmod')
  tz <- zoo(matrix(q$ret,nc=1,byrow=TRUE),index(x)[bp[-1]])
  tz
}
`period.min` <-
function(x,INDEX) {
  if(NCOL(x) > 1) stop("single col data only")
  bp <- INDEX
  xx <- as.double(as.matrix(x))
  q <- .Fortran('pminz',bp=as.integer(bp),lbp=as.integer(length(bp)),
                ia=as.double(xx),lia=as.integer(length(xx)),
                ret=as.double(rep(0,(length(bp)-1)))
                ,PACKAGE='quantmod')
  tz <- zoo(matrix(q$ret,nc=1,byrow=TRUE),index(x)[bp[-1]])
  tz
}

`ohlcq` <-
function(x,by)
{
  bp <- breakpoints(x,by=by,TRUE)
  xx <- as.double(as.matrix(x))
  #hasvol <- ifelse(NCOL(x)>=5,1,0)
  #hasadj <- ifelse(NCOL(x)==6,1,0)
  hasvol <- has.Vo(x)
  hasadj <- has.Ad(x)
  q <- .Fortran('ohlcq',bp=as.integer(bp),lbp=as.integer(length(bp)),
                ia=as.double(xx),lia=as.integer(length(xx)),
                nri=as.integer(NROW(x)),
                hasvol=as.integer(hasvol),hasadj=as.integer(hasadj),
                ret=as.double(rep(0,(length(bp)-1)*(NCOL(x))))
                ,PACKAGE='quantmod')
  #matrix(q$ret,nc=(4+hasvol+hasadj),byrow=TRUE)
  tz <- zoo(matrix(q$ret,nc=(4+hasvol+hasadj),byrow=TRUE),index(x)[bp[-1]])
  cnames <- c("Open","High","Low","Close")
  if(hasvol==1) cnames <- c(cnames,"Volume")
  if(hasadj==1) cnames <- c(cnames,"Adjusted")
  name <- deparse(substitute(x))
  tz <- as.quantmod.OHLC(tz,col.names=cnames,name=name)
  tz
}

`ohlcz` <- 
function(x,by)
{
  if(NCOL(x) > 1) stop("single col data only")
  name <- deparse(substitute(x))
  bp <- breakpoints(x,by=by,TRUE)
  xx <- as.double(as.matrix(x))
  q <- .Fortran('ohlcz',bp=as.integer(bp),lbp=as.integer(length(bp)),
                ia=as.double(xx),lia=as.integer(length(xx)),
                ret=as.double(rep(0,(length(bp)-1)*4))
                ,PACKAGE='quantmod')
  tz <- zoo(matrix(q$ret,nc=4,byrow=TRUE),index(x)[bp[-1]])
  colnames(tz)=c("Open","High","Low","Close")
  class(tz) <- c("quantmod.OHLC","zoo")
  tz
}
