`qa.f` <-
function(x,by)
{
  bp <- breakpoints(x,by=by,TRUE)
  ndx <- as.double(as.matrix(x[,1:4]))
  q <- .Fortran('toperiod',bp=as.integer(bp),lbp=as.integer(length(bp)),
                ia=as.double(ndx),lia=as.integer(length(ndx)),
                nri=as.integer(NROW(x)),nci=as.integer(NCOL(x[,1:4])),
                ret=as.double(rep(0,(length(bp)-1)*(NCOL(x[,1:4]))))
                ,PACKAGE='quantmod')
  matrix(q$ret,nc=4,byrow=TRUE)
}

