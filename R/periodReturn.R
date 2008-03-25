`periodReturn.quantmod.OHLC` <-
function(x,by=months,from=NULL,to=NULL) {
  if(identical(grep('Adjusted',colnames(x)),integer(0))) {
    Ad <- quantmod::Cl
  } else {
    Ad <- quantmod::Ad
  }
  if(is.null(from)) from <- start(as.zoo(x))
  if(is.null(to)) to <- end(as.zoo(x))
  x.period <- x[breakpoints(as.zoo(x),by=by,TRUE),]
  adj.length <- NROW(x.period)
  if(adj.length==1) x.period <- x[breakpoints(as.zoo(x),by=by,TRUE)[-1],]
  adj.x.period <- as.numeric(Ad(x.period))
  adj.start <- c(as.numeric(Ad(x))[1],adj.x.period[-adj.length])
  returns <- ((adj.x.period - adj.start)/adj.start)
  returns <- zoo(returns,index(as.zoo(x.period)))
  returns <- subset(returns,index(returns) >= from & index(returns) <= to)
  class(returns) <- c('periodReturn','zoo')
  return(returns)
}
`periodReturn.zoo` <-
function(x,by=months,from=NULL,to=NULL) {
  if(NCOL(x) > 1)
    stop(paste(sQuote('x'),'must be a univariate',dQuote('zoo'),'object'))
  from.col <- 1
  if(is.null(from)) from <- start(as.zoo(x))
  if(is.null(to)) to <- end(as.zoo(x))
  x.period <- x[breakpoints(as.zoo(x),by=by,TRUE),]
  adj.length <- NROW(x.period)
  adj.x.period <- as.numeric(x.period[,1])
  adj.start <- c(as.numeric(x[,from.col])[1],x.period[-adj.length])
  returns <- ((adj.x.period - adj.start)/adj.start)
  returns <- zoo(returns,index(as.zoo(x.period)))
  returns <- subset(returns,index(returns) >= from & index(returns) <= to)
  class(returns) <- c('periodReturn','zoo')
  return(returns)
}
`periodReturn.quantmodResults` <-
function(x,by=months,from=NULL,to=NULL)
{
  x <- x@return@returns
  periodReturn(x,by=by,from=from,to=to)
}
`periodReturn.old` <-
function(x,by=months,from=NULL,to=NULL) 
{
    UseMethod("periodReturn");
}

`periodReturn` <-
function(x,period='monthly',subset=NULL,type='arithmetic',...) {
  xx <- x
  if(is.null(subset)) subset <- '::'
  x <- as.xts(x)
  .originalCLASS <- CLASS(x)
  .originalIndexClass <- indexClass(x)
  .originalAttr <- xtsAttributes(x)

  FUN = eval(parse(text=paste('xts::to',period,sep='.'))) 
  x <- FUN(x, ...)
  if(has.Cl(x)) {
    x <- Delt(Cl(x),type=type)
  } else {
    x <- Delt(x[,4],type=type)
  }
  colnames(x) <- paste(period,'returns',sep='.')
  x <- as.xts(x)[subset]
  CLASS(x) <- .originalCLASS
  xtsAttributes(x) <- .originalAttr
  # this is required for its and ts, should make conditional
  # so as to preserve yearmon/yearqtr
  indexClass(x) <- .originalIndexClass
  reclass(x)
}

`dailyReturn` <-
function(x,subset=NULL,type='arithmetic',...) {
  periodReturn(x,'daily',subset,type,...)
}

`monthlyReturn` <-
function(x,subset=NULL,type='arithmetic',...) {
  periodReturn(x,'monthly',subset,type,...)
}

`weeklyReturn` <-
function(x,subset=NULL,type='arithmetic',...) {
  periodReturn(x,'weekly',subset,type,...)
}

`quarterlyReturn` <-
function(x,subset=NULL,type='arithmetic',...) {
  periodReturn(x,'quarterly',subset,type,...)
}

`yearlyReturn` <-
function(x,subset=NULL,type='arithmetic',...) {
  periodReturn(x,'yearly',subset,type,...)
}

`annualReturn` <- yearlyReturn

#`dailyReturn` <-
#function(x,from=NULL,to=NULL) {
#  periodReturn(x,by=weekdays,from,to)
#}
#
#`weeklyReturn` <-
#function(x,from=NULL,to=NULL) {
#  periodReturn(x,by=weeks,from,to)
#}
#
#`monthlyReturn` <-
#function(x,from=NULL,to=NULL) {
#  periodReturn(x,by=months,from,to)
#}
#
#`quarterlyReturn` <-
#function(x,from=NULL,to=NULL) {
#  periodReturn(x,by=quarters,from,to)
#}
#`annualReturn` <-
#function(x,from=NULL,to=NULL) {
#  periodReturn(x,by=years,from,to)
#}
#`allReturns` <-
#function(x,from=NULL,to=NULL) {
#  all.ret <- cbind(
#    periodReturn(x,by=weekdays,from,to),
#    periodReturn(x,by=weeks,from,to),
#    periodReturn(x,by=months,from,to),
#    periodReturn(x,by=quarters,from,to),
#    periodReturn(x,by=years,from,to)
#  )
#  colnames(all.ret) <- c('daily','weekly','monthly','quarterly','annual')
#  class(all.ret) <- c("periodReturn","zoo")
#  all.ret
#}

#`barplot.periodReturn` <-
#function(x,...)
#{
#  class(x) <- 'zoo'
#  barplot(x,col=ifelse(x > 0,3,8))
#}

`periodicity0` <-
function(x,...)
{
  p <- median(diff(time(x)))
  if(is.na(p)) stop("cannot calculate periodicity from one observation")
  p.numeric <- as.numeric(p)
  units <- attr(p,'units')
  if(units=="secs") {
    scale <- "seconds"
  }
  if(units=="mins") {
    scale <- "minute"
    if(p.numeric > 59) scale <- "hourly" 
  }
  if(units=="days") {
    scale <- "daily"
    if(p.numeric > 1) scale <- "weekly"
    if(p.numeric > 7) scale <- "monthly"
    if(p.numeric > 31) scale <- "quarterly"
    if(p.numeric > 91) scale <- "yearly"
  }
  xx <- list(difftime=p,frequency=p.numeric,
             start=index(first(x)),end=index(last(x)),units=units,scale=scale)
  class(xx) <- 'periodicity'
  xx
}

`print.periodicity` <-
function(x,...)
{
  x.freq <- ifelse(x$scale %in% c("minute","seconds"),x$frequency,'')
  if(x.freq == '') {
  cap.scale <- paste(toupper(substring(x$scale,1,1)),substring(x$scale,2),sep='')
  cat(paste(cap.scale,'periodicity from',x$start,'to',
            x$end,'\n',sep=' '))
  } else {
  cat(paste(x.freq,x$scale,'periodicity from',x$start,'to',
            x$end,'\n',sep=' '))
  }
}
