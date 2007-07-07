`periodReturn.quantmod.OHLC` <-
function(x,by=months,from=NULL,to=NULL) {
  if(is.null(from)) from <- start(as.zoo(x))
  if(is.null(to)) to <- end(as.zoo(x))
  x.period <- x[breakpoints(as.zoo(x),by=by,TRUE),]
  adj.length <- NROW(x.period)
  adj.x.period <- as.numeric(Ad(x.period))
  adj.start <- c(as.numeric(Ad(x))[1],adj.x.period[-adj.length])
  returns <- ((adj.x.period - adj.start)/adj.start)
  returns <- zoo(returns,as.Date(index(as.zoo(x.period))))
  returns <- subset(returns,index(returns) >= as.Date(from) & index(returns) <= as.Date(to))
  class(returns) <- c('quantmod.returns','zoo')
  attr(returns,'periodicity') <- as.character(substitute(by))
  return(returns)
}
`periodReturn.zoo` <-
function(x,by=months,from=NULL,to=NULL) {
  from.col <- 4
  x <- as.data.frame(x)
  if(is.null(from)) from <- start(as.zoo(x))
  if(is.null(to)) to <- end(as.zoo(x))
  x.period <- x[breakpoints(as.zoo(x),by=by,TRUE),]
  adj.length <- NROW(x.period)
  adj.x.period <- x.period[,from.col]
  adj.start <- c(x[1,from.col],x.period[-adj.length,from.col])
  returns <- ((adj.x.period - adj.start)/adj.start)
  returns <- zoo(returns,as.Date(index(as.zoo(x.period))))
  returns <- subset(returns,index(returns) >= as.Date(from) & index(returns) <= as.Date(to))
  class(returns) <- c('quantmod.returns','zoo')
  attr(returns,'periodicity') <- as.character(substitute(by))
  return(returns)
}
`periodReturn` <-
function(x,by=months,from=NULL,to=NULL) 
{
    UseMethod("periodReturn");
}


`dailyReturn` <-
function(x,from=NULL,to=NULL) {
  periodReturn(x,by=weekdays,from,to)
}

`weeklyReturn` <-
function(x,from=NULL,to=NULL) {
  periodReturn(x,by=weeks,from,to)
}

`monthlyReturn` <-
function(x,from=NULL,to=NULL) {
  periodReturn(x,by=months,from,to)
}

`quarterlyReturn` <-
function(x,from=NULL,to=NULL) {
  periodReturn(x,by=quarters,from,to)
}
`annualReturn` <-
function(x,from=NULL,to=NULL) {
  periodReturn(x,by=years,from,to)
}
`allReturns` <-
function(x,from=NULL,to=NULL) {
  all.ret <- cbind(
    periodReturn(x,by=weekdays,from,to),
    periodReturn(x,by=weeks,from,to),
    periodReturn(x,by=months,from,to),
    periodReturn(x,by=quarters,from,to),
    periodReturn(x,by=years,from,to)
  )
  colnames(all.ret) <- c('daily','weekly','monthly','quarterly','annual')
  all.ret
}
