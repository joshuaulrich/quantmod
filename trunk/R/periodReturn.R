`periodReturn` <-
function(x,by=months,from=NULL,to=NULL) {
  if(is.null(from)) from <- start(as.zoo(x))
  if(is.null(to)) to <- end(as.zoo(x))
  x <- subset(x,subset=rownames(x) >= from & rownames(x) <= to)
  x.period <- x[breakpoints(as.zoo(x),by=by,TRUE),]
  adj.length <- NROW(x.period)
  adj.x.period <- Ad(x.period)
  adj.start <- c(Ad(x)[1],Ad(x.period)[-adj.length])
  returns <- ((adj.x.period - adj.start)/adj.start)
  returns <- zoo(returns,as.Date(index(as.zoo(x.period))))
  class(returns) <- c('quantmod.returns',as.character(substitute(by)),'zoo')
  returns
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
