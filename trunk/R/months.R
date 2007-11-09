`seconds` <-
function(x,k=1,...) {
  if(periodicity(x)$units=="mins" || periodicity(x)$units=="secs")
    stop("cannot find seconds of lower frequency data")
  UseMethod("seconds")
}
`minutes` <-
function(x,k=1,...) {
  if(periodicity(x)$units=="days")
    stop("cannot find minutes of lower frequency data")
  UseMethod("minutes")
}
`minutes3` <-
function(x,...) {
  minutes(x,k=3,...)
}
`minutes5` <-
function(x,...) {
  minutes(x,k=5,...)
}
`minutes10` <-
function(x,...) {
  minutes(x,k=10,...)
}
`minutes15` <-
function(x,...) {
  minutes(x,k=15,...)
}
`minutes30` <-
function(x,...) {
  minutes(x,k=30,...)
}
`hours` <-
function(x,...) {
  UseMethod("hours")
}
`days` <-
function(x,...) {
  UseMethod("days")
}
`months` <-
function (x, abbreviate) 
{
  UseMethod("months")
}
`quarters` <-
function (x, abbreviate) 
{
  UseMethod("quarters")
}
`weekdays` <-
function (x, abbreviate) 
{
  UseMethod("weekdays")
}
`weeks` <-
function(x, abbreviate)
{
  UseMethod("weeks")
}
`years` <-
function(x, abbreviate)
{ 
  UseMethod("years")
}
`seconds.zoo` <-
function(x,k=1,...) {
  m <- as.numeric(format(index(x),"%S"))
  as.character((m %/% k)+1)
}
`minutes.zoo` <-
function(x,k=1,...) {
  m <- as.numeric(format(index(x),"%M"))
  as.character((m %/% k)+1)
}
`hours.zoo` <-
function(x,...) {
  format(index(x),"%H")
}
`days.zoo` <-
function(x,...) {
  format(index(x),"%j")
}
`days.timeSeries` <-
function(x,...) {
  format(as.timeDate(dimnames(x)[[1]]),"%j")
}
`months.zoo` <-
function(x, abbreviate = FALSE)
{
  format(index(x), ifelse(abbreviate, "%m", "%B"))
}
`months.timeSeries` <-
function(x, abbreviate = FALSE)
{
  format(as.timeDate(dimnames(x)[[1]]), ifelse(abbreviate, "%m", "%B"))
}
`quarters.zoo` <-
function(x, abbreviate = FALSE)
{
  x <- (as.POSIXlt(index(x))$mon) %/% 3
  if(abbreviate) {
    x <- as.character(x+1)
  } else {
    x <- paste("Q",x+1,sep="")
  }
  return(x)
}
`quarters.timeSeries` <-
function(x, abbreviate = FALSE)
{
  x <- (as.POSIXlt(dimnames(x)[[1]])$mon) %/% 3
  return(x+1)
}

`weekdays.zoo` <-
function(x, abbreviate = FALSE)
{
  format(index(x), ifelse(abbreviate, "%w", "%A"))
}
`weekdays.timeSeries` <-
function(x, abbreviate = FALSE)
{
  format(as.timeDate(dimnames(x)[[1]]), ifelse(abbreviate, "%w", "%A"))
}
`weeks.zoo` <-
function(x, abbreviate = FALSE)
{
  format(index(x),"%W")
}
`weeks.timeSeries` <-
function(x, abbreviate = FALSE)
{
  format(as.timeDate(dimnames(x)[[1]]),"%W") 
}
`weeks.timeDate` <-
function(x, abbreviate = FALSE)
{
  format(x,"%W") 
}
`weeks.Date` <-
function(x, abbreviate = FALSE)
{
  format(x,"%W") 
}
`weeks.POSIXt` <-
function(x, abbreviate = FALSE)
{
  format(x,"%W") 
}

`years.zoo` <-
function(x, abbreviate = FALSE)
{
  format(index(x), ifelse(abbreviate, "%y", "%Y"))
}
`years.timeSeries` <-
function(x, abbreviate = FALSE)
{
  format(as.timeDate(dimnames(x)[[1]]), ifelse(abbreviate, "%y", "%Y"))
}
