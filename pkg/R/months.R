`options.expiry` <-
function(x) {
  # the 3rd friday of month
  which(format(index(x),"%d") > 14 &
        format(index(x),"%d") < 22 &
        format(index(x),"%w")==5)
}
`futures.expiry` <-
function(x) {
  # the last 3rd friday of quarter
  which(format(index(x),"%d") > 14 &
        format(index(x),"%d") < 22 &
        format(index(x),"%w")==5 &
        as.numeric(months(x,TRUE)) %in% c(3,6,9,12))
}
`seconds` <-
function(x,k=1,...) {
  UseMethod("seconds")
}
`nseconds` <-
function(x) {
  length(breakpoints(x,seconds,TRUE))-1
}
`minutes` <-
function(x,k=1,...) {
  UseMethod("minutes")
}
`nminutes` <-
function(x) {
  length(breakpoints(x,minutes,TRUE))-1
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
`nhours` <-
function(x) {
  length(breakpoints(x,hours,TRUE))-1
}
`days` <-
function(x,...) {
  UseMethod("days")
}
`ndays` <-
function(x) {
  length(breakpoints(x,days,TRUE))-1
}
`months` <-
function (x, abbreviate) 
{
  UseMethod("months")
}
`nmonths` <-
function(x) {
  length(breakpoints(x,months,TRUE))-1
}
`quarters` <-
function (x, abbreviate) 
{
  UseMethod("quarters")
}
`nquarters` <-
function(x) {
  length(breakpoints(x,quarters,TRUE))-1
}
`weekdays` <-
function (x, abbreviate) 
{
  UseMethod("weekdays")
}
`nweekdays` <-
function(x) {
  length(breakpoints(x,weekdays,TRUE))-1
}
`weeks` <-
function(x, abbreviate)
{
  UseMethod("weeks")
}
`nweeks` <-
function(x) {
  length(breakpoints(x,weeks,TRUE))-1
}
`years` <-
function(x, abbreviate)
{ 
  UseMethod("years")
}
`nyears` <-
function(x) {
  length(breakpoints(x,years,TRUE))-1
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
