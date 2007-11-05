`minutes` <-
function(x,k=1,...) {
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
"months" <-
function (x, abbreviate) 
{
  UseMethod("months")
}
"quarters" <-
function (x, abbreviate) 
{
  UseMethod("quarters")
}
"weekdays" <-
function (x, abbreviate) 
{
  UseMethod("weekdays")
}
"weeks" <-
function(x, abbreviate)
{
  UseMethod("weeks")
}
"years" <-
function(x, abbreviate)
{ 
  UseMethod("years")
}
`minutes.zoo` <-
function(x,k=1,...) {
  m <- as.numeric(format(index(x),"%M"))
  hm <- as.numeric(format(index(x),"%H"))*60
  m <- m + hm
  as.character(rep(1:(length(m)/k),each=k)*k+m[1])
}
`hours.zoo` <-
function(x,...) {
  format(index(x),"%H")
}
`days.zoo` <-
function(x,...) {
  format(index(x),"%j")
}
"months.zoo" <-
function(x, abbreviate = FALSE)
{
  format(index(x), ifelse(abbreviate, "%m", "%B"))
}
"quarters.zoo" <-
function(x, abbreviate = FALSE)
{
  x <- (as.POSIXlt(index(x))$mon) %/% 3;
  return(x+1);
}

"weekdays.zoo" <-
function(x, abbreviate = FALSE)
{
  format(index(x), ifelse(abbreviate, "%w", "%A"))
}
"weeks.zoo" <-
function(x, abbreviate = FALSE)
{
  format(index(x),"%W")
}
"years.zoo" <-
function(x, abbreviate = FALSE)
{
  format(index(x), ifelse(abbreviate, "%y", "%Y"))
}
