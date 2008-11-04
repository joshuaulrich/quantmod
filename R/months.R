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
months.xts <- function(x, abbreviate)
{
  months(structure(.index(x), class=c('POSIXt','POSIXct'))) 
}

`nmicroseconds` <-
function(x) {
  length(endpoints(x,"microseconds"))-1
}
`nmilliseconds` <-
function(x) {
  length(endpoints(x,"milliseconds"))-1
}
`nseconds` <-
function(x) {
  length(endpoints(x,"seconds"))-1
}
`nminutes` <-
function(x) {
  length(endpoints(x,"minutes"))-1
}
`nhours` <-
function(x) {
  length(endpoints(x,"hours"))-1
}
`ndays` <-
function(x) {
  length(endpoints(x,"days"))-1
}
`nmonths` <-
function(x) {
  length(endpoints(x,"months"))-1
}
`nquarters` <-
function(x) {
  length(endpoints(x,"quarters"))-1
}
`nweekdays` <-
function(x) {
  stop('weekdays are currently unimplemented')
  length(endpoints(x,"weekdays"))-1
}
`nweeks` <-
function(x) {
  length(endpoints(x,"weeks"))-1
}
`nyears` <-
function(x) {
  length(endpoints(x,"years"))-1
}
