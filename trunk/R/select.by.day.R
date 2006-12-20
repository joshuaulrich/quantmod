"select.by.day" <-
function(data, dayofweek) {
  subset(data, strftime(as.POSIXlt(date),format="%A")==dayofweek)
}

