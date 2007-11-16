"breakpoints" <-
function(x,by=c(weekdays,weeks,months,quarters,years),...) {
  if(length(by) != 1) stop('choose ONE method for "by"')
#  by.arg <- match.arg(substitute(by),c("weekdays","weeks","months","quarters","years"));
#  by <- eval(parse(text=by.arg));
#  if(is.na(pmatch(deparse(substitute(by)),c("weekdays","weeks","months","quarters","years"))))
#    stop("by must be weekdays,weeks,months,quarters, or years");
  by <- match.fun(by);
  breaks <- which(diff(as.numeric(by(x,...))) != 0);
#  nr <- ifelse(inherits(x,'POSIXt'),NROW(x[[1]]),NROW(x))
  nr <- NROW(x)
  breaks <- c(0,breaks,nr);
  return(breaks);
}
