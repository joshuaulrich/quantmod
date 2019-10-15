`getSplits` <-
function(Symbol,from='1970-01-01',to=Sys.Date(),env=parent.frame(),src='yahoo',
         auto.assign=FALSE,auto.update=FALSE,verbose=FALSE,...) {

  # Function written by Joshua Ulrich, using
  # getSymbols.yahoo as a guide.
  if(missing(env))
    env <- parent.frame(1)
  if(is.null(env))
    auto.assign <- FALSE
  Symbol.name <- ifelse(!is.character(Symbol),
                        deparse(substitute(Symbol)),
                        as.character(Symbol))

  from.posix <- .dateToUNIX(from)
  to.posix <- .dateToUNIX(to)

  handle <- .getHandle()
  yahoo.URL <- .yahooURL(Symbol.name, from.posix, to.posix,
                         "1d", "split", handle)
  
  fr <- read.csv(curl::curl(yahoo.URL, handle=handle$ch), as.is=TRUE)

  if(NROW(fr)==0) {
    fr <- NA
  } else {
    fr$V3 <- vapply(parse(text=fr[,2]), eval, numeric(1))
    fr <- xts(fr$V3, as.Date(fr[,1], "%Y-%m-%d"))
    colnames(fr) <- paste(Symbol.name,'spl',sep='.')
  }

  if(is.xts(Symbol)) {
    if(auto.update) {
      xtsAttributes(Symbol) <- list(splits=fr)
      assign(Symbol.name,Symbol,envir=env)
    }
  } else if(auto.assign) {
      assign(paste(Symbol.name,'spl',sep='.'),fr,envir=env)
  } else fr
}
