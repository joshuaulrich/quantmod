`getDividends` <-
function(Symbol,from='1970-01-01',to=Sys.Date(),env=parent.frame(),src='yahoo',
         auto.assign=FALSE,auto.update=FALSE,verbose=FALSE,split.adjust=TRUE,...) {

  if(missing(env))
    env <- parent.frame(1)
  if(is.null(env))
    auto.assign <- FALSE
  Symbol.name <- ifelse(!is.character(Symbol),
                        deparse(substitute(Symbol)),
                        as.character(Symbol))

  from.posix <- .dateToUNIX(from)
  to.posix <- .dateToUNIX(to)

  tmp <- tempfile()
  on.exit(unlink(tmp))

  handle <- .getHandle()
  yahoo.URL <- .yahooURL(Symbol.name, from.posix, to.posix,
                         "1d", "div", handle)
  curl::curl_download(yahoo.URL, destfile=tmp, quiet=!verbose, handle=handle$ch)

  fr <- read.csv(tmp)
  fr <- xts(fr[,2],as.Date(fr[,1]))
  colnames(fr) <- paste(Symbol.name,'div',sep='.')

  # dividends from Yahoo are not split-adjusted
  if(src[1] == "yahoo" && split.adjust) {
    splits <- getSplits(Symbol.name, from="1900-01-01")
    if(is.xts(splits) && is.xts(fr) && nrow(splits) > 0 && nrow(fr) > 0) {
      fr <- fr * adjRatios(splits=merge(splits, index(fr)))[,1]
    }
  }

  if(is.xts(Symbol)) {
    if(auto.update) {
      xtsAttributes(Symbol) <- list(dividends=fr)
      assign(Symbol.name,Symbol,envir=env)
    }
  } else if(auto.assign) {
      assign(paste(Symbol.name,'div',sep='.'),fr,envir=env)
  } else fr
}
