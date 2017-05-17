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

  yahoo.URL <- "https://query1.finance.yahoo.com/v7/finance/download/"

  from.posix <- as.integer(as.POSIXct(as.Date(from, origin = "1970-01-01")))
  to.posix <- as.integer(as.POSIXct(as.Date(to, origin = "1970-01-01")))

  tmp <- tempfile()
  on.exit(unlink(tmp))

  handle <- .getHandle()
  curl::curl_download(paste0(yahoo.URL, Symbol.name,
                            "?period1=", from.posix,
                            "&period2=", to.posix,
                            "&interval=1d",
                            "&events=div",
                            "&crumb=", handle$cb),
                     destfile=tmp, quiet=!verbose, handle=handle$ch)

  fr <- read.csv(tmp)
  fr <- xts(fr[,2],as.Date(fr[,1]))
  colnames(fr) <- paste(Symbol.name,'div',sep='.')

  # dividends from Yahoo are split-adjusted; need to un-adjust
  if(src[1] == "yahoo" && !split.adjust) {
    splits <- getSplits(Symbol.name, from="1949-01-01")
    if(is.xts(splits) && is.xts(fr) && nrow(splits) > 0 && nrow(fr) > 0) {
      fr <- fr / adjRatios(splits=merge(splits, index(fr)))[,1]
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
