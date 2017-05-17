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
                            "&events=split",
                            "&crumb=", handle$cb),
                     destfile=tmp, quiet=!verbose, handle=handle$ch)

  fr <- read.csv(tmp, as.is=TRUE)

  if(NROW(fr)==0) {
    fr <- NA
  } else {
    fr$V3 <- 1 / sapply(parse(text=fr[,2]), eval)
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
