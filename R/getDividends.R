`getDividends` <-
function(Symbol,from='1970-01-01',to=Sys.Date(),env=parent.frame(),src='yahoo',
         auto.assign=FALSE,auto.update=FALSE,verbose=FALSE,split.adjust=TRUE,...,
         curl.options=list()) {

  tmp.symbol <- Symbol
  if(missing(env)) {
    env <- parent.frame(1)
  } else {
    if(exists(Symbol, envir = env, inherits = FALSE)) {
      tmp.symbol <- get(Symbol, envir = env)
    }
  }
  if(is.null(env))
    auto.assign <- FALSE
  Symbol.name <- ifelse(!is.character(Symbol),
                        deparse(substitute(Symbol)),
                        as.character(Symbol))

  from.posix <- .dateToUNIX(from)
  to.posix <- .dateToUNIX(to)

  handle <- .getHandle()
  yahoo.URL <- .yahooURL(Symbol.name, from.posix, to.posix,
                         "1d", "div", handle)

  conn <- curl::curl(yahoo.URL,handle=handle$ch)
  fr <- try(read.csv(conn, as.is=TRUE), silent = TRUE)

  if (inherits(fr, "try-error")) {
    fr <- retry.yahoo(Symbol.name, from.posix, to.posix, "1d", "div", conn)
  }

  fr <- xts(fr[,2],as.Date(fr[,1]))
  colnames(fr) <- paste(Symbol.name,'div',sep='.')

  # dividends from Yahoo are not split-adjusted
  if(src[1] == "yahoo" && split.adjust) {
    splits <- getSplits(Symbol.name, from="1900-01-01")
    if(is.xts(splits) && is.xts(fr) && nrow(splits) > 0 && nrow(fr) > 0) {
      fr <- fr * adjRatios(splits=merge(splits, index(fr)))[,1]
    }
  }

  if(is.xts(tmp.symbol)) {
    if(auto.update) {
      xtsAttributes(tmp.symbol) <- list(dividends=fr)
      assign(Symbol.name,tmp.symbol,envir=env)
    }
  } else if(auto.assign) {
      assign(paste(Symbol.name,'div',sep='.'),fr,envir=env)
  } else fr
}
