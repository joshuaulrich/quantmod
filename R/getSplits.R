`getSplits` <-
function(Symbol,from='1970-01-01',to=Sys.Date(),env=parent.frame(),src='yahoo',
         auto.assign=FALSE,auto.update=FALSE,verbose=FALSE,...,
         curl.options=list()) {

  # Function written by Joshua Ulrich, using
  # getSymbols.yahoo as a guide.
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
                         "1d", "split", handle)

  conn <- curl::curl(yahoo.URL, handle=handle$ch)
  fr <- try(read.csv(conn, as.is=TRUE), silent=TRUE)

  if (inherits(fr, "try-error")) {
    fr <- retry.yahoo(Symbol.name, from.posix, to.posix, "1d", "split", conn)
  }

  if(NROW(fr)==0) {
    fr <- NA
  } else {
    fr[,2] <- gsub(":", "/", fr[,2], fixed = TRUE)
    fr$V3 <- 1 / vapply(parse(text=fr[,2]), eval, numeric(1))
    fr <- xts(fr$V3, as.Date(fr[,1], "%Y-%m-%d"))
    colnames(fr) <- paste(Symbol.name,'spl',sep='.')
  }

  if(is.xts(tmp.symbol)) {
    if(auto.update) {
      xtsAttributes(tmp.symbol) <- list(splits=fr)
      assign(Symbol.name,tmp.symbol,envir=env)
    }
  } else if(auto.assign) {
      assign(paste(Symbol.name,'spl',sep='.'),fr,envir=env)
  } else fr
}
