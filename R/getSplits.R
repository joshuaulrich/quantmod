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
    if(!missing(auto.assign) && !isTRUE(auto.assign) && !is.null(env)) {
      warning("ignoring 'auto.assign = FALSE' because 'env' is specified")
    }
    auto.assign <- TRUE
  }
  if(is.null(env))
    auto.assign <- FALSE
  Symbol.name <- ifelse(!is.character(Symbol),
                        deparse(substitute(Symbol)),
                        as.character(Symbol))

  from.posix <- .dateToUNIX(from)
  to.posix <- .dateToUNIX(to)

  handle <- .getHandle()
  yahoo.URL <- .yahooJsonURL(Symbol.name, from.posix, to.posix, "3mo")
  yahoo.URL <- paste0(yahoo.URL, "&events=splits")

  conn <- curl::curl(yahoo.URL,handle=handle)
  json <- try(jsonlite::fromJSON(conn, simplifyVector = FALSE)$chart$result, silent = TRUE)

  if(inherits(json, "try-error")) {
    msg <- paste0("Unable to import splits for", Symmbol.name,
                  ".\n", attr(test, "condition")$message)
    stop(msg)
  }

  split.events <- json[[1]][["events"]][["splits"]]

  if(length(split.events) > 0) {
    to.xts <- function(x) {
      ratio <- x$numerator/x$denominator
      xts(ratio, as.Date(.POSIXct(x$date, "UTC")))
    }
    fr <- 1 / do.call(rbind, lapply(split.events, to.xts))
  } else {
    fr <- xts(numeric(0), .Date(integer(0)))
  }
  colnames(fr) <- paste(Symbol.name,'spl',sep='.')

  if(is.xts(tmp.symbol)) {
    if(auto.update) {
      xtsAttributes(tmp.symbol) <- list(splits=fr)
      assign(Symbol.name,tmp.symbol,envir=env)
    }
  } else if(auto.assign) {
      assign(paste(Symbol.name,'spl',sep='.'),fr,envir=env)
  } else fr
}
