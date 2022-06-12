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
  yahoo.URL <- paste0(yahoo.URL, "&events=div|splits")

  conn <- curl::curl(yahoo.URL,handle=handle)
  json <- try(jsonlite::fromJSON(conn, simplifyVector = FALSE)$chart$result, silent = TRUE)
  div.events <- json[[1]][["events"]][["dividends"]]

  if(length(div.events) > 0) {
    div.to.xts <- function(x) {
      xts(x$amount, as.Date(.POSIXct(x$date, "UTC")))
    }
    # dividends from Yahoo are split-adjusted
    divs <- do.call(rbind, lapply(div.events, div.to.xts))

    split.events <- json[[1]][["events"]][["splits"]]
    if(!split.adjust || length(split.events) == 0) {
      # un-adjust dividends for splits
      spl.to.xts <- function(x) {
        ratio <- x$numerator/x$denominator
        xts(ratio, as.Date(.POSIXct(x$date, "UTC")))
      }
      splits <- do.call(rbind, lapply(split.events, spl.to.xts))

      divs <- divs * adjRatios(splits=merge(splits, index(divs)))[,1]
    }

    fr <- divs
  } else {
    fr <- xts(numeric(0), .Date(integer(0)))
  }

  colnames(fr) <- paste(Symbol.name,'div',sep='.')

  if(is.xts(tmp.symbol)) {
    if(auto.update) {
      xtsAttributes(tmp.symbol) <- list(dividends=fr)
      assign(Symbol.name,tmp.symbol,envir=env)
    }
  } else if(auto.assign) {
      assign(paste(Symbol.name,'div',sep='.'),fr,envir=env)
  } else fr
}
