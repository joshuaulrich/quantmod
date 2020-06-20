`getOptionChain` <-
function(Symbols, Exp=NULL, src="yahoo", ...) {
  Call <- paste("getOptionChain",src,sep=".")
  if(missing(Exp)) {
    optionChain <- do.call(Call, list(Symbols=Symbols, ...))
  } else {
    optionChain <- do.call(Call, list(Symbols=Symbols, Exp=Exp, ...))
  }
  # only return non- NULL elements
  optionChain[!vapply(optionChain, is.null, logical(1))]
}

getOptionChain.yahoo <- function(Symbols, Exp, ...)
{
  if(!requireNamespace("jsonlite", quietly=TRUE))
    stop("package:",dQuote("jsonlite"),"cannot be loaded.")

  NewToOld <- function(x, tz = NULL) {
    if(is.null(x) || length(x) < 1)
      return(NULL)
    # clean up colnames, in case there's weirdness in the JSON
    names(x) <- tolower(gsub("[[:space:]]", "", names(x)))
    # set cleaned up colnames to current output colnames
    d <- with(x, data.frame(Strike=strike, 
                            Last=lastprice, 
                            Chg=change,
                            Bid=bid, 
                            Ask=ask, 
                            Vol= if("volume" %in% names(x)) {volume} else {NA}, 
                            OI= if("openinterest" %in% names(x)) {openinterest} else {NA},
                            LastTradeTime= if("lasttradedate" %in% names(x)) {lasttradedate} else {NA},
                            IV= if("impliedvolatility" %in% names(x)) {impliedvolatility} else {NA},
                            ITM= if("inthemoney" %in% names(x)) {inthemoney} else {NA},
                            row.names=contractsymbol, stringsAsFactors=FALSE))
    # convert trade time to exchange timezone
    d$LastTradeTime <- .POSIXct(d$LastTradeTime, tz=tz)
    d
  }

  # Don't check the expiry date if we're looping over dates we just scraped
  checkExp <- !hasArg(".expiry.known") || !match.call(expand.dots=TRUE)$.expiry.known
  # Construct URL
  urlExp <- paste0("https://query2.finance.yahoo.com/v7/finance/options/", Symbols[1])
  # Add expiry date to URL
  if(!checkExp)
    urlExp <- paste0(urlExp, "?&date=", Exp)

  # Fetch data (jsonlite::fromJSON will handle connection)
  tbl <- try(jsonlite::fromJSON(urlExp), silent = TRUE)

  if(inherits(tbl, "try-error")) {
    msg <- attr(tbl, "condition")[["message"]]
    expDate <- .Date(Exp / 86400)
    warning("no data for '", Symbols[1], "' expiry ", expDate,
            ", omitting\n\t(server response: ", msg, ")",
            immediate. = TRUE, call. = FALSE)
    return(NULL)
  }

  # Only return nearest expiry (default served by Yahoo Finance), unless the user specified Exp
  if(!missing(Exp) && checkExp) {
    all.expiries <- tbl$optionChain$result$expirationDates[[1]]
    all.expiries.posix <- .POSIXct(as.numeric(all.expiries), tz="UTC")

    if(is.null(Exp)) {
      # Return all expiries if Exp = NULL
      out <- lapply(all.expiries, getOptionChain.yahoo, Symbols=Symbols, .expiry.known=TRUE)
      # Expiry format was "%b %Y", but that's not unique with weeklies. Change
      # format to "%b.%d.%Y" ("%Y-%m-%d wouldn't be good, since names should
      # start with a letter or dot--naming things is hard).
      return(setNames(out, format(all.expiries.posix, "%b.%d.%Y")))
    } else {
      # Ensure data exist for user-provided expiry date(s)
      if(inherits(Exp, "Date"))
        valid.expiries <- as.Date(all.expiries.posix) %in% Exp
      else if(inherits(Exp, "POSIXt"))
        valid.expiries <- all.expiries.posix %in% Exp
      else if(is.character(Exp)) {
        expiry.range <- range(unlist(lapply(Exp, .parseISO8601, tz="UTC")))
        valid.expiries <- all.expiries.posix >= expiry.range[1] &
                          all.expiries.posix <= expiry.range[2]
      }
      if(all(!valid.expiries))
        stop("Provided expiry date(s) not found. Available dates are: ",
             paste(as.Date(all.expiries.posix), collapse=", "))

      expiry.subset <- all.expiries[valid.expiries]
      if(length(expiry.subset) == 1)
        return(getOptionChain.yahoo(Symbols, expiry.subset, .expiry.known=TRUE))
      else {
        out <- lapply(expiry.subset, getOptionChain.yahoo, Symbols=Symbols, .expiry.known=TRUE)
        # See comment above regarding the output names
        return(setNames(out, format(all.expiries.posix[valid.expiries], "%b.%d.%Y")))
      }
    }
  }

  dftables <- lapply(tbl$optionChain$result$options[[1]][,c("calls","puts")], `[[`, 1L)

  tz <- tbl$optionChain$result$quote$exchangeTimezoneName[1L]

  dftables <- lapply(dftables, NewToOld, tz=tz)
  dftables
}

