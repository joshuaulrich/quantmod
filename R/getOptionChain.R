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

getOptionChain.yahoo <- function(Symbols, Exp, ..., session=NULL)
{
  NewToOld <- function(x, tz = NULL) {
    if(is.null(x) || length(x) < 1)
      return(NULL)
    # clean up colnames, in case there's weirdness in the JSON
    names(x) <- tolower(gsub("[[:space:]]", "", names(x)))
    # set cleaned up colnames to current output colnames
    cnames <- c(contractsymbol = "ContractID",
                contractsize = "ConractSize",
                currency = "Currency",
                expiration = "Expiration",
                strike = "Strike",
                lastprice = "Last",
                change = "Chg",
                percentchange = "ChgPct",
                bid = "Bid",
                ask = "Ask",
                volume = "Vol",
                openinterest = "OI",
                lasttradedate = "LastTradeTime",
                impliedvolatility = "IV",
                inthemoney = "ITM")

    # create template data.frame for results
    N <- NROW(x)
    d <- structure(
        list(ContractID    = rep(NA_character_, N),
             ConractSize   = rep(NA_character_, N),
             Currency      = rep(NA_character_, N),
             Expiration    = rep(NA_integer_, N),
             Strike        = rep(NA_real_, N),
             Last          = rep(NA_real_, N),
             Chg           = rep(NA_real_, N),
             ChgPct        = rep(NA_real_, N),
             Bid           = rep(NA_real_, N),
             Ask           = rep(NA_real_, N),
             Vol           = rep(NA_integer_, N),
             OI            = rep(NA_integer_, N),
             LastTradeTime = rep(NA_integer_, N),
             IV            = rep(NA_real_, N),
             ITM           = rep(NA, N)),
        row.names = c(NA, -N), class = "data.frame")

    # fill in available results
    result.colnames <- cnames[names(x)]
    d[, result.colnames] <- x

    # convert expiration to POSIXct for theta decay calculations
    d$Expiration <- as.POSIXct(d$Expiration, origin = "1970-01-01", tz = "UTC")

    # convert trade time to exchange timezone
    d$LastTradeTime <- .POSIXct(d$LastTradeTime, tz=tz)

    return(d)
  }

  if (is.null(session)) {
    session <- .yahooSession()
  }
  if (!session$can.crumb) {
    stop("Unable to obtain yahoo crumb. If this is being called from a GDPR country, Yahoo requires GDPR consent, which cannot be scripted")
  }

  # Don't check the expiry date if we're looping over dates we just scraped
  checkExp <- !hasArg(".expiry.known") || !match.call(expand.dots=TRUE)$.expiry.known
  # Construct URL
  urlExp <- paste0("https://query2.finance.yahoo.com/v7/finance/options/", Symbols[1],
                   "?crumb=", session$crumb)
  # Add expiry date to URL
  if(!checkExp)
    urlExp <- paste0(urlExp, "&date=", Exp)

  # Fetch data (jsonlite::fromJSON will handle connection)
  tbl <- try(jsonlite::fromJSON(curl::curl(urlExp, handle = session$h)), silent = TRUE)

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
      out <- lapply(all.expiries, getOptionChain.yahoo, Symbols=Symbols, .expiry.known=TRUE, session=session)
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
        return(getOptionChain.yahoo(Symbols, expiry.subset, .expiry.known=TRUE, session=session))
      else {
        out <- lapply(expiry.subset, getOptionChain.yahoo, Symbols=Symbols, .expiry.known=TRUE, session=session)
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
