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
                            Bid= if("bid" %in% names(x)) {bid} else {NA}, 
                            Ask= if("ask" %in% names(x)) {ask} else {NA},    
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

.make_orats_option_df <- function(option_df, call_cols, put_cols, extra_cols) {
  call <- data.frame(
    Ticker = option_df$ticker,
    Strike = option_df$strike,
    Bid = option_df$callBidPrice,
    Ask = option_df$callAskPrice,
    Vol = option_df$callVolume,
    OI = option_df$callOpenInterest)
  put <- data.frame(
    Ticker = option_df$ticker,
    Strike = option_df$strike,
    Bid = option_df$putBidPrice,
    Ask = option_df$putAskPrice,
    Vol = option_df$putVolume,
    OI = option_df$putOpenInterest)
  call_extra <- option_df[, c("ticker", "strike", call_cols)]
  colnames(call_extra) <- gsub("call|Call", "", colnames(call_extra))
  put_extra <- option_df[, c("ticker", "strike", put_cols)]
  colnames(put_extra) <- gsub("put|Put", "", colnames(put_extra))
  extra <- option_df[, extra_cols]
  return(list(call = call, put = put, call_extra = call_extra, put_extra = put_extra, extra = extra))
}


getOptionChain.orats <- function(Symbols, Exp, api.key, dte, delta) {
  if(!requireNamespace("jsonlite", quietly=TRUE))
    stop("package:",dQuote("jsonlite"),"cannot be loaded.")
  
  if (missing(api.key)) {
    # Check if they have ORATS_API_KEY defined
    orats_key <- Sys.getenv("ORATS_API_KEY")
    if (orats_key == "") {
      stop(paste0("For the orats API an API key must be provided either",
       " as a function argument or via an environment variable ORATS_API_KEY"))
    } else {
      api.key <- orats_key
    }
  }
  # Construct URL
  base_url <- "https://api.orats.io/datav2/strikes.json"
  urlExp <- paste0(base_url, "?token=", api.key,
                  "&ticker=", paste0(Symbols, collapse=","))
  if (!missing(dte)) {
    if (length(dte) > 2) {
      stop(paste0("Date to Expiry (dte) must be of size 2, but is size (", length(dte), ")"))
    }
    urlExp <- paste0(urlExp, "&dte=", paste0(dte, collapse = ","))
  }
  if (!missing(delta)) {
    if (length(delta) > 2) {
      stop(paste0("Date to Expiry (dte) must be of size 2, but is size (", length(delta), ")"))
    }
    urlExp <- paste0(urlExp, "&delta=", paste0(delta, collapse = ","))
  }
  # Fetch data (jsonlite::fromJSON will handle connection)
  tbl <- jsonlite::fromJSON(urlExp)[["data"]]
  tbl[, "expirDate"] <- as.POSIXct(tbl[, "expirDate"], format = "%Y-%m-%d")
  tbl[, "tradeDate"] <- as.POSIXct(tbl[, "tradeDate"], format = "%Y-%m-%d")
  tbl[, "updatedAt"] <- as.POSIXct(tbl[, "updatedAt"], format = "%Y-%m-%dT%H:%M:%SZ")
  tbl[, "snapShotDate"] <- as.POSIXct(tbl[, "snapShotDate"], format = "%Y-%m-%dT%H:%M:%SZ")
  tbl[, "quoteDate"] <- as.POSIXct(tbl[, "quoteDate"], format = "%Y-%m-%dT%H:%M:%SZ")
  tbl_cols <- colnames(tbl)
  call_cols <- tbl_cols[grep("call|Call", tolower(tbl_cols))]
  put_cols <- tbl_cols[grep("put|Put", tolower(tbl_cols))]
  extra_cols <- colnames(tbl)[!(colnames(tbl) %in% c(call_cols, put_cols))]
  if(!missing(Exp)) {
    if (is.null(Exp)) {
      date_expr <- format(tbl[, "expirDate"], format = "%b.%d.%Y")
      tbl2 <- split(tbl, factor(date_expr, levels = unique(date_expr)))
      tbl3 <- lapply(tbl2, .make_orats_option_df, call_cols, put_cols, extra_cols)
      return(tbl3)
    } else {
      all.expiries <- tbl$expirDate
      all.expiries.posix <- .POSIXct(as.numeric(all.expiries), tz="UTC")
      if(inherits(Exp, "Date")) {
        valid.expiries <- as.Date(all.expiries.posix) %in% Exp
      } else if(inherits(Exp, "POSIXt")) {
        valid.expiries <- all.expiries.posix %in% Exp
      } else if(is.character(Exp)) {
        expiry.range <- range(unlist(lapply(Exp, .parseISO8601, tz="UTC")))
        valid.expiries <- all.expiries.posix >= expiry.range[1] &
          all.expiries.posix <= expiry.range[2]
      }
      if(all(!valid.expiries)) {
        stop("Provided expiry date(s) [", paste0(Exp, collapse = ","), "] not found. Available dates are: ",
             paste(as.Date(all.expiries.posix), collapse=", "))
      }
      exp_posixct <- as.POSIXct(Exp)
      tbl_exp <- tbl[tbl$expirDate %in% exp_posixct,]
      if (length(Exp) == 1) {
        return(.make_orats_option_df(tbl_exp, call_cols, put_cols, extra_cols))
      } else {
        date_expr <- format(tbl_exp[, "expirDate"], format = "%b.%d.%Y")
        tbl2 <- split(tbl_exp, factor(date_expr, levels = unique(date_expr)))
        tbl3 <- lapply(tbl2, .make_orats_option_df, call_cols, put_cols, extra_cols)
        return(tbl3)
      }
    }
  } else {
    tbl_exp <- tbl[tbl$expirDate == tbl$expirDate[1],]
    return(.make_orats_option_df(tbl_exp, call_cols, put_cols, extra_cols))
  }
}

