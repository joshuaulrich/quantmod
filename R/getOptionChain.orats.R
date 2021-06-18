#
#   quantmod: Quantitative Financial Modelling Framework
#
#   Copyright (C) 2021 Joshua M. Ulrich, Steve Bronder
#
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 3 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

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
