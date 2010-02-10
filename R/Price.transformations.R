###############################################################################
# Utility functions for handling price data
###############################################################################

#' get price column(s) from a timeseries
#'
#' Will attempt to locate price column(s) from a time series with rational defaults.
#'
#' May be subset by symbol and preference.
#' \code{prefer} Preference will be for any commonly used financial time series price description,
#' e.g. 'trade', 'close', 'bid', 'ask' with specific tests and matching for types and column names
#' currently supported in R, but a default grep match will be performed if one of the supported types doesn't match.
#'
#' @param x A data object with columns containing data to be extracted
#' @param symbol text string containing the symbol to extract
#' @param prefer preference for any particular type of price, see Details
#' @export
getPrice <- function (x, symbol=NULL, prefer=NULL)
{
   # first subset on symbol, if present
   if(!is.null(symbol)){
       loc<-grep(symbol, colnames(x))
       if (!identical(loc, integer(0))) {
           x<-x[,loc]
       } else {
           stop(paste("subscript out of bounds: no column name containing",symbol))
       }
   }
   if(is.null(prefer)){
       # default to trying Price, then Trade, then Close
       if(has.Price(x)) prefer='price'
       else if(has.Trade(x)) prefer='trade'
       else if(has.Cl(x))    prefer='close'
       else stop("subscript out of bounds, no price was discernible from the data")
   }
   if(!is.null(prefer)){
       loc <- NULL
       switch(prefer,
              Op =, open =, Open = { loc <- has.Op(x,which=TRUE) },
              Hi =, high =, High = { loc <- has.Hi(x,which=TRUE) },
              Lo =, low =, Low = { loc <- has.Lo(x,which=TRUE) },
              Cl =, close =, Close = { loc <- has.Cl(x,which=TRUE) },
              Bid =, bid = { loc <- has.Bid(x,which=TRUE) },
              Ask =, ask =, Offer =, offer = { loc <- has.Ask(x,which=TRUE) },
              Trade =, trade = { loc <- has.Trade(x,which=TRUE) },
              Price =, price = { loc <- has.Price(x,which=TRUE) },
              {loc <- grep(prefer,colnames(x))}
              )
      if (!identical(loc, integer(0))) return(x[, loc])
      else stop("subscript out of bounds, no price was discernible from the data")
   }
}

#' @export
is.BBO <- function (x)
{
   if (all(has.Bid(x), has.Ask(x))) {
       TRUE
   }
   else FALSE
}

#' @export
is.TBBO <- function (x)
{
   if (all(has.Trade(x),has.Qty(x),has.Bid(x), has.Ask(x))) {
       TRUE
   }
   else FALSE
}

#' @export
has.Bid <- function(x, which = FALSE)
{
   loc <- grep("Bid", colnames(x))
   if (!identical(loc, integer(0))) {
       return(ifelse(which, loc, TRUE))
   } else {
       loc <- grep("bid", colnames(x))
       if (!identical(loc, integer(0)))
           return(ifelse(which, loc, TRUE))
   }
   ifelse(which, loc, FALSE)
}

#' @export
has.Ask <- function(x, which = FALSE)
{
   loc <- grep("Ask", colnames(x))
   if (!identical(loc, integer(0))){
       return(ifelse(which, loc, TRUE))
   } else {
       loc <- grep("ask", colnames(x))
       if (!identical(loc, integer(0)))
           return(ifelse(which, loc, TRUE))
   }
   loc <- grep("Offer", colnames(x))
   if (!identical(loc, integer(0))) {
       return(ifelse(which, loc, TRUE))
   } else {
       loc <- grep("offer", colnames(x))
       if (!identical(loc, integer(0)))
           return(ifelse(which, loc, TRUE))
   }
   ifelse(which, loc, FALSE)
}

#' @export
has.Price <- function(x, which = FALSE)
{
   loc <- grep("Price", colnames(x))
   if (!identical(loc, integer(0))) {
       return(ifelse(which, loc, TRUE))
   } else {
       loc <- grep("price", colnames(x))
       if (!identical(loc, integer(0)))
           return(ifelse(which, loc, TRUE))
   }
   ifelse(which, loc, FALSE)
}

#' @export
has.Trade <- function(x, which = FALSE)
{
   loc <- grep("Trade", colnames(x))
   if (!identical(loc, integer(0))) {
       return(ifelse(which, loc, TRUE))
   } else {
       loc <- grep("trade", colnames(x))
       if (!identical(loc, integer(0)))
           return(ifelse(which, loc, TRUE))
   }
   #trades are often represented by a Price column in TBBO data, e.g. Reuters and blotter
   loc <- grep("Price", colnames(x))
   if (!identical(loc, integer(0))) {
       return(ifelse(which, loc, TRUE))
   } else {
       loc <- grep("price", colnames(x))
       if (!identical(loc, integer(0)))
           return(ifelse(which, loc, TRUE))
   }
   ifelse(which, loc, FALSE)
}

#' check for Trade, Bid, and Ask/Offer (BBO/TBBO), Quantity, and Price data
#'
#' A set of functions to check for appropriate TBBO/BBO and price column
#' names within a data object, as well as the availability and
#' position of those columns.
#' @param x data object
#' @param which disply position of match
#' @aliases
#' has.Trade
#' has.Ask
#' has.Bid
#' has.Price
#' is.BBO
#' is.TBBO
#' @export
has.Qty <- function(x, which = FALSE)
{
   loc <- grep("Qty", colnames(x))
   if (!identical(loc, integer(0))) {
       return(ifelse(which, loc, TRUE))
   } else {
       loc <- grep("qty", colnames(x))
       if (!identical(loc, integer(0)))
           return(ifelse(which, loc, TRUE))
   }
   loc <- grep("Quantity", colnames(x))
   if (!identical(loc, integer(0))) {
       return(ifelse(which, loc, TRUE))
   } else {
       loc <- grep("quantity", colnames(x))
       if (!identical(loc, integer(0)))
           return(ifelse(which, loc, TRUE))
   }
   ifelse(which, loc, FALSE)
}

###############################################################################
# R (http://r-project.org/) quantmod
#
# Copyright (c) 2009-2010
# Peter Carl, Dirk Eddelbuettel, Brian G. Peterson, Jeffrey Ryan, and Joshua Ulrich
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: orders.R 240 2010-02-09 17:17:18Z braverock $
#
###############################################################################
