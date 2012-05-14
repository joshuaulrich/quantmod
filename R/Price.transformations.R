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
#' @param \dots any other passthrough parameters
#' @export
getPrice <- function (x, symbol=NULL, prefer=NULL,...)
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
              Mid =, mid =, Midpoint =, midpoint = { loc <- has.Mid(x,which=TRUE) },
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
is.BAM <- function(x) {
	if (all(has.Bid(x), has.Ask(x), has.Mid(x))) {
        TRUE
    }
    else FALSE
}

#' @export
is.BATM <- function(x) {
	if (all(has.Bid(x), has.Ask(x), has.Trade(x), has.Mid(x))) {
        TRUE
    }
    else FALSE
}

#' @export
has.Bid <- function(x, which = FALSE)
{
   colAttr <- attr(x, "Bid")
   if(!is.null(colAttr))
     return(if(which) colAttr else TRUE)
   #first try with "price" for data that has both bid.size and bid.price
   loc <- grep("bid.*price", colnames(x), ignore.case=TRUE)
   if (identical(loc, integer(0))) #If no column named bid.price
     loc <- grep("bid", colnames(x), ignore.case=TRUE) #look for bid
   if (!identical(loc, integer(0))) {
       return(if(which) loc else TRUE)
   } else FALSE
}

#' @export
has.BidSize <- function(x, which = FALSE)
{
   colAttr <- attr(x, "BidSize")
   if(!is.null(colAttr))
     return(if(which) colAttr else TRUE)

   loc <- grep("bid.*(size|qty|quantity)", colnames(x), ignore.case=TRUE)
   if (!identical(loc, integer(0))) {
       return(if(which) loc else TRUE)
   } else FALSE
}

#' @export
has.Ask <- function(x, which = FALSE)
{
   colAttr <- attr(x, "Ask") #case sensitive; doesn't work for SYMBOL.Ask :-(
   if(!is.null(colAttr))
     return(if(which) colAttr else TRUE)
    #first try with "price" for data that has both ask.size and ask.price
   loc <- grep("(ask|offer).*price", colnames(x), ignore.case=TRUE) 
   if (identical(loc, integer(0))) #if that failed, try to find just "ask|offer"
     loc <- grep("(ask|offer)", colnames(x), ignore.case=TRUE)
   if (!identical(loc, integer(0))) { 
       return(if(which) loc else TRUE)
   } else FALSE
}

#' @export
has.AskSize <- function(x, which = FALSE)
{
   colAttr <- attr(x, "AskSize")
   if(!is.null(colAttr))
     return(if(which) colAttr else TRUE)

   loc <- grep("(ask|offer).*(size|qty|quantity)", colnames(x), ignore.case=TRUE)
   if (!identical(loc, integer(0))) {
       return(if(which) loc else TRUE)
   } else FALSE
}

#' @export
has.Price <- function(x, which = FALSE)
{
   colAttr <- attr(x, "Price")
   if(!is.null(colAttr))
     return(if(which) colAttr else TRUE)

   locBidAsk <- c(has.Bid(x, which=TRUE),has.Ask(x, which=TRUE))
   loc <- grep("price", colnames(x), ignore.case=TRUE)
   loc <- loc[!(loc %in% locBidAsk)]
   if (!identical(loc, integer(0))) {
       return(if(which) loc else TRUE)
   } else FALSE
}

#' @export
has.Trade <- function(x, which = FALSE)
{
   colAttr <- attr(x, "Trade")
   if(!is.null(colAttr))
     return(if(which) colAttr else TRUE)

   loc <- grep("trade", colnames(x), ignore.case=TRUE)
   if (!identical(loc, integer(0))) {
       return(if(which) loc else TRUE)
   } else FALSE
}

has.Mid <- function(x, which=FALSE) {
    colAttr <- attr(x, "Mid")
    if(!is.null(colAttr))
        return(if(which) colAttr else TRUE)

	loc <- grep("Mid", colnames(x), ignore.case = TRUE)
    if (!identical(loc, integer(0))) 
        return(ifelse(which, loc, TRUE))
    ifelse(which, loc, FALSE)
}

has.Chg <- function(x, which=FALSE) {
    colAttr <- attr(x, "Chg")
    if(!is.null(colAttr))
        return(if(which) colAttr else TRUE)    
	loc <- grep("(chg|change)", colnames(x), ignore.case=TRUE)
    if (!identical(loc, integer(0))) 
        return(ifelse(which, loc, TRUE))
    ifelse(which, loc, FALSE)
}

#has.Un <- function(x, which=FALSE) {
#	loc <- grep("Unadj", colnames(x), ignore.case = TRUE)
#    if (!identical(loc, integer(0))) 
#        return(ifelse(which, loc, TRUE))
#    ifelse(which, loc, FALSE)
#}



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
#' has.AskSize
#' has.Bid
#' has.BidSize
#' has.Price
#' is.BBO
#' is.TBBO
#' @export

has.Qty <- function(x, which = FALSE)
{
   colAttr <- attr(x, "Qty")
   if(!is.null(colAttr))
     return(if(which) colAttr else TRUE)

   locBidAsk <- c(has.Bid(x, which=TRUE),has.Ask(x, which=TRUE))
   loc <- grep("qty", colnames(x), ignore.case=TRUE)
   loc <- loc[!(loc %in% locBidAsk)]
   if (!identical(loc, integer(0))) {
       return(if(which) loc else TRUE)
   } else FALSE
}

# Column setting functions
set.AllColumns <- function(x) {
  cols <- c("Op","Hi","Lo","Cl","Vo","Ad","Price","Trade","Qty",
            "Bid","BidSize","Ask","AskSize","Mid","Chg")
  for(col in cols) {
    try(x <- do.call(paste("set",col,sep="."), list(x)), silent=TRUE )
  }
  return(names(x))
}

set.Chg <- function(x, error=TRUE) {
    if(has.Chg(x))
        attr(x,"Chg") <- has.Chg(x, which=TRUE)
    return(x)
}

set.Mid <- function(x, error=TRUE) {
    if(has.Mid(x))
        attr(x,"Mid") <<- has.Mid(x, which=TRUE)
    return(x)
}

set.Ad <- function(x, error=TRUE) {
  if(has.Ad(x))
    attr(x,"Ad") <- has.Ad(x, which=TRUE)
  return(x)
}


set.Bid <- function(x, error=TRUE) {
  if(has.Bid(x))
    attr(x,"Bid") <- has.Bid(x, which=TRUE)
  return(x)
}
set.BidSize <- function(x, error=TRUE) {
  if(has.BidSize(x))
    attr(x,"BidSize") <- has.BidSize(x, which=TRUE)
  return(x)
}
set.Hi <- function(x, error=TRUE) {
  if(has.Hi(x))
    attr(x,"Hi") <- has.Hi(x, which=TRUE)
  return(x)
}
set.Lo <- function(x, error=TRUE) {
  if(has.Lo(x))
    attr(x,"Lo") <- has.Lo(x, which=TRUE)
  return(x)
}
set.Op <- function(x, error=TRUE) {
  if(has.Op(x))
    attr(x,"Op") <- has.Op(x, which=TRUE)
  return(x)
}
set.Qty <- function(x, error=TRUE) {
  if(has.Qty(x))
    attr(x,"Qty") <- has.Qty(x, which=TRUE)
  return(x)
}
set.Vo <- function(x, error=TRUE) {
  if(has.Vo(x))
    attr(x,"Vo") <- has.Vo(x, which=TRUE)
  return(x)
}
set.Ask <- function(x, error=TRUE) {
  if(has.Ask(x))
    attr(x,"Ask") <- has.Ask(x, which=TRUE)
  return(x)
}
set.AskSize <- function(x, error=TRUE) {
  if(has.AskSize(x))
    attr(x,"AskSize") <- has.AskSize(x, which=TRUE)
  return(x)
}
set.Cl <- function(x, error=TRUE) {
  if(has.Cl(x))
    attr(x,"Cl") <- has.Cl(x, which=TRUE)
  return(x)
}
set.Price <- function(x, error=TRUE) {
  if(has.Price(x))
    attr(x,"Price") <- has.Price(x, which=TRUE)
  return(x)
}
set.Trade <- function(x, error=TRUE) {
  if(has.Trade(x))
    attr(x,"Trade") <- has.Trade(x, which=TRUE)
  return(x)
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
