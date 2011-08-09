\name{getSymbols.yahoo}
\alias{getSymbols.yahoo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Download OHLC Data From Yahoo Finance }
\description{
Downloads \code{Symbols} to specified \code{env}
from \sQuote{finance.yahoo.com}.  This method is
not to be called directly, instead a call to
\code{getSymbols(Symbols,src='yahoo')} will in
turn call this method. It is documented for the
sole purpose of highlighting the arguments
accepted, and to serve as a guide to creating
additional getSymbols \sQuote{methods}.
}
\usage{
getSymbols.yahoo(Symbols,
                 env,
                 return.class = 'xts',
                 index.class  = 'Date',
                 from = "2007-01-01",
                 to = Sys.Date(),
                  ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
       \item{Symbols}{ a character vector specifying
                       the names of each symbol to be loaded}
       \item{env}{ where to create objects. (.GlobalEnv) }  
       \item{return.class}{ class of returned object }  
       \item{index.class}{ class of returned object index (xts only) }  
       \item{from}{ Retrieve data no earlier than this date. 
                    (2007-01-01)}
       \item{to}{ Retrieve data through this date (Sys.Date())}
       \item{\dots}{ additional parameters }
}
\details{
Meant to be called internally by \code{getSymbols} (see also).

One of a few currently defined methods for loading
data for use with \pkg{quantmod}.  Essentially a
simple wrapper to the underlying Yahoo! finance site's
historical data download.
}
\value{
A call to getSymbols.yahoo will load into the specified
environment one object for each
\code{Symbol} specified, with class defined 
by \code{return.class}. Presently this may be \code{ts},
\code{its}, \code{zoo}, \code{xts}, or \code{timeSeries}.

In the case of xts objects, the indexing will be by Date. This
can be altered with the \code{index.class} argument.  See
\code{indexClass} for more information on changing index classes.
}
\references{ Yahoo Finance: \url{http://finance.yahoo.com} }
\author{ Jeffrey A. Ryan }
\seealso{ \code{\link{getSymbols}},
          \code{\link{setSymbolLookup}} }
\examples{
\dontrun{
# All 3 getSymbols calls return the same
# MSFT to the global environment
# The last example is what NOT to do!

## Method #1
getSymbols('MSFT',src='yahoo')


## Method #2
setDefaults(getSymbols,src='yahoo')
  # OR
setSymbolLookup(MSFT='yahoo')

getSymbols('MSFT')

#########################################
##  NOT RECOMMENDED!!!
#########################################
## Method #3
getSymbols.yahoo('MSFT',env=globalenv())
}
}
\keyword{ data }
