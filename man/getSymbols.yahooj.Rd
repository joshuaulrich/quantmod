\name{getSymbols.yahooj}
\alias{getSymbols.yahooj}
\title{ Download OHLC Data From Yahoo! Japan Finance }
\description{
Downloads \code{Symbols} to specified \code{env}
from \sQuote{finance.yahoo.co.jp}.  This method is
not to be called directly, instead a call to
\code{getSymbols(Symbols,src='yahooj')} will in
turn call this method. It is documented for the
sole purpose of highlighting the arguments
accepted, and to serve as a guide to creating
additional getSymbols \sQuote{methods}.
}
\usage{
getSymbols.yahooj(Symbols,
                  env,
                  return.class = 'xts',
                  index.class  = 'Date',
                  from = "2007-01-01",
                  to = Sys.Date(),
                   ...)
}
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

One of the few currently defined methods for loading
data for use with \pkg{quantmod}.  Essentially a
simple wrapper to the underlying Yahoo! Japan finance site's
historical data download.

The string \sQuote{YJ} will be prepended to the \code{Symbols} because
Japanese ticker symbols usually start with a number and it is cumbersome
to use variable names that start with a number in the R environment.

It is recommended to prepend the ticker symbols with \sQuote{YJ} yourself
if you use \code{setSymbolLookup}. That will make it possible for the main
\code{getSymbols} function to find the symbols in the lookup table.
}
\value{
A call to getSymbols.yahooj will load into the specified
environment one object for each
\code{Symbol} specified, with class defined 
by \code{return.class}. Presently this may be \code{ts},
\code{zoo}, \code{xts}, or \code{timeSeries}.

In the case of xts objects, the indexing will be by Date. This
can be altered with the \code{index.class} argument.  See
\code{indexClass} for more information on changing index classes.
}
\references{ Yahoo! Japan Finance: \url{http://finance.yahoo.co.jp} }
\author{ Wouter Thielen }
\seealso{ \code{\link{getSymbols}},
          \code{\link{setSymbolLookup}} }
\examples{
\dontrun{
# All 4 getSymbols calls return the same
# Sony (6758.T) OHLC to the global environment
# The last example is what NOT to do!

## Method #1
getSymbols('6758.T',src='yahooj')


## Method #2
getSymbols('YJ6758.T',src='yahooj')


## Method #3
setDefaults(getSymbols,src='yahooj')
  # OR
setSymbolLookup(YJ6758.T='yahooj')

getSymbols('YJ6758.T')

#########################################
##  NOT RECOMMENDED!!!
#########################################
## Method #4
getSymbols.yahooj('6758.T',env=globalenv())
}
}
\keyword{ data }
