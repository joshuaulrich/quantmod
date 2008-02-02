\name{getSymbols.google}
\alias{getSymbols.google}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Download OHLC Data From Google Finance }
\description{
Downloads \code{Symbols} to specified \code{env}
from \sQuote{finance.google.com}.  This method is
not to be called directly, instead a call to
\code{getSymbols(Symbols,src='google')} will in
turn call this method. It is documented for the
sole purpose of highlighting the arguments
accepted, and to serve as a guide to creating
additional getSymbols \sQuote{methods}.
}
\usage{
getSymbols.google(Symbols,
                 env,
                 return.class = c("quantmod.OHLC","zoo"),
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
       \item{from}{ Retrieve no earlier than this date }
       \item{to}{ Retrieve though this date }
       \item{\dots}{ additional parameters }
}
\details{
Meant to be called internally by \code{getSymbols} (see also).

One of a few currently defined methods for loading
data for use with \pkg{quantmod}.  Essentially a
simple wrapper to the underlying 
Google Finance site for historical data.

A word of warning. Google is the home of \emph{BETA}, and historic data
is no exception. There is a \emph{BUG} in practically all series that
include the dates Dec 29,30, and 31 of 2003. The data will show
the wrong date and corresponding prices. This essentially makes it
useless, but if they ever apply a fix the data is nice(r) than
Yahoo, in so much as it is all split adjusted and there is forty
years worth to be had.  As long as you skip the holiday week of
2003. : )

}
\value{
A call to getSymbols.google will load into the specified
environment one object for each
\code{Symbol} specified, with class defined 
by \code{return.class}. Presently this may be \code{ts},
\code{its}, \code{zoo}, \code{quantmod.OHLC}, or \code{timeSeries}.
}
\note{
As mentioned in the details section, a serious flaw exists within
the google database/SQL. A caution is issued when retrieving data
via this method if this particular error is encountered, but one
can only wonder what else may be wrong. Caveat emptor.
}
\references{Google Finance: \url{http://finance.google.com}}
\author{ Jeffrey A. Ryan }
\seealso{ \code{\link{getSymbols}},
          \code{\link{setSymbolLookup}} }
\examples{
\dontrun{
# All 3 getSymbols calls return the same
# MSFT to the global environment
# The last example is what NOT to do!

## Method #1
getSymbols('MSFT',src='google')


## Method #2
setDefaults(getSymbols,src='google')
  # OR
setSymbolLookup(MSFT='google')

getSymbols('MSFT')

#########################################
##  NOT RECOMMENDED!!!
#########################################
## Method #3
getSymbols.google('MSFT',verbose=TRUE,env=globalenv())
}
}
\keyword{ data }
