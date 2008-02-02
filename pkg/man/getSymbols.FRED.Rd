\name{getSymbols.FRED}
\alias{getSymbols.FRED}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Download Federal Reserve Economic Data - FRED(R) }
\description{
R access to over 11,000 data series accessible
via the St. Louis Federal Reserve Bank's FRED system.

Downloads \code{Symbols} to specified \code{env}
from \sQuote{research.stlouisfed.org}.  This method is
not to be called directly, instead a call to
\code{getSymbols(Symbols,src='FRED')} will in
turn call this method. It is documented for the
sole purpose of highlighting the arguments
accepted, and to serve as a guide to creating
additional getSymbols \sQuote{methods}.
}
\usage{
getSymbols.FRED(Symbols,
                env,
                return.class = "zoo",
                ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
       \item{Symbols}{ a character vector specifying
                       the names of each symbol to be loaded}
       \item{env}{ where to create objects. (.GlobalEnv) }  
       \item{return.class}{ class of returned object }  
       \item{\dots}{ additional parameters }
}
\details{
Meant to be called internally by \code{getSymbols} (see also).

One of many methods for loading
data for use with \pkg{quantmod}.  Essentially a
simple wrapper to the underlying FRED
data download site.

Naming conventions must follow those as seen
on the Federal Reserve Bank of St Louis's
website for FRED. A lookup facility will hopefully
be incorporated into \pkg{quantmod} in the near future.
}
\value{
A call to getSymbols.FRED will load into the specified
environment one object for each
\code{Symbol} specified, with class defined 
by \code{return.class}. Presently this may be \code{ts},
\code{its}, \code{zoo}, or \code{timeSeries}.
}
\references{ St. Louis Fed: Economic Data - FRED \url{http://research.stlouisfed.org/fred2/}
}
\author{ Jeffrey A. Ryan }
\seealso{ \code{\link{getSymbols}},
          \code{\link{setSymbolLookup}} }
\examples{
\dontrun{
# All 3 getSymbols calls return the same
# CPI data to the global environment
# The last example is what NOT to do!

## Method #1
getSymbols('CPIAUCNS',src='FRED')


## Method #2
setDefaults(getSymbols,src='FRED')
  # OR
setSymbolLookup(CPIAUCNS='FRED')

getSymbols('CPIAUCNS')

#########################################
##  NOT RECOMMENDED!!!
#########################################
## Method #3
getSymbols.FRED('CPIAUCNS',env=globalenv())
}
}
\keyword{ data }
