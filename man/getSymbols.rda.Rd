\name{getSymbols.rda}
\alias{getSymbols.rda}
\alias{getSymbols.RData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Load Data from R Binary File }
\description{
Downloads \code{Symbols} to specified \code{env}
from local R data file.  This method is
not to be called directly, instead a call to
\code{getSymbols(Symbols,src='rda')} will in
turn call this method. It is documented for the
sole purpose of highlighting the arguments
accepted, and to serve as a guide to creating
additional getSymbols \sQuote{methods}.
}
\usage{
getSymbols.rda(Symbols,
                 env,
                 dir="",
                 return.class = "zoo",
                 extension="rda",
                 col.names=c("Open","High","Low","Close","Volume","Adjusted"),
                 ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
       \item{Symbols}{ a character vector specifying
                       the names of each symbol to be loaded}
       \item{env}{ where to create objects. (.GlobalEnv) }  
       \item{dir}{ directory of rda/RData file }
       \item{return.class}{ class of returned object }  
       \item{extension}{ extension of R data file }
       \item{col.names}{ data column names }
       \item{\dots}{ additional parameters }
}
\details{
Meant to be called internally by \code{getSymbols} (see also).

One of a few currently defined methods for loading
data for use with \pkg{quantmod}.  Essentially a
simple wrapper to the underlying \R \code{load}.
}
\value{
A call to getSymbols.csv will load into the specified
environment one object for each
\code{Symbol} specified, with class defined 
by \code{return.class}. Presently this may be \code{ts},
\code{its}, \code{zoo}, \code{quantmod.OHLC}, \code{data.frame}, 
or \code{timeSeries}.
}
\author{ Jeffrey A. Ryan }
\seealso{ \code{\link{getSymbols}},
          \code{\link{load}},
          \code{\link{setSymbolLookup}} }
\examples{
\dontrun{
# All 3 getSymbols calls return the same
# MSFT to the global environment
# The last example is what NOT to do!

## Method #1
getSymbols('MSFT',src='rda')
getSymbols('MSFT',src='RData')


## Method #2
setDefaults(getSymbols,src='rda')
  # OR
setSymbolLookup(MSFT='rda')
  # OR
setSymbolLookup(MSFT=list(src='rda'))

getSymbols('MSFT')

#########################################
##  NOT RECOMMENDED!!!
#########################################
## Method #3
getSymbols.rda('MSFT',verbose=TRUE,env=globalenv())
}
}
\keyword{ data }
