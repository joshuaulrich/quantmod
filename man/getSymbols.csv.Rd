\name{getSymbols.csv}
\alias{getSymbols.csv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Load Data from csv File }
\description{
Downloads \code{Symbols} to specified \code{env}
from local comma seperated file.  This method is
not to be called directly, instead a call to
\code{getSymbols(Symbols,src='csv')} will in
turn call this method. It is documented for the
sole purpose of highlighting the arguments
accepted, and to serve as a guide to creating
additional getSymbols \sQuote{methods}.
}
\usage{
getSymbols.csv(Symbols,
                 env,
                 dir="",
                 return.class = "xts",
                 extension="csv",
                  ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
       \item{Symbols}{ a character vector specifying
                       the names of each symbol to be loaded}
       \item{env}{ where to create objects. (.GlobalEnv) }  
       \item{dir}{ directory of csv file }
       \item{return.class}{ class of returned object }  
       \item{extension}{ extension of csv file }
       \item{\dots}{ additional parameters }
}
\details{
Meant to be called internally by \code{getSymbols} (see also).

One of a few currently defined methods for loading
data for use with \pkg{quantmod}.  Essentially a
simple wrapper to the underlying \R \code{read.csv}.
}
\value{
A call to getSymbols.csv will load into the specified
environment one object for each
\code{Symbol} specified, with class defined 
by \code{return.class}. Presently this may be \code{ts},
\code{its}, \code{zoo}, \code{xts}, or \code{timeSeries}.
}
\note{
This has yet to be tested on a windows platform. It \emph{should} work
though file seperators may be an issue.
}
\author{ Jeffrey A. Ryan }
\seealso{ \code{\link{getSymbols}},
          \code{\link{read.csv}},
          \code{\link{setSymbolLookup}} }
\examples{
\dontrun{
# All 3 getSymbols calls return the same
# MSFT to the global environment
# The last example is what NOT to do!

## Method #1
getSymbols('MSFT',src='csv')


## Method #2
setDefaults(getSymbols,src='csv')
  # OR
setSymbolLookup(MSFT='csv')

getSymbols('MSFT')

#########################################
##  NOT RECOMMENDED!!!
#########################################
## Method #3
getSymbols.csv('MSFT',verbose=TRUE,env=globalenv())
}
}
\keyword{ data }
