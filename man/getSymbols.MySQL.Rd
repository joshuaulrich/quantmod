\name{getSymbols.MySQL}
\alias{getSymbols.MySQL}
\alias{getSymbols.mysql}
\title{ Retrieve Data from MySQL Database }
\description{
Fetch data from MySQL database.  As with other
methods extending the \code{getSymbols} function,
this should \emph{NOT} be called directly.  Its
documentation is meant to highlight the formal
arguments, as well as provide a reference for
further user contributed data tools.
}
\usage{
getSymbols.MySQL(Symbols,
                 env, 
                 return.class = c("quantmod.OHLC", "zoo"), 
                 db.fields = c("date", "o", "h", "l", "c", "v", "a"), 
                 field.names = NULL,
                 user = NULL, 
                 password = NULL, 
                 dbname = NULL, 
                 ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
       \item{Symbols}{ a character vector specifying
                       the names of each symbol to be loaded}
       \item{env}{ where to create objects. (.GlobalEnv)}
       \item{return.class}{ desired class of returned object.
                            Can be quantmod.OHLC (default
                            for \dQuote{yahoo} OHLC data),
                            zoo, data.frame, ts, or its. (zoo)}
       \item{db.fields}{ character vector indicating
                           names of fields to retrieve}
       \item{field.names}{ names to assign to returned columns }
       \item{user}{ username to access database }
       \item{password}{ password to access database }
       \item{dbname}{ database name }
       \item{\dots}{ currently not used }
}
\details{
Meant to be called internally by \code{getSymbols} (see also)

One of a few currently defined methods for loading data for
use with \pkg{quantmod}. Its use requires the packages
\pkg{DBI} and \pkg{MySQL}, along with a running
MySQL database with tables corresponding to the
\code{Symbol} name.

The purpose of this abstraction is to make transparent the
\sQuote{source} of the data, allowing instead the user to
concentrate on the data itself.
}
\value{
A call to getSymbols.MySQL will load into the specified
environment one object for each \code{Symbol} specified,
with class defined by \code{return.class}.
}
\references{ 
\itemize{
   \cite{MySQL AB \url{http://www.mysql.com}}

   \cite{David A. James and Saikat DebRoy (2006).
         R Interface to the MySQL databse.
         \url{www.omegahat.org}}

   \cite{R-SIG-DB. DBI: R Database Interface}
 }
 }
\author{ Jeffrey A. Ryan }
\note{ 
The default configuration needs a table named
for the Symbol specified (e.g. MSFT), with
column names date,o,h,l,c,v,a. For table
layout changes it is best to use
\code{setDefaults(getSymbols.MySQL,...)} with
the new db.fields values specified.
}
\seealso{ \code{\link{getSymbols}},
          \code{\link{setSymbolLookup}} }
\examples{
\dontrun{
# All 3 getSymbols calls return the same
# MSFT to the global environment
# The last example is what NOT to do!

setDefaults(getSymbols.MySQL,user='jdoe',password='secret',
            dbname='tradedata')

## Method #1
getSymbols('MSFT',src='MySQL')


## Method #2
setDefaults(getSymbols,src='MySQL')
  # OR
setSymbolLookup(MSFT='MySQL')

getSymbols('MSFT')

#########################################
##  NOT RECOMMENDED!!!
#########################################
## Method #3
getSymbols.MySQL('MSFT',env=globalenv())
}
}
\keyword{ data }
