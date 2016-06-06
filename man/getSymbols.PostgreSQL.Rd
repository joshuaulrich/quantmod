\name{getSymbols.PostgreSQL}
\alias{getSymbols.PostgreSQL}
\alias{getSymbols.PostgreSQL}
\title{ Retrieve Data from PostgreSQL Database }
\description{
Fetch data from PostgreSQL database.  As with other
methods extending the \code{getSymbols} function,
this should \emph{NOT} be called directly.  Its
documentation is meant to highlight the formal
arguments, as well as provide a reference for
further user contributed data tools.
}
\usage{
getSymbols.PostgreSQL(Symbols,
                 env, 
                 return.class = 'xts',
                 db.fields = c("date", "o", "h", "l", "c", "v", "a"), 
                 field.names = NULL,
                 user = NULL, 
                 password = NULL, 
                 dbname = NULL, 
                 host = "localhost", 
                 port = 5432,
                 options = "",
                 search_path=NULL,
                 ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
       \item{Symbols}{ a character vector specifying
                       the names of each symbol to be loaded}
       \item{env}{ where to create objects. (.GlobalEnv)}
       \item{return.class}{ desired class of returned object.
                            Can be xts, 
                            zoo, data.frame, ts, or its. (zoo)}
       \item{db.fields}{ character vector indicating
                           names of fields to retrieve}
       \item{field.names}{ names to assign to returned columns }
       \item{user}{ username to access database }
       \item{password}{ password to access database }
       \item{dbname}{ database name }
       \item{host}{ database host }
       \item{port}{ database port }
       \item{options}{ command-line options to be sent to the server }
       \item{search_path}{ schema path for table search }
       \item{\dots}{ currently not used }
}
\details{
Meant to be called internally by \code{getSymbols} (see also)

One of a few currently defined methods for loading data for
use with \pkg{quantmod}. Its use requires the packages
\pkg{DBI} and \pkg{PostgreSQL}, along with a running
PostgreSQL database with tables corresponding to the
\code{Symbol} name.

The purpose of this abstraction is to make transparent the
\sQuote{source} of the data, allowing instead the user to
concentrate on the data itself.
}
\value{
A call to getSymbols.PostgreSQL will load into the specified
environment one object for each \code{Symbol} specified,
with class defined by \code{return.class}.
}
\references{ 
\itemize{
   \cite{PostgreSQL \url{https://www.postgresql.org}}

   \cite{R-SIG-DB. DBI: R Database Interface}
 }
 }
\author{ Jeffrey A. Ryan and Andre I. Mikulec }
\note{ 
The default configuration needs a table named
for the Symbol specified (e.g. MSFT), with
column names date,o,h,l,c,v,a. For table
layout changes it is best to use
\code{setDefaults(getSymbols.PostgreSQL,...)} with
the new db.fields values specified.
}
\seealso{ \code{\link{getSymbols}},
          \code{\link{setSymbolLookup}} }
\examples{
\dontrun{
# All 3 getSymbols calls return the same
# MSFT to the global environment
# The last example is what NOT to do!

setDefaults(getSymbols.PostgreSQL,user='jdoe',password='secret',
            dbname='tradedata',search_path='usschema')

## Method #1
getSymbols('MSFT',src='PostgreSQL')


## Method #2
setDefaults(getSymbols,src='PostgreSQL')
  # OR
setSymbolLookup(MSFT='PostgreSQL')

getSymbols('MSFT')

#########################################
##  NOT RECOMMENDED!!!
#########################################
## Method #3
getSymbols.PostgreSQL('MSFT',env=globalenv())
}
}
\keyword{ data }
