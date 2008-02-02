\name{getSymbols.SQLite}
\alias{getSymbols.SQLite}
\title{ Retrieve Data from SQLite Database }
\description{
Fetch data from SQLite database.  As with other methods
extending \code{getSymbols}
this function should \emph{NOT} be called directly.
}
\usage{
getSymbols.SQLite(Symbols,
                  env,
                  return.class = c("quantmod.OHLC", "zoo"),
                  db.fields = c("row_names",
                                "Open",
                                "High",
                                "Low",
                                "Close",
                                "Volume",
                                "Adjusted"),
                  field.names = NULL,
                  dbname = NULL,
                  POSIX = TRUE,
                  ...)
}
\arguments{
  \item{Symbols}{ a character vector specifying the names of each
                  symbol to be loaded }
  \item{env}{ where to create the objects }
  \item{return.class}{ desired class of returned object }
  \item{db.fields}{ character vector naming fields to retrieve }
  \item{field.names}{ names to assign to returned columns }
  \item{dbname}{ database name }
  \item{POSIX}{ are rownames numeric }
  \item{\dots}{ additional arguments }
}
\details{
Meant to be called internally by \code{getSymbols} (see also)

One of a few currently defined methods for loading data for use
with 'quantmod'. Its use requires the packages 'DBI' and 'RSQLite',
along with a SQLite database.

The purpose of this abstraction is to make transparent the
'source' of the data, allowing instead the user to concentrate on
the data itself.

}
\value{
A call to getSymbols.SQLite will load into the specified
environment one object for each 'Symbol' specified, with class
defined by 'return.class'.
}
\references{
\itemize{
   \cite{SQLite \url{http://www.sqlite.org}}

   \cite{David A. James 
         RSQLite: SQLite interface for R
         }

   \cite{R-SIG-DB. DBI: R Database Interface}
}
}
\author{Jeffrey A. Ryan}
\note{ 
This function is experimental at best, and has not been
thoroughly tested.  Use with caution, and please report
any bugs to the maintainer of quantmod.
}
\seealso{ \code{\link{getSymbols}}}
\examples{
\dontrun{
getSymbols("QQQQ",src="SQLite")
}
}
\keyword{ utilities }
