\name{quantmod.OHLC}
\alias{quantmod.OHLC}
\alias{as.quantmod.OHLC}
\alias{quantmod.OHLC}
\title{ Create Open High Low Close Object }
\description{
Coerce an object with the apporpriate columns to class
\code{quantmod.OHLC}, which extends zoo.
}
\usage{
as.quantmod.OHLC(x,
                 col.names = c("Open", "High",
                               "Low", "Close",
                               "Volume", "Adjusted"),
                 name = NULL, ...)
}
\arguments{
  \item{x}{ object of class \code{zoo} }
  \item{col.names}{ suffix for columns }
  \item{name}{ name to attach unique column suffixes to,
               defaults to the object name } 
  \item{\dots}{ additional arguments (unused) }
}
\details{
\code{quantmod.OHLC} is actually just a renaming of an object
of class \code{zoo}, with the convention of NAME.Open, NAME.High, ...
for the column names.

Additionally methods may be written to handle or check for the above conditions
within other functions - as is the case within the \pkg{quantmod} package.
}
\value{
An object of class c('quantmod.OHLC','zoo')
}
\author{ Jeffrey A. Ryan }
\seealso{ \code{\link{OHLC.Transformations}}, \code{\link{getSymbols}} }
\keyword{ data }
