\name{getQuote}
\alias{getQuote}
\title{ Download Current Stock Quote }
\description{
Fetch current stock quote from specified source.  At present this only
handle sourcing quotes from Yahoo Finance, but it will be extended to
additional sources over time.
}
\usage{
getQuote(Symbols, src = "yahoo")
}
\arguments{
  \item{Symbols}{ character string of symbol }
  \item{src}{ source of data (only yahoo is implemented) }
}
\value{
Returns a zoo object of one row, with timestamp of class
POSIXct.
}
\references{ Yahoo! Finance \url{finance.yahoo.com} }
\author{ Jeffrey A. Ryan }
\seealso{ \code{\link{getSymbols}}}
\examples{
\dontrun{
getQuote("AAPL")
}
}
\keyword{ utilities }
