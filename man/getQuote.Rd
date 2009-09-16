\name{getQuote}
\alias{getQuote}
\alias{standardQuote}
\alias{yahooQF}
\alias{yahooQuote.EOD}
\title{ Download Current Stock Quote }
\description{
Fetch current stock quote(s) from specified source.  At present this only
handle sourcing quotes from Yahoo Finance, but it will be extended to
additional sources over time.
}
\usage{
getQuote(Symbols, src = "yahoo", what = standardQuote(), ...)

standardQuote(src="yahoo")
yahooQF(names)
yahooQuote.EOD
}
\arguments{
  \item{Symbols}{ character string of symbols, seperated by semi-colons }
  \item{src}{ source of data (only yahoo is implemented) }
  \item{what}{ what should be retrieved }
  \item{names}{ which data should be retrieved }
  \item{\dots}{ currently unused }
}
\value{
A maximum of 200 symbols may be requested per call to Yahoo!, and 
all requested will be returned in one data.frame object. 
If more that 200 symbols are requested, multiple 200 symbol calls will be made
and the results will be returned as one data object.

\code{getQuote} returns a data frame with rows matching the number of Symbols
requested, and the columns matching the requested columns.

The \code{what} argument allows for specific data to be requested
from yahoo, using Yahoo!'s formatting string.  A list and interactive
selection tool can be seen with \code{yahooQF}.

\code{standardQuote} currently only applied to Yahoo! data, and returns
an object of class quoteFormat, for use within the \code{getQuote} function.

\code{yahooQuote.EOD} is a constant that returns OHLCV data
}
\references{ 
Yahoo! Finance \url{finance.yahoo.com} 
gummy-stuff.org \url{www.gummy-stuff.org/Yahoo-data.htm}
}
\author{ Jeffrey A. Ryan }
\seealso{ \code{\link{getSymbols}}}
\examples{
yahooQuote.EOD
\dontrun{
getQuote("AAPL")
getQuote("QQQQ;SPY;^VXN",what=yahooQF(c("Bid","Ask")))
standardQuote()
yahooQF()
}
}
\keyword{ IO }
\keyword{ data }
