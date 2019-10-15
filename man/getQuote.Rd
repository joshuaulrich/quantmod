\name{getQuote}
\alias{getQuote}
\alias{standardQuote}
\alias{yahooQF}
\alias{yahooQuote.EOD}
\title{ Download Current Stock Quote }
\description{
Fetch current stock quote(s) from specified source.  At present this only
handles sourcing quotes from Yahoo Finance and Alpha Vantage, but it can be
extended to additional sources.
}
\usage{
getQuote(Symbols, src = "yahoo", what, ...)

standardQuote(src="yahoo")
yahooQF(names)
yahooQuote.EOD
}
\arguments{
  \item{Symbols}{ character string of symbols, separated by semi-colons }
  \item{src}{ source of data ("yahoo" and "av" are currently implemented) }
  \item{what}{ what should be retrieved }
  \item{names}{ which data should be retrieved }
  \item{\dots}{ currently unused }
}
\value{
The number of symbols per request is limited to 200 for Yahoo! and 100 for
Alpha Vantage.  \code{getQuote} will make multiple requests if more than the
maximum number of symbols are requested.  It will then combine the results of
all the requests and return one data frame.  The data frame will contain a
column for each requested data field, and the requested symbols will be stored
in the row names.

In order to import quotes from Alpha Vantage, you must obtain an
\emph{API key} by completing a free, one-time registration at their website:
\url{https://www.alphavantage.co/}.  Then you can call \code{getQuote} with
\code{src = "av"} and \code{api.key = "[your key]"}.

The \code{what} argument allows for specific data to be requested.  For
\code{getQuote.yahoo}, the value of \code{what} should be a \code{quoteFormat}
object like that returned by \code{standardQuote}.  The \code{yahooQF} function
will create an interactive selection tool if the \code{what} argument is
missing.  The \code{what} argument is currently ignored for Alpha Vantage data
because only a few fields are available.

\code{standardQuote} currently only applies to the Yahoo! data source. It
returns a \code{quoteFormat} object to use with the \code{getQuote} function.

\code{yahooQuote.EOD} is a constant \code{quoteFormat} object for OHLCV data.
}
\references{ 
  Yahoo! Finance \url{https://finance.yahoo.com}
  Alpha Vantage \url{https://www.alphavantage.co}
}
\author{
  Jeffrey A. Ryan (Yahoo)
  Ethan B. Smith (Alpha Vantage)
}
\seealso{ \code{\link{getSymbols}}, \code{\link{getSymbols.av}} }
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
