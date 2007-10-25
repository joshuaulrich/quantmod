\name{Next}
\alias{Next}
\alias{Next.quantmod.OHLC}
\alias{Next.zoo}
\alias{Next.data.frame}
\alias{Next.numeric}
\title{ Advance a Time Series }
\description{
Create a new series with all values advanced forward one period.
The value of period 1, becomes the value at period 2,
value at 2 becomes the original value at 3, etc.  The opposite
of \code{Lag}.  \code{NA} is used to fill.
}
\usage{
Next(x, k = 1)

\method{Next}{quantmod.OHLC}(x,k=1)

\method{Next}{zoo}(x,k=1)

\method{Next}{data.frame}(x,k=1)

\method{Next}{numeric}(x,k=1)

}
\arguments{
  \item{x}{ vector or series to be advanced }
  \item{k}{ periods to advance }
}
\details{
Shift series k-periods up, appending \code{NA}s to end of series.

Specifically designed to handle \code{quantmod.OHLC} and
\code{zoo} series within the \pkg{quantmod} workflow.

If no S3 method is found, a call to \code{lag} in \pkg{base} is made,
with the indexing reversed to shift the time series forward.
}
\value{
The original \code{x} appended with \code{k} \code{NA}s and
missing the leading \code{k} values.

The returned series maintains the number of obs. of the original.

Unlike \code{Lag}, only one value for \code{k} is allowed.
}
\author{ Jeffrey A. Ryan }
\note{
This function's purpose is to get the \dQuote{next} value of
the data you hope to forecast, e.g. a stock's closing value
at t+1.  Specifically to be used within
the \pkg{quantmod} framework of \code{specifyModel}, as a
functional wrapper to the LHS of the model equation.

It is not magic - and thus will not get tomorrow's values\ldots
}
\seealso{  \code{\link{specifyModel}}, \code{\link{Lag}} }
\examples{
Stock.Close <- c(102.12,102.62,100.12,103.00,103.87,103.12,105.12)
Close.Dates <- as.Date(c(10660,10661,10662,10665,10666,10667,10668),origin="1970-01-01")
Stock.Close <- zoo(Stock.Close,Close.Dates)

Next(Stock.Close)       #one period ahead
Next(Stock.Close,k=1)   #same

merge(Next(Stock.Close),Stock.Close)

\dontrun{
# a simple way to build a model of next days
# IBM close, given todays. Technically both
# methods are equal, though the former is seen
# as more intuitive...ymmv
specifyModel(Next(Cl(IBM)) ~ Cl(IBM))
specifyModel(Cl(IBM) ~ Lag(Cl(IBM)))
}
}
\keyword{ misc }
\keyword{ datagen }
