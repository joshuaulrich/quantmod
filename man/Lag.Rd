\name{Lag}
\alias{Lag}
\alias{Lag.quantmod.OHLC}
\alias{Lag.zoo}
\alias{Lag.data.frame}
\alias{Lag.numeric}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Lag a Time Series }
\description{
Create a lagged series from data, with \code{NA} used to fill.
}
\usage{
Lag(x, k = 1)

\method{Lag}{quantmod.OHLC}(x, k = 1)

\method{Lag}{zoo}(x, k = 1)

\method{Lag}{data.frame}(x, k = 1)

\method{Lag}{numeric}(x, k = 1)
}
\arguments{
  \item{x}{ vector or series to be lagged }
  \item{k}{ periods to lag. }
}
\details{
Shift series k-periods down, prepending \code{NA}s to front
of series.

Specifically designed to handle \code{quantmod.OHLC} and
\code{zoo} series within the \code{quantmod} workflow.

If no S3 method is found, a call to \code{lag} in \pkg{base}
is made.
}
\value{
The original \code{x} prepended with \code{k} \code{NA}s
and missing the trailing \code{k} values.

The returned series maintains the number of obs. of the
original.
}
\author{ Jeffrey A. Ryan }
\note{ 
This function differs from \code{lag} by returning 
the original series modified, as opposed to simply changing
the time series properties.  It differs from the like
named \code{Lag} in the \pkg{Hmisc} as it deals primarily
with time-series like objects.

It is important to realize that if there is no applicable
method for \code{Lag}, the value returned will be from 
\code{lag} in \pkg{base}.  That is, coerced to \code{'ts'}
if necessary, and subsequently shifted.
}
\seealso{ \code{\link{lag}} }
\examples{
Stock.Close <- c(102.12,102.62,100.12,103.00,103.87,103.12,105.12)
Close.Dates <- as.Date(c(10660,10661,10662,10665,10666,10667,10668),origin="1970-01-01")
Stock.Close <- zoo(Stock.Close,Close.Dates)

Lag(Stock.Close)        #lag by 1 period
Lag(Stock.Close,k=1)    #same
Lag(Stock.Close,k=1:3)  #lag 1,2 and 3 periods

}
\keyword{ ts }
\keyword{ datagen }
\keyword{ misc }
\concept{ trading }
