\name{Lag}
\alias{Lag}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Lag a Time Series }
\description{
Create a lagged series from data. Can return multiple lags if
given a vector for \code{k}.  Always returns a series with the
same length as the original, with \code{NA}'s padding.
}
\usage{
Lag(x, k = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ \code{vector} or series to be lagged }
  \item{k}{ periods to lag. Multiple periods can be passed as vector }
}
\details{
Performs a basic lag of a data series, shifting the values down
k-periods.  Specifying multiple values for \code{k} (e.g.
\code{k=c(1,3,5)}), will return an object with the number of
columns corresponding to the number of different lags
specified.
}
\value{
A lagged data series, or columns of lagged data series, while
maintaining the \code{class} of the original.
}
\author{ Jeffrey A. Ryan }
\note{ 
The primary reason for yet another lag function is to offer a
more natural function for financial lags.  Additionally it was
desired to be able to specify multiple lags within one call, 
maximizing the vectorization approach in \R.
}
\seealso{ \code{\link{lag}} }
\examples{

}
\keyword{ ts }
\keyword{ datagen }
\keyword{ misc }
