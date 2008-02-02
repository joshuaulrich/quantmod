\name{options.expiry}
\alias{options.expiry}
\alias{futures.expiry}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Calculate Contract Expirations }
\description{
Return the index of the contract expiration date.  The third Friday of the
month for options, the last third Friday of the quarter for futures.
}
\usage{
options.expiry(x)
futures.expiry(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a time-indexed zoo object }
}
\details{
Designed to be used within a charting context via \code{addExpiry}, the values returned
are based on the description above. Exceptions, though rare, are not accounted for.
}
\value{
A numeric vector of values to index on.
}
\references{ ~put references to the literature/web site here ~ }
\author{ Jeffrey A. Ryan }
\note{ 
There is currently no accounting for holidays that may interfere with
the general rule. Additionally all efforts have been focused on
US equity and futures markets.
}
\seealso{ \code{\link{addExpiry}} }
\examples{
\dontrun{
getSymbols("AAPL")

options.expiry(AAPL)
futures.expiry(AAPL)

AAPL[options.expiry(AAPL)]
}
}
\keyword{ utilities }% __ONLY ONE__ keyword per line
