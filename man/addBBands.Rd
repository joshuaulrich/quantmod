\name{addBBands}
\alias{addBBands}
\title{ Add Bollinger Bands to Chart }
\description{
Add Bollinger Bands to current chart.
}
\usage{
addBBands(n = 20, ma = "SMA", sd = 2, on = -1)
}
\arguments{
  \item{n}{ number of moving average periods }
  \item{ma}{ type of moving average to be used }
  \item{sd}{ number of standard deviations }
  \item{on}{ which figure area of chart to apply to }
}
\details{
See bollingerBands in \pkg{TTR} for specific details
as to implementation and references.
}
\value{
Bollinger Bands will be drawn, or scheduled to be drawn,
on the current chart.  A
chobTA object will be returned silently.
}
\references{ See bollingerBands in \pkg{TTR} written by Josh Ulrich }
\author{ Jeffrey A. Ryan }
\seealso{ \code{\link{addTA}} }
\examples{
\dontrun{
addBBands()
}
}
\keyword{ utilities }
