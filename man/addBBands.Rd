\name{addBBands}
\alias{addBBands}
\title{ Add Bollinger Bands to Chart }
\description{
Add Bollinger Bands to current chart.
}
\usage{
addBBands(n = 20, sd = 2, maType = "SMA", draw = 'bands', on = -1)
}
\arguments{
  \item{n}{ number of moving average periods }
  \item{maType}{ type of moving average to be used }
  \item{sd}{ number of standard deviations }
  \item{draw}{ indicator to draw: bands, percent, or width }
  \item{on}{ which figure area of chart to apply to }
}
\details{
The primary addition to this function call over the \pkg{TTR}
version is in the \code{draw} argument.  \sQuote{bands} will draw
standard Bollinger Bands, \sQuote{percent} will draw Bollinger \%b
and \sQuote{width} will draw Bolinger Bands Width.  The last two will
be drawn in new figure regions.

See bollingerBands in \pkg{TTR} for specific details
as to implementation and references.
}
\value{
Bollinger Bands will be drawn, or scheduled to be drawn,
on the current chart.  If \code{draw} is either percent or width
a new figure will be added to the current TA figures charted.

A chobTA object will be returned silently.
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
