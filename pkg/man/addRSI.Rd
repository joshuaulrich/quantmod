\name{addRSI}
\alias{addRSI}
\title{ Add Relative Strength Index to Chart }
\description{
Add a Relative Strength Index indicator to chart.
}
\usage{
addRSI(n = 14, maType = "EMA", wilder = TRUE)
}
\arguments{
  \item{n}{ periods }
  \item{maType}{ type of MA to use }
  \item{wilder}{ use wilder (see EMA) }
}
\details{
see 'RSI' in \pkg{TTR} for specific details and references.
}
\value{
An RSI indicator will be draw in a new window on the current
chart.  A chobTA object will be returned silently.
}
\references{ see RSI in \pkg{TTR} written by Josh Ulrich }
\author{ Jeffrey A. Ryan }
\seealso{ \code{\link{addTA}} }
\examples{
\dontrun{
addRSI()
}
}
\keyword{ utilities }
