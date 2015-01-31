\name{addWPR}
\alias{addWPR}
\title{ Add William's Percent R to Chart }
\description{
Add William's percent R indiator to the current chart.
}
\usage{
addWPR(n = 14)
}
\arguments{
  \item{n}{ periods }
}
\details{
see 'WPR' in \pkg{TTR} for details and references.
}
\value{
A William's percent R indicator will be draw in a new window on the current
chart.  A chobTA object will be returned silently.
}
\references{ see 'WPR' in \pkg{TTR} written by Josh Ulrich }
\author{ Jeffrey A. Ryan }
\seealso{ \code{\link{addTA}}}
\examples{
\dontrun{
addWPR()
}
}
\keyword{ utilities }
