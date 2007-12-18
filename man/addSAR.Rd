\name{addSAR}
\alias{addSAR}
\title{ Add Parabolic Stop and Reversal to Chart }
\description{
Add Parabolic Stop and Reversal indicator overlay to chart. 
}
\usage{
addSAR(accel = c(0.02, 0.2), col = "blue")
}
\arguments{
  \item{accel}{ Accelleration factors - see SAR }
  \item{col}{ color of points (optional) }
}
\details{
see 'SAR' in \pkg{TTR} for specific details and references.
}
\value{
A SAR overlay will be drawn on the current
chart.  A chobTA object will be returned silently.
}
\references{ see SAR in \pkg{TTR} written by Josh Ulrich }
\author{ Jeffrey A. Ryan }
\seealso{ \code{\link{addTA}}}
\examples{
\dontrun{
addSAR()
}
}
\keyword{ utilities }
