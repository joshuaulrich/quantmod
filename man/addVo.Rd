\name{addVo}
\alias{addVo}
\title{ Add Volume to Chart }
\description{
Add Volume of a series, if available, to the current chart.
This is the default TA argument for all charting functions.
}
\usage{
addVo()
}
\details{
Add volume bars to current chart if data object contains
appropriate volume column.
}
\value{
Volume will be draw in a new window on the current
chart.  A chobTA object will be returned silently.
}
\author{ Jeffrey A. Ryan }
\seealso{ \code{\link{addTA}}}
\examples{
\dontrun{
addVo()
}
}
\keyword{ utilities }
