\name{addExpiry}
\alias{addExpiry}
\title{ Add Contract Expiration Bars to Chart }
\description{
Apply options or futures expiration vertical bars
to current chart.
}
\usage{
addExpiry(type = "options", lty = "dotted")
}
\arguments{
  \item{type}{ options or futures expiration }
  \item{lty}{ type of lines to draw }
}
\details{
See options.expiry and futures.expiry in \pkg{quantmod}
for details and limitations.
}
\value{
Expiration lines will be drawn at appropriate dates.
A chibTA object will be returned silently.
}
\author{ Jeffrey A. Ryan }
\seealso{ \code{\link{addTA}} }
\examples{
\dontrun{
addExpiry()
}
}
\keyword{ utilities }
