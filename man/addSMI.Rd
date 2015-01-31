\name{addSMI}
\alias{addSMI}
\title{ Add Stochastic Momentum Indicator to Chart }
\description{
Add Stochastic Momentum Indicator to chart.
}
\usage{
addSMI(n=13,slow=25,fast=2,signal=9,ma.type="EMA")
}
\arguments{
  \item{n}{ periods }
  \item{slow}{ slow  }
  \item{fast}{ fast  }
  \item{signal}{ signal }
  \item{ma.type}{ MA tyep to use, recycled as necessary }
}
\details{
see 'SMI in \pkg{TTR} for specifics and references.
}
\value{
An SMI indicator will be draw in a new window on the current
chart.  A chobTA object will be returned silently.
}
\references{ see SMI in \pkg{TTR} written by Josh Ulrich }
\author{ Jeffrey A. Ryan }
\seealso{\code{\link{addTA}}}
\examples{
\dontrun{
addSMI()
}
}
\keyword{ utilities }
