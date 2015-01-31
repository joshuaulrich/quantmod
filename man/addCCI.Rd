\name{addADX}
\alias{addADX}
\title{ Add Commodity Channel Index }
\description{
Add Commodity Channel Index
}
\usage{
addCCI(n = 20, maType="SMA", c=0.015)
}
\arguments{
  \item{n}{ periods to use for DX calculation}
  \item{maType}{ moving average type }
  \item{c}{ Constant to apply to the mean deviation.}
}
\details{
See 'CCI' in \pkg{TTR} for specific details and references.
}
\value{
An CCI indicator will be draw in a new window on the current
chart.  A chobTA object will be returned silently.
}
\references{ see CCI in \pkg{TTR} written by Josh Ulrich }
\author{Jeffrey A. Ryan }
\seealso{ \code{\link{addTA}} }
\examples{
\dontrun{
addCCI()
}
}
\keyword{utilities}
