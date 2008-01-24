\name{addADX}
\alias{addADX}
\title{ Add Directional Movement Index }
\description{
Add Directional Movement Index
}
\usage{
addADX(n = 14, maType="EMA", wilder=TRUE)
}
\arguments{
  \item{n}{ periods to use for DX calculation}
  \item{maType}{ moving average type }
  \item{wilder}{ should Welles Wilder EMA be used? }
}
\details{
See 'ADX' in \pkg{TTR} for specific details and references.
}
\value{
An ADX indicator will be draw in a new window on the current
chart.  A chobTA object will be returned silently.
}
\references{ see ADX in \pkg{TTR} written by Josh Ulrich }
\author{Jeffrey A. Ryan }
\seealso{ \code{\link{addTA}} }
\examples{
\dontrun{
addADX()
}
}
\keyword{utilities}
