\name{addMACD}
\alias{addMACD}
\title{ Add Moving Average Convergence Divergence to Chart }
\description{
Add Moving Average Convergence Divergence indicator to chart.
}
\usage{
addMACD(fast = 12, slow = 26, signal = 9, type = "EMA", histogram = TRUE, col)
}
\arguments{
  \item{fast}{ fast period }
  \item{slow}{ slow period }
  \item{signal}{ signal period }
  \item{type}{ type of MA to use. Single values will be replicated }
  \item{histogram}{ include histogram }
  \item{col}{ colors to use for lines (optional) }
}
\details{
See and 'MACD' in \pkg{TTR} for specific
details and implementation references.
}
\value{
A MACD indicator will be draw in a new window on the
current chart.  A chobTA object will be returned silently.
}
\references{ see MACD in \pkg{TTR} written by Josh Ulrich}
\author{Jeffrey A. Ryan }
\seealso{ \code{\link{addTA}} }
\examples{
\dontrun{
addMACD()
}
}
\keyword{ utilities }
