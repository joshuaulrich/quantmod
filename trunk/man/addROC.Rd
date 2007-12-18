\name{addROC}
\alias{addROC}
\title{ Add Rate Of Change to Chart }
\description{
Add Rate Of Change indicator to chart.
}
\usage{
addROC(n = 1, type = c("discrete", "continuous"), col = "red")
}
\arguments{
  \item{n}{ periods }
  \item{type}{ compounding type }
  \item{col}{ line color (optional) }
}
\details{
See 'ROC' in \pkg{TTR} for specific details and references.
}
\value{
A ROC indicator will be draw in a new window on the current
chart.  A chobTA object will be returned silently.
}
\references{ see ROC in \pkg{TTR} written by Josh Ulrich }
\author{Jeffrey A. Ryan }
\seealso{ \code{\link{addTA}} }
\examples{
\dontrun{
addROC()
}
}
\keyword{utilities}
