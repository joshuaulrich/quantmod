\name{modelSignal}
\alias{modelSignal}
\title{ Extract Model Signal Object }
\description{
Extract model signal object from quantmodResults object
as an object of class \code{zoo}.
}
\usage{
modelSignal(x)
}
\arguments{
  \item{x}{ object of class \code{quantmodResults} }
}
\details{
For use after a call to \code{tradeModel} to extract the generated
signal of a given \code{quantmod} model. Normally this would
not need to be called by the end user unless he was manually
post processing the trade results.
}
\value{
A \code{zoo} object indexed by signal dates.
}
\author{ Jeffrey A. Ryan }
\seealso{  \code{\link{tradeModel}} }
\keyword{ utilities }
