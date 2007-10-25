\name{periodicity}
\alias{periodicity}
\title{ Approximate Series Periodicity }
\description{
Estimate the periodicity of a time series like
object by calculating the median time between
observations in days.
}
\usage{
periodicity(x, ...)
}
\arguments{
  \item{x}{ a zoo-like object with a Date based index }
  \item{\dots}{ unused }
}
\details{
A simple wrapper to quickly estimate the periodicity
of a given data. Returning a statement of the
median number of days between observations.
}
\value{
An object of class 'difftime'
}
\author{ Jeffrey A. Ryan }
\seealso{ \code{\link{difftime}} }
\note{
This should be extended to handle non-zoo objects,
though that hasn't happened : \
}
\examples{
zoo.ts <- zoo(rnorm(231),as.Date(13514:13744,origin="1970-01-01"))
periodicity(zoo.ts)
}
\keyword{ utilities }
