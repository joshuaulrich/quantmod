\name{period.min}
\alias{period.min}
\title{ Calculate Min By Period }
\description{
Calculate a minimum for each period of INDEX. Essentially a rolling
application of minimum over a series of non-overlapping sections.
}
\usage{
period.min(x, INDEX)
}
\arguments{
  \item{x}{ a univariate data object }
  \item{INDEX}{ a vector of breakpoints to calculate maximum on }
}
\details{
Used to calculate a minimum per period given an arbitrary
index of sections to be calculated over. This is an optimized function
for minimum  There are additionally optimized versions for max, sum, and
prod. 
}
\value{
A numeric vector of minimums per period.
}
\author{ Jeffrey A. Ryan }
\seealso{ \code{\link{period.sum}},
          \code{\link{period.max}}, \code{\link{period.prod}} }
\examples{
period.min(c(1,1,4,2,2,6,7,8,-1,20),c(0,3,5,8,10))
}
\keyword{ utilities }% __ONLY ONE__ keyword per line
