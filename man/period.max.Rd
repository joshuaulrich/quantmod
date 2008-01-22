\name{period.max}
\alias{period.max}
\title{ Calculate Max By Period }
\description{
Calculate a maximum for each period of INDEX. Essentially a rolling
application of maximum over a series of non-overlapping sections.
}
\usage{
period.max(x, INDEX)
}
\arguments{
  \item{x}{ a univariate data object }
  \item{INDEX}{ a vector of breakpoints to calculate maximum on }
}
\details{
Used to calculate a maximum per period given an arbitrary
index of sections to be calculated over. This is an optimized function
for maximum.  There are additionally optimized versions for min, sum, and
prod. 
}
\value{
A numeric vector of maximums per period.
}
\author{ Jeffrey A. Ryan }
\seealso{ \code{\link{period.sum}},
          \code{\link{period.min}}, \code{\link{period.prod}} }
\examples{
period.max(c(1,1,4,2,2,6,7,8,-1,20),c(0,3,5,8,10))
}
\keyword{ utilities }% __ONLY ONE__ keyword per line
