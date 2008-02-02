\name{period.sum}
\alias{period.sum}
\title{ Calculate Sum By Period }
\description{
Calculate a sum for each period of INDEX. Essentially a rolling
application of sum over a series of non-overlapping sections.
}
\usage{
period.sum(x, INDEX)
}
\arguments{
  \item{x}{ a univariate data object }
  \item{INDEX}{ a vector of breakpoints to calculate sum on }
}
\details{
Used to calculate a sum per period given an arbitrary
index of sections to be calculated over. This is an optimized function
for sum.  There are additionally optimized versions for min, max, and
prod. 
}
\value{
A numeric vector of sums per period.
}
\author{ Jeffrey A. Ryan }
\seealso{ \code{\link{period.max}},
          \code{\link{period.min}}, \code{\link{period.prod}} }
\examples{
period.sum(c(1,1,4,2,2,6,7,8,-1,20),c(0,3,5,8,10))
}
\keyword{ utilities }% __ONLY ONE__ keyword per line
