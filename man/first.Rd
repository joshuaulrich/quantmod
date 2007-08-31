\name{first}
\alias{first}
\alias{last}
\title{ Return First or Last Element of Data }
\description{
A simple generic function to return the first or last
element or row of a vector or two-dimensional
data object.
}
\usage{
first(x,...)
last(x,...)

\method{first}{default}(x,...)

\method{last}{default}(x,...)
}
\arguments{
  \item{x}{ 1 or 2 dimensional data object }
  \item{\dots}{ additional args - unused }
}
\details{
A simple wrapper to extract the first or
last element of an object.
}
\value{
A one element/row data object
}
\author{ Jeffrey A. Ryan }
\examples{
first(1:100)
last(1:100)

data(LakeHuron)
first(LakeHuron)
last(LakeHuron)
}
\keyword{ utilities }
