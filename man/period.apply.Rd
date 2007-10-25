\name{period.apply}
\alias{period.apply}
\title{ Apply Function Over Specified Interval }
\description{
Apply a specified function to data over a given
interval, where the interval is taken to be
the data from INDEX[k] to INDEX[k+1], for
k=1:length(INDEX).
}
\usage{
period.apply(x, INDEX, FUN, ...)
}
\arguments{
  \item{x}{ data to apply FUN to }
  \item{INDEX}{ numeric vector specifying indexing }
  \item{FUN}{ an argument of type \code{function} }
  \item{\dots}{ additional arguments for \code{FUN}}
}
\details{
Similar to the rest of the apply family, calculate
a specified functions value given a shifting set
of data values.

Useful for applying arbitrary functions over
time, as when INDEX is the result of a call
to breakpoints.
}
\value{
A numeric vector with length INDEX
}
\author{ Jeffrey A. Ryan }
\seealso{ \code{\link{breakpoints}} \code{\link{apply.monthly}}}
\examples{
zoo.data <- zoo(rnorm(31)+10,as.Date(13514:13744,origin="1970-01-01"))
bp <- breakpoints(zoo.data,by=weeks)
period.apply(zoo.data, INDEX=bp, FUN=function(x) sd(x))
period.apply(zoo.data, INDEX=bp, FUN=sd)                  #same

}
\keyword{ utilities }% __ONLY ONE__ keyword per line
