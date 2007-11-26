\name{apply.monthly}
\alias{apply.daily}
\alias{apply.weekly}
\alias{apply.monthly}
\alias{apply.quarterly}
\alias{apply.yearly}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Apply Function over Calendar Periods }
\description{
Apply a specified function to each distinct period in a given
time series object.
}
\usage{
apply.daily(x, FUN)
apply.weekly(x, FUN)
apply.monthly(x, FUN)
apply.quarterly(x, FUN)
apply.yearly(x, FUN)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ an object of class zoo }
  \item{FUN}{ an \R function }
}
\details{
Simple mechanism to apply a function to non-overlapping
time periods, e.g. weekly, monthly, etc. Different from rolling
functions in that this will subset the data based on the specified
time period (implicit in the call), and return a vector of values
for each period in the original data.

Essentially a wrapper to the \pkg{quantmod} functions
\code{breakpoints} and \code{period.apply}, mainly as
a convenience.
}
\value{
A vector of results produced by \code{FUN}, corresponding
to the appropriate periods.
}
\author{ Jeffrey A. Ryan }
\seealso{ \code{\link{breakpoints}}, \code{\link{period.apply}},
          \code{\link{to.monthly}} }
\examples{
zoo.ts <- zoo(rnorm(231),as.Date(13514:13744,origin="1970-01-01"))

start(zoo.ts)
end(zoo.ts)

apply.monthly(zoo.ts,sd)
apply.monthly(zoo.ts,function(x) var(x))
}
\keyword{ utilities }
