\name{breakpoints}
\alias{breakpoints}
\title{ Locate Breakpoints by Date }
\description{
Extract index values of a given zoo object, where the
returned index is equal to the last date of each 
recurring period.
}
\usage{
breakpoints(x, by = c(weekdays, weeks, months, quarters, years),
            ...)
}
\arguments{
  \item{x}{ zoo object with index of class \code{Date} }
  \item{by}{ period to be indexed from }
  \item{\dots}{ currently unused }
}
\details{
Given a \code{zoo} object with the appropriate
index type, \code{breakpoints} will return
a numeric vector corresponding to the final
rows in each period specified.

For example, \code{by=years} would return the
index values matching the last observation
in each calendar year. Similarly, \code{by=weeks}
would match the last observations in any given week.

The final index returned
corresponds to the last observation in the
data object, as this may be useful for calculating
\dQuote{values to date}.
}
\value{
A numeric vector which can be used to extract
rows from the given data object.
}
\author{ Jeffrey A. Ryan }
\note{
The last value in the returned vector \emph{most likely}
does not mark the end of the specified period,
rather it marks the end of the data.
}
\examples{
zoo.data <- zoo(rnorm(31)+10,as.Date(13514:13744,origin="1970-01-01"))
breakpoints(zoo.data,by=weeks)

bp <- breakpoints(zoo.data,by=quarters)
zoo.data[bp]
}
\keyword{ utilities }% __ONLY ONE__ keyword per line
