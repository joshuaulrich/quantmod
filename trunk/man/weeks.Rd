\name{weeks}
\alias{weeks}
\alias{years}
\alias{weeks.zoo}
\alias{years.zoo}
\title{ Extract Weeks, Years of a Time Series Object }
\description{
Extract weeks and years. This is a generic function, methods for zoo
classes are documented here
}
\usage{
\method{weeks}{zoo}(x, abbreviate)

\method{years}{zoo}(x, abbreviate)
}
\arguments{
  \item{x}{ zoo object with index of class Date }
  \item{abbreviate}{ abbreviate the results }
}
\details{
Behaves exactly as weekdays, months and quarters from \pkg{base}
with the obvious difference in that the method extracts the weeks
or years of the object.
}
\value{
A numeric vector giving the week number of the year corresponding to each
observed date for \code{weeks}, and year in
CCYY-MM-DD format for \code{years}.
}
\references{ zoo }
\author{ Jeffrey A. Ryan }
\note{ 
The zoo object's index must be of class Date, or coercible to such.
}
\seealso{ \code{\link{weekdays}}, \code{\link{Date}} }
\examples{
zoo.data <- zoo(rnorm(31)+10,as.Date(13514:13744,origin="1970-01-01"))
weeks(zoo.data)
years(zoo.data)
}
\keyword{ utilities }
