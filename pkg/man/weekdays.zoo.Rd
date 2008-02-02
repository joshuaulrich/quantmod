\name{weekdays.zoo}
\alias{weekdays.zoo}
\alias{months.zoo}
\alias{quarters.zoo}
\alias{weekdays.timeSeries}
\alias{months.timeSeries}
\alias{quarters.timeSeries}
\title{ Extract Parts of a Time Series Object }
\description{
Extract the minute, hour, day, weekday, month or quarter. This documents the methods
for the zoo class with an index of class \code{Date}, or coercible to such.
}
\usage{
\method{weekdays}{zoo}(x,abbreviate=FALSE)
\method{weekdays}{timeSeries}(x,abbreviate=FALSE)

\method{months}{zoo}(x,abbreviate=FALSE)
\method{months}{timeSeries}(x,abbreviate=FALSE)

\method{quarters}{zoo}(x,abbreviate=FALSE)
\method{quarters}{timeSeries}(x,abbreviate=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a object with index that inherits \code{Date} or \code{POSIXt} }
  \item{abbreviate}{ logical. Should names be abbreviated? }
}
\value{
\code{weekdays} and \code{months} return a character vector of the
names, or for \code{abbreviate=TRUE} a character vector of month
numbers for \code{months.zoo}.

\code{quarters.zoo} returns a numeric vector of \code{1}
to \code{4} for \code{abbreviate=TRUE}, else a character vector
containing \dQuote{Q1},\dQuote{Q2},\dQuote{Q3}, and/or \dQuote{Q4}
}
\author{ Jeffrey A. Ryan }
\seealso{ \code{\link{DateTimeClasses}}, \code{\link{quarters}},
          \code{\link{weekdays}}, \code{\link{months}} }
\examples{
zoo.data <- zoo(rnorm(31)+10,as.Date(13514:13744,origin="1970-01-01"))
weekdays(zoo.data)
months(zoo.data)
quarters(zoo.data)
}
\keyword{ utilities }
