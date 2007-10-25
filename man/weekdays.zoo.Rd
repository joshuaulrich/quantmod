\name{weekdays.zoo}
\alias{weekdays.zoo}
\alias{months.zoo}
\alias{quarters.zoo}
\title{ Extract Parts of a zoo Object }
\description{
Extract the weekday, month or quarter. This documents the methods
for the zoo class.
}
\usage{
\method{weekdays}{zoo}(x,abbreviate=FALSE)

\method{months}{zoo}(x,abbreviate=FALSE)

\method{quarters}{zoo}(x,abbreviate=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a zoo object with index of class \code{Date} }
  \item{abbreviate}{ logical. Should names be abbreviated? }
}
\value{
\code{weekdays} and \code{months} return a character vector of the
names.

\code{quarters} returns a character vector of \code{Q1} to \code{Q4}
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
