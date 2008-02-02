\name{minutes}
\alias{minutes}
\alias{seconds}
\alias{minutes3}
\alias{minutes5}
\alias{minutes10}
\alias{minutes15}
\alias{minutes30}
\alias{hours}
\alias{days}
\alias{seconds.zoo}
\alias{minutes.zoo}
\alias{hours.zoo}
\alias{days.zoo}
\title{ Extract Minutes, Hours, or Days of a High Frequency Time Series Object }
\description{
Extract the minutes or hours of a day, or days since the beginning of the year 
This function is a generic, with currently a single method implemented
for objects of class \code{zoo}.
}
\usage{
\method{seconds}{zoo}(x,k=1,...)

seconds(x,k=1,...)

\method{minutes}{zoo}(x,k=1,...)

minutes(x,k=1,...)
minutes3(x,...)
minutes5(x,...)
minutes10(x,...)
minutes15(x,...)
minutes30(x,...)

\method{hours}{zoo}(x,...)

\method{days}{zoo}(x,...)
}
\arguments{
  \item{x}{ zoo object with index of class Date }
  \item{k}{ used to identify which \dQuote{bar} obs. belongs to }
  \item{\dots}{ additional unused arguments }
}
\details{
Behaves exactly as weekdays, months and quarters from \pkg{base}
with the obvious difference in that the method extracts the
minutes or hours of a day, and day of the year for a given object.

\code{minutes3},
\code{minutes5},
\code{minutes10},
\code{minutes15},
\code{minutes30} are wrappers to the appropriate minutes
time scale.
}
\value{
\code{minutes} returns the minute of the day as a character [0-59]

\code{hours} returns the hour of the day as a character [0-23]

\code{days} returns the day of the year as a character [001-366]
}
\author{ Jeffrey A. Ryan }
\note{
The zoo object's index must be of class POSIXct or POSIXlt,
or coercible to such, and have sufficient time detail.
}
\seealso{ \code{\link{weekdays}}, \code{\link{Date}}, \code{\link{POSIXlt}},
           }
\examples{
start.time <- as.POSIXlt("2004-12-31 00:08:30")
minute.data <- zoo(rnorm(251),seq(start.time,start.time+15000,by=60))
minutes(minute.data)
minutes5(minute.data)
hours(minute.data)
days(minute.data)
}
\keyword{ utilities }
