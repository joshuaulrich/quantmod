\name{periodicity}
\alias{periodicity}
\title{ Approximate Series Periodicity }
\description{
Estimate the periodicity of a time series like
object by calculating the median time between
observations in days.
}
\usage{
periodicity(x, ...)
}
\arguments{
  \item{x}{ a zoo-like object with a time based index }
  \item{\dots}{ unused }
}
\details{
A simple wrapper to quickly estimate the periodicity
of a given data. Returning a list of the
median number of days between observations
as a difftime object, the numerical difference,
the units of measurement,
and the derived scale of the data as a string. 

The time index currently must be of either \code{Date}
or \code{POSIX} class.

The only list item of note is the \code{scale}.
This is an estimate of the periodicity of the data
in common terms - e.g. 7 day dialy data is best
described as \sQuote{weekly}, and would be returned as
such.

Possible \code{scale} values are:

\sQuote{minute},\sQuote{hourly},
\sQuote{daily},\sQuote{weekly},
\sQuote{monthly},\sQuote{quarterly},
and \sQuote{yearly}.
}
\value{
A list containing the \code{difftime} object,
frequency, units, and suitable scale.
}
\author{ Jeffrey A. Ryan }
\seealso{ \code{\link{difftime}} }
\note{
This should be extended to handle non-zoo objects,
though that hasn't happened. 
}
\examples{
zoo.ts <- zoo(rnorm(231),as.Date(13514:13744,origin="1970-01-01"))
periodicity(zoo.ts)
}
\keyword{ utilities }
