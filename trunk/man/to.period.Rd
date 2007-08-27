\name{to.period}
\alias{to.period}
\alias{to.daily}
\alias{to.weekly}
\alias{to.monthly}
\alias{to.quarterly}
\alias{to.yearly}
\title{ Convert OHLC data to lower periodicity }
\description{
Convert an OHLC type object to a specified periodicity lower
than the given data object. For example, convert a daily
series to a monthly series, or a monthly series to an yearly one.

The result will contain, if applicable, the open and close for the
given period, as well as the maximum and minimum prices over the
new period, reflected in the new high and low, respectively.
}
\usage{
to.daily(x)
to.weekly(x)
to.monthly(x)
to.quarterly(x)
to.yearly(x)

to.period(x, period = months)

\method{to.period}{quantmod.OHLC}(x,period=months)
\method{to.period}{zoo}(x,period=months)
}
\arguments{
  \item{x}{ OHLC type object } 
  \item{period}{ period to convert to }
}
\details{
Essentially an easy and reliable way to convert daily data
into any commonly required periodicity. It is important to note
that all dates will be aligned to the \emph{end} of each period,
e.g. the row corresponding to August 2007 would become
'2007-08-31'.  This is different than what would be downloaded
from yahoo as a monthly series - as yahoo illogically using the
\emph{beginning} of the month to signify a month.

It is not possible to convert a series from a lower periodicity
to a higher periodicity - e.g. weekly to daily, as that would
require magic.
}
\value{
An object of the same type as \code{x}, with new periodicity.
}
\author{ Jeffrey A. Ryan }
\seealso{ \code{\link{as.quantmod.OHLC}}, \code{\link{getSymbols}} }
\keyword{ utilities }
