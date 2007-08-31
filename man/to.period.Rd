\name{to.period}
\alias{to.period}
\alias{to.weekly}
\alias{to.monthly}
\alias{to.quarterly}
\alias{to.yearly}
\title{ Convert OHLC data to lower periodicity }
\description{
Convert an OHLC or univariate zoo object to a specified periodicity lower
than the given data object. For example, convert a daily
series to a monthly series, or a monthly series to an yearly one.

The result will contain, if applicable, the open and close for the
given period, as well as the maximum and minimum prices over the
new period, reflected in the new high and low, respectively.
}
\usage{
to.weekly(x)
to.monthly(x)
to.quarterly(x)
to.yearly(x)

to.period(x, period = months, ...)
}
\arguments{
  \item{x}{ OHLC type object } 
  \item{period}{ period to convert to }
  \item{\dots}{ additional arguments }
}
\details{
Essentially an easy and reliable way to convert daily data
into any commonly required periodicity. It is important to note
that all dates will be aligned to the \emph{end} of each period,
e.g. the row corresponding to August 2007 would become
'2007-08-31'.  This is different than what would be downloaded
from yahoo as a monthly series - as yahoo illogically using the
\emph{beginning} of the month to signify a month - i.e. the first
date available, not the 1st!

It is also possible to pass a single time series, such as
a univariate exchange rate, and return an OHLC object of
lower frequency - e.g. the weekly OHLC of the daily series.

It is not possible to convert a series from a lower periodicity
to a higher periodicity - e.g. weekly to daily, as that would
require magic.
}
\value{
An object of the same type as \code{x}, with new periodicity.
}
\examples{
\dontrun{
# download daily US/EU exchange rate from
# the FRED system
getSymbols("DEXUSEU",src="FRED")
getSymbols("QQQQ",src="yahoo")

# look at the data : )
DEXUSEU   # univariate time series
QQQQ      # OHLC already

# now it's a yearly OHLC
dex1 <- to.yearly(DEXUSEU)

# first monthly, then to yearly
dex2 <- to.yearly(to.monthly(DEXUSEU))

identical(dex1,dex2) # it's the same!

q1 <- to.yearly(QQQQ)
q2 <- to.yearly(to.monthly(QQQQ))

# these don't match - sometimes dates
# are off depending on when cutoffs
# occur - so BE CAREFUL when converting
# a converted object!
identical(q1,q2)    
}
}
\author{ Jeffrey A. Ryan }
\seealso{ \code{\link{as.quantmod.OHLC}}, \code{\link{getSymbols}},
          \code{\link{apply.monthly}}}
\keyword{ utilities }
