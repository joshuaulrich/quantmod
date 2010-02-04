\name{has.OHLC}
\alias{has.Ad}
\alias{has.Op}
\alias{has.Hi}
\alias{has.Lo}
\alias{has.Cl}
\alias{has.Vo}
\alias{has.OHLC}
\alias{has.OHLCV}
\alias{is.OHLC}
\alias{is.OHLCV}
\alias{has.HLC}
\alias{is.HLC}
\title{ Check For OHLC Data }
\description{
A set of functions to check for appropriate
OHLC and HLC column names within a data object, as well
as the availability and position of those columns.
}
\usage{
is.OHLC(x)
has.OHLC(x, which = FALSE)

is.OHLCV(x)
has.OHLCV(x, which = FALSE)

is.HLC(x)
has.HLC(x, which = FALSE)

has.Op(x, which = FALSE)
has.Hi(x, which = FALSE)
has.Lo(x, which = FALSE)
has.Cl(x, which = FALSE)
has.Vo(x, which = FALSE)
has.Ad(x, which = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ data object }
  \item{which}{ disply position of match }
}
\details{
Mostly used internally by \pkg{quantmod}, they can be useful
for checking whether an object
can be used in OHLC requiring functions like \code{Op},
\code{OpCl}, etc.

Columns names must contain the full description of data,
that is, Open, High, Low, Close, Volume or Adjusted. Abbreviations
will return \code{FALSE} (or \code{NA} when which=TRUE). See
\code{\link{quantmod.OHLC}} for details of \pkg{quantmod}
naming conventions.

\code{is.OHLC} (and is.HLC, similarly) will only return TRUE is there are columns
for Open, High, Low and Close.  Additional columns will not affect
the value.
}
\value{
A logical value indicating success or failure by default.

If which=TRUE, a numeric value representing the column position
will be returned.

\code{is.OHLC} and \code{is.HLC} return a single value of TRUE or FALSE.
}
\author{ Jeffrey A. Ryan }
\seealso{ \code{\link{quantmod.OHLC}},\code{\link{OHLC.Transformations}} }
\examples{
\dontrun{
getSymbols("YHOO")

is.OHLC(YHOO)
has.OHLC(YHOO)

has.Ad(YHOO)

}
}
\keyword{ utilities }
