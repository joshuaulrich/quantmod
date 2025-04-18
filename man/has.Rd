\name{has.OHLC}
\alias{has.Ask}
\alias{has.Bid}
\alias{has.Price}
\alias{has.Qty}
\alias{has.Trade}
\alias{is.BBO}
\alias{is.TBBO}
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
\alias{has.HL}
\alias{is.HL}
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

is.HL(x)
has.HL(x, which = FALSE)

has.Op(x, which = FALSE)
has.Hi(x, which = FALSE)
has.Lo(x, which = FALSE)
has.Cl(x, which = FALSE)
has.Vo(x, which = FALSE)
has.Ad(x, which = FALSE)

is.BBO(x)
is.TBBO(x)

has.Ask(x, which = FALSE)
has.Bid(x, which = FALSE)
has.Price(x, which = FALSE)
has.Qty(x, which = FALSE)
has.Trade(x, which = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ data object }
  \item{which}{ display position of match }
}
\details{
Mostly used internally by \pkg{quantmod}, they can be useful
for checking whether an object
can be used in OHLC requiring functions like \code{Op},
\code{OpCl}, etc.

Columns names must contain the full description of data,
that is, Open, High, Low, Close, Volume or Adjusted. Abbreviations
will return \code{FALSE} (or \code{NA} when \code{which = TRUE}). See
\code{\link{quantmod.OHLC}} for details of \pkg{quantmod}
naming conventions.

The "is" functions only return \code{TRUE} when the objects has the relevant
column names:
\itemize{
\item \code{is.HL}: High, Low
\item \code{is.HLC}: High, Low, Close
\item \code{is.OHLC}: Open, High, Low, Close
}
The search for relevant column names is not case sensitive.
Any additional columns in the object does not affect the return value.
}
\value{
A logical value indicating success or failure by default.

If \code{which = TRUE}, a numeric value representing the column position
will be returned.

\code{is.OHLC}, \code{is.HL}, and \code{is.HLC} return a single logical value,
either \code{TRUE} or \code{FALSE}.
}
\author{ Jeffrey A. Ryan }
\seealso{ \code{\link{quantmod.OHLC}},\code{\link{OHLC.Transformations}} }
\examples{
\dontrun{
getSymbols("AAPL")

is.OHLC(AAPL)
has.OHLC(AAPL)

has.Ad(AAPL)

}
}
\keyword{ utilities }
