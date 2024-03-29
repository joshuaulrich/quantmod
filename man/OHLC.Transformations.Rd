\name{OHLC.Transformations}
\alias{OHLC.Transformations}
\alias{getPrice}
\alias{Ad}
\alias{Cl}
\alias{ClCl}
\alias{ClOp}
\alias{Hi}
\alias{HiCl}
\alias{Lo}
\alias{LoCl}
\alias{LoHi}
\alias{Op}
\alias{OpCl}
\alias{OpHi}
\alias{OpLo}
\alias{OpOp}
\alias{Vo}
\alias{HL}
\alias{HLC}
\alias{OHLC}
\alias{OHLCV}
\alias{seriesHi}
\alias{seriesLo}
\alias{seriesIncr}
\alias{seriesDecr}
\alias{seriesAccel}
\alias{seriesDecel}
\title{ Extract and Transform OHLC Time-Series Columns }
\description{
Extract (transformed) data from a suitable OHLC object.
Column names must contain the
complete description - either \dQuote{Open}, \dQuote{High},
\dQuote{Low}, \dQuote{Close},
\dQuote{Volume}, or \dQuote{Adjusted} - though may
also contain additional characters.  This is the default for objects
returned from most \code{getSymbols} calls.

In the case of functions consisting of combined
Op, Hi, Lo, Cl (e.g. \code{ClCl(x)}) the one period
transformation will be applied.

For example, to return the Open to Close of a
object it is
possible to call \code{OpCl(x)}. If multiple periods
are desired a call to the function \code{Delt} is 
necessary.

\code{seriesLo} and \code{seriesHi} will return the
low and high, respectively, of a given series.

\code{seriesAccel}, \code{seriesDecel}, \code{seriesIncr},
and \code{seriesDecr}, return a vector of logicals
indicating if the series is accellerating, decellerating,
increasing, or decreasing. This is managed by \code{diff},
which provides NA fill and suitable re-indexing.  These
are here to make trade rules easier to read.

\code{HL} extracts the High and Low columns.
\code{HLC} extracts the High, Low, and Close columns.
\code{OHLC} extracts the Open, High, Low, and Close columns.

These functions are merely to speed the model
specification process.  All columns may also be extracted
through standard R methods.

Assignment will not work at present.

\code{getPrice} will attempt to extract price column(s) from a time series,
using sensible defaults. Additionally, the user may provide by symbol and price
preference.
}
\usage{
Op(x)
Hi(x)
Lo(x)
Cl(x)
Vo(x)
Ad(x)

seriesHi(x)
seriesLo(x)
seriesIncr(x, thresh=0, diff.=1L)
seriesDecr(x, thresh=0, diff.=1L)

OpCl(x)
ClCl(x)
ClOp(x)
HiCl(x)
LoCl(x)
LoHi(x)
OpHi(x)
OpLo(x)
OpOp(x)

HL(x)
HLC(x)
OHLC(x)
OHLCV(x)

getPrice(x, symbol=NULL, prefer=NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ A data object with columns containing
   data to be extracted. }
  \item{thresh}{ noise threshold (seriesIncr/seriesDecr) }
  \item{diff.}{ differencing (seriesIncr/seriesDecr) }
  \item{symbol}{ text string containing the symbol to extract }
  \item{prefer}{ price type preference (see Details) }
  \item{\dots}{ not currently used }
}
\details{
Internally, the code uses grep to locate the appropriate
columns. Therefore it is necessary to use inputs with
column names matching the requirements in the description
section, though the exact naming convention is not as important.

\code{prefer} can be used with \code{getPrice} to extract many commonly used
financial time series prices descriptions (e.g. open, high, low, close, bid,
ask/offer, midpoint, trade, price). If the value of \code{prefer} does not
match one of the currently supported types, it will be matched against the
object column names using \code{grep}.
}
\value{
Returns an object of the same class as the original
series, with the appropriately column names
if applicable and/or possible.  The only exceptions are for \code{quantmod.OHLC}
objects which will be returned as \code{zoo} objects, and calls to
\code{seriesLo} and \code{seriesHi} which \emph{may} return a numeric
value instead of the original object type.
}
\author{ Jeffrey A. Ryan }
\seealso{ \code{\link{specifyModel}}  }
\examples{
\dontrun{
getSymbols('IBM',src='yahoo')
Ad(IBM)
Cl(IBM)
ClCl(IBM)

seriesHi(IBM)
seriesHi(Lo(IBM))

removeSymbols('IBM')
}
}
\keyword{ utilities }
