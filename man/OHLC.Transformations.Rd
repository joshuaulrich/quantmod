\name{OHLC.Transformations}
\alias{OHLC.Transformations}
\alias{Ad}
\alias{Cl}
\alias{ClCl}
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
\alias{HLC}
\alias{OHLC}
\alias{seriesHi}
\alias{seriesLo}
\alias{seriesInc}
\alias{seriesDec}
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

\code{HLC} extracts the High, Low, and Close columns.
\code{OHLC} extracts the Open, High, Low, and Close columns.

These functions are merely to speed the model
specification process.  All columns may also be extracted
through standard R methods.

Assignment will not work at present.
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

OpCl(x)
ClCl(x)
HiCl(x)
LoCl(x)
LoHi(x)
OpHi(x)
OpLo(x)
OpOp(x)

HLC(x)
OHLC(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ A data object with columns containing
   data to be extracted. }
}
\details{
Internally, the code uses grep to locate the appropriate
columns. Therefore it is necessary to use inputs with
column names matching the requirements in the description
section, though the exact naming convention is not as important.
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
