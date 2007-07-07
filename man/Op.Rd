\name{quantmod.OHLC Transformations}
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
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Extract and Transform quantmod.OHLC Columns }
\description{
Extract (transformed) data from an object of class 
\code{quantmod.OHLC}. Column names must contain the
complete description - either Open, High, Low, Close,
Volume, or Adjusted.  This is the default for objects
of class \code{quantmod.OHLC}
\cr
In the case of functions consisting of combined
Op, Hi, Lo, Cl (e.g. \code{ClCl(x)}) the one period
transformation will be applied.

For example, to return the Open to Close of a
particular \code{quantmod.OHLC} object it is
necessary to call \code{OpCl(x)}. If multiple periods
are desired a call to the function \code{Delt} is 
necessary.

These functions are merely for speed the model 
specification process.  All columns may also be extracted
through standard R methods
}
\usage{
Op(x)
Hi(x)
Lo(x)
Cl(x)
Vo(x)
Ad(x)

OpCl(x)
ClCl(x)
HiCl(x)
LoCl(x)
LoHi(x)
OpHi(x)
OpLo(x)
OpOp(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ An object of class \code{quantmod.OHLC}}
}
\details{
Internally, the code uses grep to locate the appropriate
columns. Therefore it is necessary to use inputs with
column names matching the requirements in the description
section
}
\value{
Returns an object of class \code{quantmod.OHLC}
}
\author{ Jeffrey A. Ryan }
\seealso{ \code{\link{specifyModel}}  }
\examples{
\dontrun{
getSymbols('IBM',src='yahoo')
Ad(IBM)
Cl(IBM)
ClCl(IBM)
removeSymbols('IBM')
}
}
\keyword{ utilities }
