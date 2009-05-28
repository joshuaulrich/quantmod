\name{adjustOHLC}
\Rdversion{1.1}
\alias{adjustOHLC}
\title{
Adjust Open,High,Low,Close Prices For Splits and Dividends
}
\description{
Adjust all columns of an OHLC
object for
split and dividend.
}
\usage{
adjustOHLC(x,
           adjust = c("split","dividend"),
           use.Adjusted = FALSE,
           ratio = NULL)
}
\arguments{
  \item{x}{ An OHLC object }
  \item{adjust}{ adjust by split, dividend, or both (default) }
  \item{use.Adjusted}{ use the \sQuote{Adjusted} column in Yahoo! data to adjust }
  \item{ratio}{ ratio to adjust with, bypassing internal calculations }
}
\details{
This function calculates the adjusted
Open, High, Low, and Close prices according
to split and dividend information.

There are three methods available to
calculate the new OHLC object prices.

By default, getSplits and getDividends are
called to retrieve the respective information. These
may dispatch to custom methods following the \dQuote{.}
methodology used by quantmod dispatch.  See \code{getSymbols}
for information related to extending quantmod.
This information is passed to
\code{adjSplitDiv} from the \pkg{TTR} package, and
the resulting ratio calculations are used to adjust
to observed historical prices.
This is the most precise way to adjust a series.

The second method works only on standard
Yahoo! data containing an explicit
Adjusted column.

A final method allows for one to pass
a \code{ratio} into the function directly.

All methods proceed as follows:

New columns are derived by taking the 
ratio of adjusted value to original Close, and multiplying
by the difference of the respective column and the
original Close.  This is then added to the modified Close
column to arrive at the remaining \sQuote{adjusted}
Open, High, Low column values.

If no adjustment is needed, the function returns the
original data unaltered.
}
\value{
An object of the original class, with prices
adjusted for splits and dividends.
}
\references{
Yahoo Finance \url{http://finance.yahoo.com}
}
\author{
Jeffrey A. Ryan
}
\section{Warning }{
Using \code{use.Adjusted = TRUE} will be less precise
than the method that employs actual split
and dividend information. This is due to
loss of precision from Yahoo! using
Adjusted columns of only two decimal places.
The advantage is that this can be run offline,
and for short series or those with few adjustments
the loss of precision will be small.

The resulting precision loss will be from row
observation to row observation, as the calculation
will be exact for intraday values.
}
\seealso{
\code{\link{getSymbols.yahoo}}
\code{\link{getSplits}}
\code{\link{getDividends}}
\code{\link[TTR:TTR-package]{adjSplitDiv}}
}
\examples{
\dontrun{
getSymbols("AAPL", from="1990-01-01", src="yahoo")
head(AAPL)
head(AAPL.a <- adjustOHLC(AAPL))
head(AAPL.uA <- adjustOHLC(AAPL, use.Adjusted=TRUE))

# intrada adjustments are precise across all methods
# an example with Open to Close (OpCl)
head(cbind(OpCl(AAPL),OpCl(AAPL.a),OpCl(AAPL.uA)))

# Close to Close changes ma lose precision
head(cbind(ClCl(AAPL),ClCl(AAPL.a),ClCl(AAPL.uA)))
}
}
\keyword{ misc }
