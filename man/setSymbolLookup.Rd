\name{setSymbolLookup}
\alias{setSymbolLookup}
\alias{getSymbolLookup}
\alias{unsetSymbolLookup}
\alias{loadSymbolLookup}
\alias{saveSymbolLookup}
\title{ Manage Symbol Lookup Table }
\description{
Create and manage Symbol defaults lookup table within
\R session for use in \code{getSymbols} calls.
}
\usage{
setSymbolLookup(...)
getSymbolLookup(Symbols=NULL)
unsetSymbolLookup(Symbols,confirm=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{ name=value pairs for symbol defaults }
  \item{Symbols}{ name of symbol(s) }
}
\details{
Use of these functions allows the user to specify
a default scheme for each \code{Symbol} to be
loaded.

Different sources (e.g. yahoo, MySQL),
return classes (e.g. quantmod.OHLC,zoo, etc),
and more can be specified for each Symbol of
interest.

The argument list to \code{setSymbolLookup} is
simply the unquoted name of the \code{Symbol} matched
to the desired defaults.

For example, to signify that the stock data for
Sun Microsystems (SUNW) should be downloaded from
Yahoo! Finance, one would call
\code{setSymbolLookup(SUNW='yahoo')}

All changes are made to the current list, and will
persist until the end of the session. If it is
desirable to maintain the values, use \code{saveSymbolLookup}
and \code{loadSymbolLookup}.
}
\value{
Called for its side effects, the function changes the
\code{options} value for the specified Symbol
through a call to \code{options(getSymbols.sources=...)}
}
\author{ Jeffrey A. Ryan }
\note{ 
Changes are \emph{NOT} persistent across sessions,
as the table is stored in the session options by default.

This \emph{may} change to allow for an easier to
manage process, as for now it is designed to minimize
the clutter created during a typical session.
}
\seealso{ \code{\link{getSymbols}}, \code{\link{options}},
          \code{\link{getDefaults}} }
\examples{
setSymbolSource(QQQQ='yahoo',DIA='MySQL')
getSymbolSource('QQQQ')
getSymbolSource(c('QQQQ','DIA')

\dontrun{
## Will download QQQQ from yahoo
## and load DIA from MySQL
getSymbols(c('QQQQ','DIA'))

}
}
\keyword{ utilities }
