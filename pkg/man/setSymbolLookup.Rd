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

saveSymbolLookup(file,dir="")
loadSymbolLookup(file,dir="")
}
\arguments{
  \item{\dots}{ name=value pairs for symbol defaults }
  \item{Symbols}{ name of symbol(s) }
  \item{confirm}{ warn before deleting lookup table }
  \item{file}{ filename }
  \item{dir}{ directory of filename }
}
\details{
Use of these functions allows the user to specify
a set of default parameters for each \code{Symbol} to be
loaded.

Different sources (e.g. yahoo, MySQL, csv),
can be specified for each Symbol of
interest. The sources must be \emph{valid}
\code{getSymbols} methods - see \code{getSymbols}
for details on which methods are available, as well as
how to add additional methods.

The argument list to \code{setSymbolLookup} is
simply the unquoted name of the \code{Symbol} matched
to the desired default source, or list of Symbol
specific parameters.

For example, to signify that the stock data for
Sun Microsystems (JAVA) should be downloaded from
Yahoo! Finance, one would call
\code{setSymbolLookup(JAVA='yahoo')}
or \code{setSymbolLookup(JAVA=list(src='yahoo'))}

It is also possible to specify additional,
possibly source specific, lookup details on
a per symbol basis.  These include an alternate
naming convention (useful for sites like Yahoo! where
certain non-traded symbols are prepended with a caret,
or more correctly a curcumflex accent. In that case one would
specify \code{setSymbolLookup(DJI=list(name="^DJI",src="yahoo"))})
as well as passed parameters like \code{dbname} and
\code{password} for database sources. See the
specific getSymbols function related to the source
in question for more details of each implementation.

All changes are made to the current list, and will
persist \emph{only} until the end of the session. To \emph{always}
use the same defaults it is necessary to call
\code{setSymbolLookup} with the appropriate
parameters from a startup file (e.g.
.Rprofile) or to use \code{saveSymbolLookup} and
\code{loadSymbolLookup} to save and restore lookup
tables.

To unset a specific Symbol's defaults, simply assign
\code{NULL} to the Symbol.
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
\seealso{ \code{\link{getSymbols}}, \code{\link{options}},}
\examples{
setSymbolLookup(QQQQ='yahoo',DIA='MySQL')
getSymbolLookup('QQQQ')
getSymbolLookup(c('QQQQ','DIA'))

\dontrun{
## Will download QQQQ from yahoo
## and load DIA from MySQL
getSymbols(c('QQQQ','DIA'))
}

## Use something like this to always retrieve
## from the same source

.First <- function() {
  require(quantmod,quietly=TRUE)
  quantmod::setSymbolLookup(JAVA="MySQL")
}

## OR

saveSymbolLookup()
loadSymbolLookup()
}
\keyword{ utilities }
