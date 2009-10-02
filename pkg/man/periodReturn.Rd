\name{periodReturn}
\alias{periodReturn}
\alias{allReturns}
\alias{dailyReturn}
\alias{weeklyReturn}
\alias{monthlyReturn}
\alias{quarterlyReturn}
\alias{yearlyReturn}
\alias{annualReturn}
\title{ Calculate Periodic Returns }
\description{
Given a set of prices, return 
periodic returns. 
}
\usage{
periodReturn(x,
             period='monthly',
             subset=NULL,
             type='arithmetic',
             leading=TRUE,
             ...)

dailyReturn(x, subset=NULL, type='arithmetic',
           leading=TRUE, ...)
weeklyReturn(x, subset=NULL, type='arithmetic',
           leading=TRUE, ...)
monthlyReturn(x, subset=NULL, type='arithmetic',
           leading=TRUE, ...)
quarterlyReturn(x, subset=NULL, type='arithmetic',
           leading=TRUE, ...)
annualReturn(x, subset=NULL, type='arithmetic',
           leading=TRUE, ...)
yearlyReturn(x, subset=NULL, type='arithmetic',
           leading=TRUE, ...)
allReturns(x, subset=NULL, type='arithmetic',
           leading=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ object of state prices, or an OHLC type object }
  \item{period}{ character string indicating time period.  Valid entries are
             \sQuote{daily}, \sQuote{weekly}, 
             \sQuote{monthly}, \sQuote{quarterly}, \sQuote{yearly}.  All 
             are accessible from wrapper functions described below.
             Defaults to monthly returns (same as monthlyReturn)}
  \item{subset}{ an xts/ISO8601 style subset string }
  \item{type}{ type of returns: arithmetic (discrete) or log (continuous) }
  \item{leading}{ should incomplete leading period returns be returned }
  \item{\dots}{ passed along to to.period }
}
\details{
\code{periodReturn} is the underlying function for wrappers:
\itemize{
  \item \code{allReturns: }      calculate all available return periods
  \item \code{dailyReturn: }     calculate daily returns
  \item \code{weeklyReturn: }    calculate weekly returns
  \item \code{monthlyReturn: }   calculate monthly returns
  \item \code{quarterlyReturn: } calculate quarterly returns
  \item \code{annualReturn: }    calculate annual returns
}
}
\value{
Returns object of the class that was originally passed in,
with the possible exception of monthly and quarterly return
indicies being changed to class \code{yearmon} and \code{yearqtr}
where available.  This can be overridden with the \code{indexAt}
argument passed in the \ldots to the \code{to.period} function.

By default, if \code{subset} is NULL, the full dataset will
be used.
}
\note{
Attempts are made to re-convert the resultant series to its original
class, if supported by the xts package.  At present, objects inheriting
from the \sQuote{ts} class are returned as \code{xts} objects.  This
is to make the results more visually appealling and informative.  All
\code{xts} objects can be converted to class \code{ts} with
\code{as.ts} if that is desirable.

The first and final row of returned object will have the period return to last date, 
i.e. this week/month/quarter/year return to date even if the start/end is not the start/end
of the period. Leading period calculations can be suppressed by setting \code{leading=FALSE}.
}
\author{ Jeffrey A. Ryan }
\note{
}
\seealso{\code{\link{getSymbols}}}
\examples{
\dontrun{
getSymbols('QQQQ',src='yahoo')
allReturns(QQQQ)  # returns all periods

periodReturn(QQQQ,period='yearly',subset='2003::')  # returns years 2003 to present
periodReturn(QQQQ,period='yearly',subset='2003')  # returns year 2003

rm(QQQQ)
}
}
\keyword{ utilities }% at least one, from doc/KEYWORDS
