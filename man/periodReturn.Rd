\name{periodReturn}
\alias{periodReturn}
\alias{allReturns}
\alias{dailyReturn}
\alias{weeklyReturn}
\alias{monthlyReturn}
\alias{quarterlyReturn}
\alias{annualReturn}
\title{ Calculate Periodic Returns }
\description{
Given a set of prices, return a \code{quantmod.return} object
of returns calculated during specified period. 
}
\usage{
periodReturn(x, period='monthly', subset=NULL, type='arithmetic',
             ...)

% allReturns(x, from = NULL, to = NULL)
dailyReturn(x, subset=NULL, type='arithmetic', ...)
weeklyReturn(x, subset=NULL, type='arithmetic', ...)
monthlyReturn(x, subset=NULL, type='arithmetic', ...)
quarterlyReturn(x, subset=NULL, type='arithmetic', ...)
annualReturn(x, subset=NULL, type='arithmetic', ...)
yearlyReturn(x, subset=NULL, type='arithmetic', ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ zoo object of state prices}
  \item{period}{ character string indicating time period.  Valid entries are
             daily, weekly, monthly, quarterly, yearly.  All 
             are accessible from wrapper functions described below.
             Defaults to monthly returns (same as monthlyReturn)}
  \item{subset}{ an xts style subset string }
  \item{type}{ type of returns: arithmetic (discrete) or log (continuous) }
}
\details{
\code{periodReturn} is the underlying function for wrappers:
\itemize{
%  \item \code{allReturns: }      calculate all available return periods
  \item \code{dailyReturn: }     calculate daily returns
  \item \code{weeklyReturn: }    calculate weekly returns
  \item \code{monthlyReturn: }   calculate monthly returns
  \item \code{quarterlyReturn: } calculate quarterly returns
  \item \code{annualReturn: }    calculate annual returns
}
}
\value{
Returns object of the class that was originally passed in
}
\references{ Bodie,Kane...}
\author{ Jeffrey A. Ryan }
\note{
The final row of returned object will have the current period return to last date, 
i.e. this week/month/quarter/year return to date.

By default, if \code{from} and/or \code{to} is missing, they will
be set to the dataset beginning and end, respectively. One or both may be set, and 
will subset the data, inclusively, after all calculations have been made.  The values must 
be character strings representing dates in ISO 8601 format. (e.g. 1992-01-01).

Additional methods to handle \code{ts} and \code{data.frame} objects will enable automatic
conversion to the suitable zoo object as returned by a call to \code{getSymbols}
}
\seealso{\code{\link{getSymbols}} }
\examples{
\dontrun{
getSymbols('QQQQ',src='yahoo')
allReturns(QQQQ)  # returns all periods

periodReturn(QQQQ,by=years,from='2003-01-01')  # returns years 2003 to present
periodReturn(QQQQ,by=years,from='2003-01-01',to='2004-01-01')  # returns year 2003

rm(QQQQ)
}
}
\keyword{ utilities }% at least one, from doc/KEYWORDS
