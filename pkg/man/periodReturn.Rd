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
Given a set of prices, return a \code{quantmod.return} object
of returns calculated during specified period. 
}
\usage{
periodReturn(x, period='monthly', subset=NULL, type='arithmetic',
             ...)

dailyReturn(x, subset=NULL, type='arithmetic', ...)
weeklyReturn(x, subset=NULL, type='arithmetic', ...)
monthlyReturn(x, subset=NULL, type='arithmetic', ...)
quarterlyReturn(x, subset=NULL, type='arithmetic', ...)
annualReturn(x, subset=NULL, type='arithmetic', ...)
yearlyReturn(x, subset=NULL, type='arithmetic', ...)

allReturns(x, subset=NULL, type='arithmetic')
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
\references{ Bodie,Kane...}
\author{ Jeffrey A. Ryan }
\note{
The final row of returned object will have the current period return to last date, 
i.e. this week/month/quarter/year return to date.
}
\seealso{\code{\link{getSymbols}} \code{\link[pkg:xts]{to.period}} }
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
