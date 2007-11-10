\name{getFX}
\alias{getFX}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Download Exchange Rates }
\description{
Download exchange rates or metals prices from oanda.
}
\usage{
getFX(Currencies,
      from = "2007-01-01", to = Sys.Date(),
      env = .GlobalEnv,
      verbose = FALSE,
      warning = TRUE,
      auto.assign = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Currencies}{ Currency pairs expressed as \sQuote{CUR/CUR} }
  \item{from}{ start date expressed in ISO CCYY-MM-DD format }
  \item{to}{ end date expressed in ISO CCYY-MM-DD format }
  \item{env}{ which environment should they be loaded into }
  \item{verbose}{ be verbose }
  \item{warning}{ show warnings }
  \item{auto.assign}{ use auto.assign }
  \item{\dots}{ additional parameters to be passed to getSymbols.oanda method }
}
\details{
A convenience wrapper to \code{getSymbols(x,src='oanda')}. See
\code{getSymbols} and \code{getSymbls.oanda} for more detail.
}
\value{
The results of the call will be the data will be assigned
automatically to the environment specified (global by default). Additionally
a vector of downloaded symbol names will be returned.

See \code{getSymbols} and \code{getSymbols.oanda} for more detail.
}
\references{ Oanda.com \url{http://www.oanda.com} }
\author{ Jeffrey A. Ryan }
\seealso{ \code{\link{getSymbols}}, \code{\link{getSymbols.oanda}} }
\examples{
\dontrun{

getFX("USD/JPY")

getFX("EUR/USD",from="2005-01-01")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities}
