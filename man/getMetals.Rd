\name{getMetals}
\alias{getMetals}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Download Daily Metals Prices }
\description{
Download daily metals prices from oanda.
}
\usage{
getMetals(Metals,
      from = Sys.Date() - 500,
      to = Sys.Date(),
      base.currency="USD",
      env = parent.frame(),
      verbose = FALSE,
      warning = TRUE,
      auto.assign = TRUE, ...)
}
\arguments{
  \item{Metals}{ metals expressed in common name or symbol form }
  \item{from}{ start date expressed in ISO CCYY-MM-DD format }
  \item{to}{ end date expressed in ISO CCYY-MM-DD format }
  \item{base.currency}{ which currency should the price be in }
  \item{env}{ which environment should they be loaded into }
  \item{verbose}{ be verbose }
  \item{warning}{ show warnings }
  \item{auto.assign}{ use auto.assign }
  \item{\dots}{ additional parameters to be passed to getSymbols.oanda method }
}
\details{
A convenience wrapper to \code{getSymbols(x,src='oanda')}.

The most useful aspect of getMetals is the ablity to specify the Metals
in terms of underlying 3 character symbol or by name (e.g. XAU (gold) , XAG (silver),
XPD (palladium), or XPT (platinum)).

There are unique aspects of any continuously traded commodity, and it
is recommended that the user visit \url{http://www.oanda.com} for
details on specific pricing issues.

See \code{getSymbols} and \code{getSymbls.oanda} for more detail.
}
\value{
Data will be assigned
automatically to the environment specified (parent by default).
If auto.assign is set to FALSE, the data from a single metal
request will simply be returned from the function call.

If auto.assign is used (the default)
a vector of downloaded symbol names will be returned.

See \code{getSymbols} and \code{getSymbols.oanda} for more detail.
}
\references{ Oanda.com \url{http://www.oanda.com} }
\author{ Jeffrey A. Ryan }
\seealso{ \code{\link{getSymbols}}, \code{\link{getSymbols.oanda}} }
\examples{
\dontrun{

getFX(c("gold","XPD"))

getFX("plat",from="2005-01-01")
}
}
\keyword{ utilities}
