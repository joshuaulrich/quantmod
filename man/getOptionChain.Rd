\name{getOptionChain}
\alias{getOptionChain}
\title{ Download Option Chains }
\description{
Function to download option chain data from
data providers.
}
\usage{
getOptionChain(Symbols, Exp = NULL, src="yahoo", ...)
}
\arguments{
  \item{Symbols}{ The name of the underlying symbol }
  \item{Exp}{ One or more expiration dates, or NULL.
              If Exp is missing, the default is to
              only return the front month contract.
 }
  \item{src}{ Source of data.  Currently only \sQuote{yahoo} is provided }
  \item{\dots}{ additional parameters }
}
\details{
This function is a wrapper to data-provider specific
APIs.  By default the data is sourced from yahoo.
}
\value{
A named list containing two data.frames, one
for calls and one for puts.  If more than one
expiration was requested, this two-element list
will be contained within list of length \code{length(Exp)}.
Each element of this list will be named with the expiration
month and year (for Yahoo sourced data).

If \code{Exp} is set to \code{NULL}, all expirations
will be returned. Not explicitly setting will only
return the front month.
}
\references{ \url{http://finance.yahoo.com} }
\author{ Jeffrey A. Ryan }
\examples{
\dontrun{
AAPL.OPT <- getOptionChain("AAPL")
AAPL.OPTS <- getOptionChain("AAPL", NULL)
}
}
\keyword{ utilities }% __ONLY ONE__ keyword per line
