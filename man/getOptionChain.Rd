\name{getOptionChain}
\alias{getOptionChain}
\title{ Download Option Chains }
\description{
Function to download option chain data from
data providers.
}
\usage{
getOptionChain(Symbols, Exp = NULL, src="yahoo", \dots)
}
\arguments{
  \item{Symbols}{ The name of the underlying symbol. Source \sQuote{yahoo} only 
   allows for a single ticker while source \sQuote{orats} can return multiple tickers.}
  \item{Exp}{ One or more expiration dates, NULL, or an ISO-8601 style string.
    If \code{Exp} is missing, only the front month contract will be returned.
 }
  \item{src}{ Source of data.  One of \sQuote{yahoo} or \sQuote{orats} with
    a default of \sQuote{yahoo}.}
  \item{\dots}{ Additional parameters.}
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
month, day, and year (for Yahoo sourced data).

If \code{Exp} is set to \code{NULL}, all expirations
will be returned. Not explicitly setting will only
return the front month.
}
\author{ Jeffrey A. Ryan, Joshua M. Ulrich, Steve Bronder }
\references{
 \url{http://finance.yahoo.com},
 \url{https://docs.orats.io/datav2-api-guide/data.html#strikes}
}
\examples{
\dontrun{
# Only the front-month expiry
AAPL.OPT <- getOptionChain("AAPL")
# All expiries
AAPL.OPTS <- getOptionChain("AAPL", NULL)
# All 2015 and 2016 expiries
AAPL.2015 <- getOptionChain("AAPL", "2015/2016")
# Using orats backend
NFLX.AAPL.2021 <- getOptionChain(c("NFLX", "AAPL"), "2021", src = "orats", 
  api.key = Sys.getenv("ORATS_API_KEY"))
}
}
\seealso{\code{\link{getOptionChain.orats}}}
\keyword{ utilities }% __ONLY ONE__ keyword per line

