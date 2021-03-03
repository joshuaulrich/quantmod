\name{getOptionChain.orats}
\alias{getOptionChain.orats}
\title{ Download Option Chain Data from orats }
\description{
Function to download option chain data from orats.
}
\usage{
getOptionChain.orats(Symbols, Exp, api.key, dte, delta)
}
\arguments{
  \item{Symbols}{ The name of the underlying symbol. Source \sQuote{yahoo} only 
   allows for a single ticker while source \sQuote{orats} can return multiple tickers.}
  \item{Exp}{ One or more expiration dates, NULL, or an ISO-8601 style string.
    If \code{Exp} is missing, only the front month contract will be returned.
 }
  \item{api.key}{ A character vector for the key given with an account for 
   accessing the orats API. If missing, the function will look for an
   environment variable \code{ORATS_API_KEY} containing the API key.}
  \item{dte}{ A vector of two integers giving
   a range of expiry dates to subset the results by.}
  \item{delta}{A vector of two integers giving
   a range deltas to subset the results by.}
}
\details{
This function is a wrapper to data-provider specific
APIs.  By default the data is sourced from yahoo.
}
\value{

For orats, returns A named list containing five data.frames, one
each for calls and puts that follows a similar form to the return 
from yahoo, but does not have a Last price and instead has a
Ticker column for multiple ticker requests. The *_extra data.frames
contain additional information from the \sQuote{orats} API end point
whose definitions are available at the URL in the references.
If more than one expiration was requested, The results will be returned as
a list within list of length \code{length(Exp)}.
Each element of this list will be named with the expiration
month, day, and year (\%b.\%d.\%Y).

If \code{Exp} is set to \code{NULL}, all expirations
will be returned. Not explicitly setting will only
return the front month.
}
\references{
 \url{https://docs.orats.io/datav2-api-guide/data.html#strikes}
}
\author{Steve Bronder }
\examples{
\dontrun{
# Only the front-month expiry
AAPL.OPT <- getOptionChain("AAPL", 
  api.key = Sys.getenv("ORATS_API_KEY"))
# All expiries
AAPL.OPTS <- getOptionChain("AAPL", NULL,
  api.key = Sys.getenv("ORATS_API_KEY"))
# All 2015 and 2016 expiries
AAPL.2015 <- getOptionChain("AAPL", "2015/2016", 
  api.key = Sys.getenv("ORATS_API_KEY"))
}
}
\keyword{ utilities }% __ONLY ONE__ keyword per line
