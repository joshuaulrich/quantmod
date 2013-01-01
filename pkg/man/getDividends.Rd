\name{getDividends}
\alias{getDividends}
\title{ Load Financial Dividend Data }
\description{
Download, or download and append stock dividend data
from Yahoo! Finance.
}
\usage{
getDividends(Symbol, 
             from = "1970-01-01",
             to = Sys.Date(), 
             env = parent.frame(), 
             src = "yahoo", 
             auto.assign = FALSE, 
             auto.update = FALSE, 
             verbose = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Symbol}{ The Yahoo! stock symbol }
  \item{from}{ date from in CCYY-MM-DD format }
  \item{to}{ date to in CCYY-MM-DD format }
  \item{env}{ where to create object }
  \item{src}{ data source (only yahoo is valid at present)}
  \item{auto.assign}{ should results be loaded to env}
  \item{auto.update}{ automatically add dividend to data object }
  \item{verbose}{ display status of retrieval }
  \item{\dots}{ currently unused }
}
\details{
Eventually destined to be a wrapper function along the lines
of \code{getSymbols} to different sources - this currently
only support Yahoo data.
}
\value{
If auto.assign is TRUE, the symbol will be written
to the environment specified in \code{env} with a
.div appended to the name.

If auto.update is TRUE and the object is of class
\code{xts}, the dividends will be included as an
attribute of the original object and be reassigned
to the environment specified by \code{env}.

All other cases will return the dividend data
as an \code{xts} object.
}
\references{ Yahoo! Finance: \url{http://finance.yahoo.com}}
\author{ Jeffrey A. Ryan }
\note{ 
This function is very preliminary - and will most likely
change significantly in the future.
}
\seealso{ \code{\link{getSymbols}} }
\examples{
\dontrun{
getSymbols("MSFT")
getDividends("MSFT")

getDividends(MSFT)
}
}
\keyword{ utilities }
