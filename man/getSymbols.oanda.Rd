\name{getSymbols.oanda}
\alias{getSymbols.oanda}
\alias{oanda.currencies}
\title{ Download Currency and Metals Data from Oanda.com }
\description{
Access to 191 currency and metal prices, downloadable
as more that 36000 currency pairs from Oanda.com.

Downloads \code{Symbols} to specified \code{env} from
\url{www.oanda.com} historical currency database.
This method is not meant to be called directly, instead
a call to \code{getSymbols(\dQuote{x},src=\dQuote{oanda})} will
in turn call this method. It is documented for the
sole purpose of highlighting the arguments accepted,
and to serve as a guide to creating additional
getSymbols 'methods'.
}
\usage{
getSymbols.oanda(Symbols,
                 env,
                 return.class = "xts",
                 from = Sys.Date() - 500,
                 to = Sys.Date(),
                 ...)
}
\arguments{
  \item{Symbols}{ a character vector specifying the names
of each symbol to be loaded - expressed as a currency pair.
(e.g. U.S. Dollar to Euro rate would be expressed as
a string \dQuote{USD/EUR}. The naming convention follows from
Oanda.com, and a table of possible values is
available by calling \code{oanda.currencies} }
  \item{env}{ where to create objects. }
  \item{return.class}{ class of returned object }
  \item{from}{ Start of series expressed as "CCYY-MM-DD" }
  \item{to}{ Start of series expressed as "CCYY-MM-DD" }
  \item{\dots}{ additional parameters }
}
\details{
 Meant to be called internally by getSymbols only.

 Oanda data is 7 day daily average price data, that is Monday through Sunday.
There is a limit of 2000 days per request, and getSymbols will
fail with a warning that the limit has been exceeded.
}
\value{
 A call to getSymbols(Symbols,src="oanda") will load into the specified
environment one object for each 'Symbol' specified, with class
defined  by 'return.class'. Presently this may be 'ts', 'its',
'zoo', 'xts', or 'timeSeries'.
}
\note{
Oanda rates are quoted as one unit of base currency to the
equivelant amount of foreign
currency.
}
\references{ Oanda.com \url{http://www.oanda.com} }
\author{ Jeffrey A. Ryan }
\seealso{ Currencies: \code{\link{getSymbols.FRED}},
          \code{\link{getSymbols}} }
\examples{
\dontrun{
getSymbols("USD/EUR",src="oanda")
getSymbols("USD/EUR",src="oanda",from="2005-01-01")
}
}
\keyword{ datasets }
