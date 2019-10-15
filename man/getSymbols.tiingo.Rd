\name{getSymbols.tiingo}
\alias{getSymbols.tiingo}
\title{ Download OHLC Data from Tiingo }
\description{
  Downloads historical or realtime equity price data
  from \url{https://api.tiingo.com/}.
  Registration is required.
}
\usage{
  getSymbols.tiingo(Symbols, env, api.key,
                    return.class="xts",
                    periodicity="daily",
                    adjust=FALSE,
                    from='2007-01-01',
                    to=Sys.Date(),
                    data.type="json",
                    ...)
}
\arguments{
  \item{Symbols}{ a character vector specifying the names
    of the symbols to be loaded}
  \item{env}{ where to create objects (environment) }
  \item{api.key}{ the API key issued by Tiingo when you registered (character)}
  \item{return.class}{ class of returned object, see Value (character) }
  \item{periodicity}{ one of \code{"daily"}, \code{"weekly"}, \code{"monthly"}, or \code{"Annually"} }
  \item{adjust}{ adjust for dividends and splits? (FALSE) }
  \item{from}{ Retrieve data no earlier than this date. (2007-01-01)}
  \item{to}{ Retrieve data through this date (Sys.Date())}
  \item{data.type}{ either \code{"json"} or \code{"csv"} }
  \item{\dots}{ additional parameters as per \code{\link{getSymbols}} }
}
\details{
  Meant to be called internally by \code{getSymbols} only.
  This method is not meant to be called directly, instead
  a call to \code{getSymbols("x", src="tiingo")} will
  in turn call this method. It is documented for the
  sole purpose of highlighting the arguments accepted.
  
  You must register with Tiingo in order to download their data.
  Register at their web site, \url{https://api.tiingo.com},
  and you will receive an \emph{API key}:
    a short string of alphanumeric characters (e.g., "FU4U").
  Provide the API key every time you call \code{getSymbols};
  or set it globally using \code{setDefaults(getSymbols.tiingo, api.key="yourKey")}.
  
  Tiingo provides daily, weekly, monthly, and annual data.
  Use \code{periodicity} to select one.
  This API accessor will return adjusted or unadjusted OHLC as well as split and dividend information.

  For daily, weekly, and monthly data, Tiingo says the available data is up to 30 years;

  Tiingo provides access to data via two APIs. You can choose the API via
  the \code{data.type} argument. \code{data.type="json"}, the default, will
  import data using the JSON API. This API includes additional metadata (e.g.
  last updated time, timezone, etc) that is not provided via the CSV API.
}
\value{
A call to \code{getSymbols(Symbols, src="tiingo")} will create objects
in the specified environment,
one object for each \code{Symbol} specified.
The object class of the object(s) is determined by \code{return.class}.
Presently this may be \code{"ts"}, \code{"zoo"}, \code{"xts"}, or \code{"timeSeries"}.
}
% \note{
% [TBD]
% }
\references{ Tiingo documentation available at \url{https://www.tiingo.com} }
\author{ Steve Bronder }
\seealso{
\code{\link{getSymbols}},
\code{\link{getSymbols.yahoo}},
\code{\link{getSymbols.av}}
}
\examples{
\dontrun{
# You'll need the API key given when you registered
getSymbols("IBM", src="tiingo", api.key="yourKey")

# Repeating your API key every time is tedious.
# Fortunately, you can set a global default.
setDefaults(getSymbols.tiingo, api.key="yourKey")
getSymbols("IBM", src="tiingo")
}
}
