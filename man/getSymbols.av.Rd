\name{getSymbols.av}
\alias{getSymbols.av}
\alias{getSymbols.alphavantage}
\alias{getSymbols.Alphavantage}
\alias{getSymbols.alphVantage}
\alias{getSymbols.AlphVantage}
\title{ Download OHLC Data from Alpha Vantage }
\description{
Downloads historical or realtime equity price data
from \url{https://www.alphavantage.co/}.
Free registration is required.
}
\usage{
getSymbols.av(Symbols, env, api.key,
              return.class = "xts",
              periodicity = "daily",
              adjusted = FALSE,
              interval = "1min",
              output.size = "compact",
              data.type = "json",
              ...)
}
\arguments{
  \item{Symbols}{ a character vector specifying the names
    of the symbols to be loaded}
  \item{env}{ where to create objects (environment) }
  \item{api.key}{ the API key issued by Alpha Vantage when you registered (character)}
  \item{return.class}{ class of returned object, see Value (character) }
  \item{periodicity}{ one of \code{"daily"}, \code{"weekly"}, \code{"monthly"}, or \code{"intraday"} }
  \item{adjusted}{if TRUE, include a column of closing prices
    adjusted for dividends and splits}
  \item{interval}{one of \code{"1min"}, \code{"5min"}, \code{"15min"}, \code{"30min"}, or \code{"60min"}
    (intraday data only)}
  \item{output.size}{ either \code{"compact"} or \code{"full"} }
  \item{data.type}{ either \code{"json"} or \code{"csv"} }
  \item{\dots}{ additional parameters as per \code{\link{getSymbols}} }
}
\details{
Meant to be called internally by getSymbols only.
This method is not meant to be called directly, instead
a call to \code{getSymbols("x", src="av")} will
in turn call this method. It is documented for the
sole purpose of highlighting the arguments accepted.

You must register with Alpha Vantage in order to download their data,
but the one-time registration is fast and free.
Register at their web site, \url{https://www.alphavantage.co/},
and you will receive an \emph{API key}:
a short string of alphanumeric characters (e.g., "FU4U").
Provide the API key everytime you call \code{getSymbols};
or set it globally using \code{setDefaults(getSymbols.av, api.key="yourKey")}.

The Alpha Vantage site provides daily, weekly, monthly, and intraday data.
Use \code{periodicity} to select one.
Note that intraday data will includes today's data (delayed) if downloaded
while the market is open, which is pretty cool.

Set \code{adjusted=TRUE} to include a column of closing prices adjusted for
dividends and stock splits (available only for daily, weekly, and monthly data).

The intraday data is provided as a sequence of OHLC bars.
Use the \code{interval} argument to determine the "width" of the bars:
1 minute bars, 5 minutes bars, 15 minutes bars, etc.

By default Alpha Vantage returns the 100 most-recent data points (\code{output.size="compact"}).
Set \code{output.size="full"} to obtain the entire available history.
For daily, weekly, and monthly data, Alpha Vantage says the available data is up to 20 years;
for intraday data, the available history is the most recent 10 or 15 days.
Be forewarned that downloading \code{full} data requires more time than \code{compact} data, of course.

Alpha Vantage provides access to data via two APIs. You can choose the API via
the \code{data.type} argument. \code{data.type="json"}, the default, will
import data using the JSON API. This API includes additional metadata (e.g.
last updated time, timezone, etc) that is not provided via the CSV API.
}
\value{
A call to \code{getSymbols(Symbols, src="av")} will create objects
in the specified environment,
one object for each \code{Symbol} specified.
The object class of the object(s) is determined by \code{return.class}.
Presently this may be \code{"ts"}, \code{"zoo"}, \code{"xts"}, or \code{"timeSeries"}.
}
% \note{
% [TBD]
% }
\references{ Alpha Vantage documentation available at \url{https://www.alphavantage.co/} }
\author{ Paul Teetor }
\seealso{
          \code{\link{getSymbols}},
          \code{\link{getSymbols.yahoo}},
          \code{\link{getSymbols.google}}
}
\examples{
\dontrun{
# You'll need the API key given when you registered
getSymbols("IBM", src="av", api.key="yourKey")

# The default output.size="compact" returns only the most recent 100 rows.
# Set output.size="full" for all available data.
getSymbols("IBM", src="av", api.key="yourKey", output.size="full")

# Intraday data is available for the most recent 10 or 15 days
# and includes quasi-realtime data (i.e., 20-minute delayed)
getSymbols("IBM", src="av", api.key="yourKey", output.size="full",
  periodicity="intraday")

# Repeating your API key every time is tedious.
# Fortunately, you can set a global default.
setDefaults(getSymbols.av, api.key="yourKey")
getSymbols("IBM", src="av")
}
}
