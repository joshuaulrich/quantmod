\name{getSymbols.FXMacroData}
\alias{getSymbols.FXMacroData}
\title{Download Macroeconomic Data from FXMacroData}
\description{
Download macroeconomic announcement time series from FXMacroData into an
\code{xts} object.
}
\usage{
getSymbols.FXMacroData(Symbols, env, return.class = "xts",
  currency = "USD", value.field = "val",
  api.key = Sys.getenv("FXMACRODATA_API_KEY"), limit = 1000, ...)
}
\arguments{
  \item{Symbols}{character vector of FXMacroData series. Use
  \code{"USD.INFLATION"}, \code{"USD.UNEMPLOYMENT"}, or pass an indicator such
  as \code{"inflation"} with \code{currency = "USD"}.}
  \item{env}{where to create objects when \code{auto.assign = TRUE}.}
  \item{return.class}{class of returned object, passed to quantmod conversion.}
  \item{currency}{default currency used when a symbol omits the currency
  prefix.}
  \item{value.field}{response field to use as the numeric observation, usually
  \code{"val"} or \code{"val_mom"}.}
  \item{api.key}{optional FXMacroData API key. Defaults to the
  \code{FXMACRODATA_API_KEY} environment variable.}
  \item{limit}{maximum number of observations to download. Requests are paged
  100 rows at a time.}
  \item{\dots}{additional arguments, including \code{from}, \code{to},
  \code{auto.assign}, \code{verbose}, and \code{warnings}.}
}
\details{
The source calls
\url{https://api.fxmacrodata.com/v1/announcements/\{currency\}/\{indicator\}}
and converts the \code{data} array to a dated \code{xts} series. Authentication
is optional for public USD macro series; when provided, the API key is sent in
the \code{X-API-Key} request header.
}
\value{
If \code{auto.assign = TRUE}, the loaded symbol names are returned invisibly and
objects are created in \code{env}. Otherwise, an \code{xts} object is returned.
}
\examples{
\dontrun{
getSymbols("USD.INFLATION", src = "FXMacroData")
getSymbols("unemployment", src = "FXMacroData", currency = "USD",
           auto.assign = FALSE)
}
}
\seealso{
\code{\link{getSymbols}}, \code{\link{setSymbolLookup}},
\code{\link{getSymbols.FRED}}
}
