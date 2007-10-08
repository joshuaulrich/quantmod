\name{chartSeries}
\alias{chartSeries}
\alias{barChart}
\alias{candleChart}
\alias{matchChart}
\alias{lineChart}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Create Financial Charts }
\description{
Charting tool to create standard financial charts
given a time series like object. Serves as the base function for
future technical analysis additions. Possible chart
styles include candles, matches (1 pixel candles), bars,
and lines. Chart may have white or black background.
}
\usage{
chart.OHLC(x,
           type = c("auto", "candlesticks", "matchsticks", "bars","line"), 
           show.vol = TRUE, 
           show.grid = TRUE, 
           name = deparse(substitute(x)),
           time.scale = NULL,
           technicals = NULL,
           line.type = "l",
           xlab = "time", ylab = "price", bg.col = "black")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ an OHLC object - see details }
  \item{type}{ style of chart to draw }
  \item{show.vol}{ display volume on chart? }
  \item{show.grid}{ display price grid lines? }
  \item{name}{ name of chart }
  \item{time.scale}{ what is the timescale? automatically deduced }
  \item{technicals}{ a list of technical indicators and params (yet to be added) }
  \item{xlab}{ derived time scale }
  \item{ylab}{ y axis label }
  \item{theme}{ 'white' or 'black' background }
}
\details{
Currently displays standard style OHLC charts familiar
in financial applications, or line charts when
not passes OHLC data. Works with objects having explicit
time-series properties, at present limited to \code{quantmod.OHLC}
data as returned by \code{getSymbols.yahoo}. In theory any zoo
object conforming to OHLC conventions can be coerced to a
\code{quantmod.OHLC} object, and thus charted.

Line charts are created with close data, or from single
column time series.

Future versions will allow for additional charting functions
to be called subsequently which will allow for
additional information to be display on the chart, such as
technical indicators/studies, as well as entry and exit points
given a particular trade strategy.
}
\value{
Returns a standard chart plus volume, if available, suitably scaled.
}
\author{ Jeffrey A. Ryan }
\note{
Most details can be fine-tuned within the function, though the code
does a good job of scaling and labelling axes for the user.
}
\seealso{ \code{\link{getSymbols}} }
\examples{
\dontrun{
getSymbols("YHOO")
chartSeries(YHOO)

barChart(YHOO)
candleChart(YHOO)
matchChart(YHOO,theme="white")
lineChart(YHOO)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }
