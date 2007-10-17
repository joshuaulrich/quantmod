\name{chartSeries}
\alias{chartSeries}
\alias{barChart}
\alias{candleChart}
\alias{matchChart}
\alias{lineChart}
\title{ Create Financial Charts }
\description{
Charting tool to create standard financial charts
given a time series like object. Serves as the base function for
future technical analysis additions. Possible chart
styles include candles, matches (1 pixel candles), bars,
and lines. Chart may have white or black background.
}
\usage{
chartSeries(x,
           type = c("auto", "candlesticks", "matchsticks", "bars","line"), 
           show.vol = TRUE, 
           show.grid = TRUE, 
           name = deparse(substitute(x)),
           time.scale = NULL,
           technicals = NULL,
           line.type = "l",
           bar.type = "ohlc",
           xlab = "time", ylab = "price", theme = "black",
           up.col,dn.col,color.vol = TRUE, col.candles = FALSE,
           ...)

barChart(x,name=deparse(substitute(x)),type='bars',...)
candleChart(x,name=deparse(substitute(x)),type='candlesticks',...)
lineChart(x,name=deparse(substitute(x)),type='line',
          color.vol=FALSE,...)
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
  \item{line.type}{ type of line in line chart }
  \item{bar.type}{ type of barchart - ohlc or hlc }
  \item{xlab}{ derived time scale }
  \item{ylab}{ y axis label }
  \item{theme}{ 'white' or 'black' background }
  \item{up.col}{ up bar/candle color }
  \item{dn.col}{ down bar/candle color }
  \item{color.vol}{ color code volume? }
  \item{col.candles}{ 4 color candle pattern }
  \item{\dots}{ additional parameters }
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

line.type and bar.type allow further fine tuning of
chart styles to user tastes, as do the addition of
up.col and dn.col arguments.

col.candles implements a color coding scheme used in some
charting applications, and follows the following rules:

grey  => Open[t] < Close[t] and Open[t] < Close[t-1]
white => Open[t] < Close[t] and Open[t] > Close[t-1]
red   => Open[t] > Close[t] and Open[t] < Close[t-1]
black => Open[t] > Close[t] and Open[t] > Close[t-1]

}
\value{
Returns a standard chart plus volume, if available, suitably scaled.
}
\author{ Jeffrey A. Ryan }
\references{ Josh Ulrich - TTR package and col.candles coding }
\note{
Most details can be fine-tuned within the function, though the code
does a reasonable job of scaling and labelling axes for the user.

This is mostly a proof-of-concept function. It is currently being
rewritten with S4 style constructs to better facilitate the
addition of technical analysis indicators, as well as more
dynamic updating for adding additional series, and rescaling.

Additional motivation to add outlined candles to allow
for scaling and advanced color coding is owed to Josh Ulrich, as
are the base functions for the yet to be released technical analysis
routines.
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
