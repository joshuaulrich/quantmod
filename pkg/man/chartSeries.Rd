\name{chartSeries}
\alias{chartSeries}
\alias{barChart}
\alias{candleChart}
\alias{matchChart}
\alias{lineChart}
\alias{cS2}  %remove before final push
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
           subset = NULL,
           show.grid = TRUE, 
           name = NULL,
           time.scale = NULL,
           TA = c(addVo()),
           line.type = "l",
           bar.type = "ohlc",
           xlab = "time", ylab = "price", theme = chartTheme("black"),
           major.ticks='auto', minor.ticks=TRUE,
           up.col,dn.col,color.vol = TRUE, multi.col = FALSE,
           ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ an OHLC object - see details }
  \item{type}{ style of chart to draw }
  \item{subset}{ currently unused }
  \item{show.grid}{ display price grid lines? }
  \item{name}{ name of chart }
  \item{time.scale}{ what is the timescale? automatically deduced }
  \item{TA}{ a vector of technical indicators and params }
  \item{line.type}{ type of line in line chart }
  \item{bar.type}{ type of barchart - ohlc or hlc }
  \item{xlab}{ derived time scale }
  \item{ylab}{ y axis label }
  \item{theme}{ a chart.theme object }
  \item{major.ticks}{ where should major ticks be drawn}
  \item{minor.ticks}{ should minor ticks be draw? }
  \item{up.col}{ up bar/candle color }
  \item{dn.col}{ down bar/candle color }
  \item{color.vol}{ color code volume? }
  \item{multi.col}{ 4 color candle pattern }
  \item{\dots}{ additional parameters }
}
\details{
Currently displays standard style OHLC charts familiar
in financial applications, or line charts when
not passes OHLC data. Works with objects having explicit
time-series properties.

Line charts are created with close data, or from single
column time series.

\code{TA} allows for the inclusion of a variety of
chart overlays and tecnical indicators.  A full list is
available from \code{addTA}. The default TA argument is
\code{addVo()} - which adds volume, if available, to the
chart being drawn.

\code{theme} requires an object of class \code{chart.theme}, created
by a call to \code{chartTheme}.  This function can be used to modify the
look of the resulting chart.  See \code{chart.theme} for details.

\code{line.type} and \code{bar.type} allow further fine tuning of
chart styles to user tastes.

\code{multi.col} implements a color coding scheme used in some
charting applications, and follows the following rules:

\itemize{
  \item{grey}{ => Op[t] < Cl[t] and Op[t] < Cl[t-1]}
  \item{white}{ => Op[t] < Cl[t] and Op[t] > Cl[t-1]}
  \item{red}{   => Op[t] > Cl[t] and Op[t] < Cl[t-1]}
  \item{black}{ => Op[t] > Cl[t] and Op[t] > Cl[t-1]}
}
}
\value{
Returns a standard chart plus volume, if available, suitably scaled.
}
\author{ Jeffrey A. Ryan }
\references{ Josh Ulrich - \pkg{TTR} package and multi.col coding }
\note{
Most details can be fine-tuned within the function, though the code
does a reasonable job of scaling and labelling axes for the user.

The current implementation maintains a record of actions carried
out for any particular chart.  This is used to recreate the original when
adding new indicator.  A list of applied TA actions is available with a call
to \code{listTA}. This list can be assigned to a variable and used in new
chart calls to recreate a set of technical indicators.  It is also possible
to force all future charts to use the same indicators by calling \code{setTA}.

Additional motivation to add outlined candles to allow
for scaling and advanced color coding is owed to Josh Ulrich, as
are the base functions (from \pkg{TTR}) for the yet to be
released technical analysis charting code
}
\seealso{ \code{\link{getSymbols}}, \code{\link{addTA}}, \code{\link{setTA}},
          \code{\link{chartTheme}} }
\examples{
\dontrun{
getSymbols("YHOO")
chartSeries(YHOO)
chartSeries(YHOO,theme=chartTheme('white'))
chartSeries(YHOO,TA=NULL)   #no volume
chartSeries(YHOO,TA=c(addVo(),addBBands()))  #add volume and Bollinger Bands from TTR

addMACD()   #  add MACD indicator to current chart

setTA()
chartSeries(YHOO)   #draws chart again, this time will all indicators present
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }
