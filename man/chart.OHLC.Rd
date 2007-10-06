\name{chart.OHLC}
\alias{chart.OHLC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Create OHLC Financial Chart }
\description{
Charting tool to create standard financial charts
given OHLC data. Serves as the base function for
future technical analysis additions.
}
\usage{
chart.OHLC(x,
           chart = c("auto", "candlesticks", "matchsticks", "bar","line"), 
           plot.volume = TRUE, 
           grid.lines = TRUE, 
           name = deparse(substitute(x)),
           xlab = "time", ylab = "price", bg.col = "black")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ an OHLC object - see details }
  \item{chart}{ style of chart to draw }
  \item{plot.volume}{ display volume on chart? }
  \item{grid.lines}{ display price grid lines? }
  \item{name}{ name of chart }
  \item{xlab}{ NA }
  \item{ylab}{ y axis label }
  \item{bg.col}{ 'white' or 'black' background }
}
\details{
Currently displays a standard style OHLC chart familiar
in financial applications. Works with objects having explicit
time-series properties, at present limited to \code{quantmod.OHLC}
data as returned by \code{getSymbols.yahoo}. In theory any zoo
object conforming to OHLC conventions can be coerced to a
\code{quantmod.OHLC} object, and thus charted.

Future versions will allow for additional charting functions
to be called subsequently which will allow for
additional information to be display on the chart, such as
technical indicators/studies, as well as entry and exit points
given a particular trade strategy.
}
\value{
Returns a standard OHLC chart plus volume
}
\references{ ~put references to the literature/web site here ~ }
\author{ Jeffrey A. Ryan }
\note{
Most details can be fine-tuned within the function, though the code
does a good job of scaling and labelling axes for the user.
}
\seealso{ \code{\link{getSymbols}} }
\examples{
\dontrun{
getSymbols("YHOO")
chart.OHLC(YHOO)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }
