\name{addMA}
\alias{addMA}
\alias{addSMA}
\alias{add_SMA}
\alias{addEMA}
\alias{add_EMA}
\alias{addWMA}
\alias{add_WMA}
\alias{addDEMA}
\alias{add_DEMA}
\alias{addEVWMA}
\alias{add_EVWMA}
\alias{add_VMA}
\alias{addZLEMA}
\alias{add_VWAP}
\alias{add_GMMA}
\title{ Add Moving Average to Chart }
\description{
Add one or more moving averages to a chart.
}
\usage{
addSMA(n = 10, on = 1, with.col = Cl, overlay = TRUE, col = "brown")

addEMA(n = 10, wilder = FALSE, ratio=NULL, on = 1,
       with.col = Cl, overlay = TRUE, col = "blue")

addWMA(n = 10, wts=1:n, on = 1, with.col = Cl, overlay = TRUE, col = "green")

addDEMA(n = 10, on = 1, with.col = Cl, overlay = TRUE, col = "pink")

addEVWMA(n = 10, on = 1, with.col = Cl, overlay = TRUE, col = "yellow")

addZLEMA(n = 10, ratio=NULL, on = 1, with.col = Cl, overlay = TRUE, col = "red")
}
\arguments{
  \item{n}{ periods to average over }
  \item{wilder}{ logical; use wilder? }
  \item{wts}{ a vector of weights }
  \item{ratio}{ a smoothing/decay ratio }
  \item{on}{ apply to which figure (see below) }
  \item{with.col}{ using which column of data (see below) }
  \item{overlay}{ draw as overlay }
  \item{col}{ color of MA }
}
\details{
see the appropriate base MA functions in \pkg{TTR} for
more details and references.
}
\value{
A moving average indicator will be draw on the current
chart.  A chobTA object will be returned silently.
}
\references{ see MovingAverages in \pkg{TTR} written by Josh Ulrich }
\author{ Jeffrey A. Ryan }
\seealso{ \code{\link{addTA}}}
\examples{
\dontrun{
addSMA()
addEMA()
addWMA()
addDEMA()
addEVWMA()
addZLEMA()
}
}
\keyword{ utilities }
