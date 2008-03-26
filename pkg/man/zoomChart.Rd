\name{zoomChart}
\alias{zoom}
\alias{zoomChart}
\title{ Change Zoom Level Of Current Chart }
\description{
Using \pkg{xts} style date subsetting, zoom into
or out of the current chart.
}
\usage{
zoom(n=1, eps=2)
zoomChart(subset = NULL)
}

\arguments{
  \item{n}{ the number of interactive view changes per call }
  \item{eps}{ the distance between clicks to be considered a valid subset request }
  \item{subset}{ a valid subset string }
}
\details{
These function allow for viewing of specific
areas of a chart produced by \code{chartSeries}
by simply specifying the dates of interest

\code{zoom} is an interactive chart version
of \code{zoomChart} which utilizes the standard
\R device interaction tool \code{locator} to
estimate the subset desired. This estimate is
then passed to zoomChart for actual redrawing.
At present it is
quite experimental in its interface and arguments.
Its usage entails a call to \code{zoom()} followed
by the selection of the leftmost and rightmost points
desired in the newly zoomed chart. This selection
is accomplished by the user left-clicking each extreme
point.  Two click are required to determine the level
of zooming.  Double clicking will reset the chart to
the full data range.  The arguments and internal
working of this function are likely to change dramatically
in future releases, though its use will likely remain.

Standard format for the \code{subset} argument
is the same as the subsetting for xts objects,
which is how the data is stored internally for
rendering.

Calling \code{zoomChart} with no arguments (NULL)
resets the chart to the original data.

Examples include '2007' for all of the year 2007,
'2007::2008' for years 2007 through 2008, '::2007'
for all data from the beginning of the set to the
end of 2007, '2007::' all data from the beginning of
2007 through the end of the data.  For specifics
regarding the level of detail and internal interpretation
please see \code{[.xts}
}
\value{
This function is called for its side effect - notably
changing the perspective of the current chart, and
changing its formal subset level.  The underlying
data attached to the chart is left unchanged.
}
\author{ Jeffrey A. Ryan }
\seealso{ \code{\link{chartSeries}} }
\examples{
\dontrun{
data(sample_matrix)
chartSeries(sample_matrix)
zoomChart('2007-04::')
zoomChart()

zoom() # interactive example
}
}
\keyword{ utilities }
