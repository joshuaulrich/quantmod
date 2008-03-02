\name{zoomChart}
\alias{zoomChart}
\title{ Change Zoom Level Of Current Chart }
\description{
Using \pkg{xts} style date subsetting, zoom into
or out of the current chart.
}
\usage{
zoomChart(subset = NULL)
}

\arguments{
  \item{subset}{ a valid subset string }
}
\details{
This function allows for viewing of specific
areas of a chart produced by \code{chartSeries}
by simply specifying the dates of interest

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
}
}
\keyword{ utilities }
