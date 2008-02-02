\name{setTA}
\alias{setTA}
\alias{unsetTA}
\alias{listTA}
\title{ Manage TA Argument Lists }
\description{
Used to manage the TA arguments used inside \code{chartSeries}
calls.
}
\usage{
setTA(type = c("chartSeries", "barChart", "candleChart"))

listTA(dev)
}
\arguments{
  \item{type}{ the function to apply defaults TAs to }
  \item{dev}{ the device to display TA arguments for }
}
\details{
\code{setTA} and \code{unsetTA} provide a simple
way to reuse the same TA arguments for multiple
charts.  By default all charting functions will be
set to use the current chart's defaults.

It is important to note that the \emph{current} device
will be used to extract the list of TA arguments to apply.
This is done with a call to listTA internally, and followed
by calls to setDefaults of the appropriate functions.

An additional way to set default TA arguments for subsequent
charts is via \code{setDefaults}. See the examples.
}
\value{
Called for its side-effect of setting the default
TA arguments to quantmod's charting functions.
}
\author{ Jeffrey A. Ryan }
\note{ 
Using \code{setDefaults} directly will require the vector
of default TA calls to be wrapped in a call to 
\code{substitute} to prevent unintended evaluations; \emph{OR}
a call to \code{listTA} to get the present TA arguments.  This last
approach is what \code{setTA} wraps.
}
\seealso{ \code{\link{chartSeries}},
          \code{\link{addTA}} }
\examples{
\dontrun{
listTA()
setTA()

# a longer way to accomplish the same
setDefaults(chartSeries,TA=listTA())
setDefaults(barCart,TA=listTA())
setDefaults(candleChart,TA=listTA())

setDefaults(chartSeries,TA=substitute(c(addVo(),addBBands())))
}
}
\keyword{ utilities }
