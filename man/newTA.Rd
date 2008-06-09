\name{newTA}
\alias{newTA}
\alias{addTA}
\title{ Create A New TA Indicator For chartSeries }
\description{
Functions to assist in the creation of indicators or
content to be drawn on plots produced by chartSeries.
}
\usage{
addTA(ta,
      order = NULL,
      on = NA,
      legend = "auto",
      ...)

newTA(FUN,
      tFUN,
      on = NA,
      legend.name,
      fdots = TRUE,
      cdots = TRUE,
      data.at = 1,
      ...)
}
\arguments{
  \item{ta}{ data to be plotted }
  \item{order}{ which should the columns (if > 1) be plotted }
  \item{legend}{ what custom legend text should be added to the chart. }
  \item{FUN}{ Main filter function name - as a symbol } 
  \item{tFUN}{ Pre-filter transformation or extraction function }
  \item{on}{ where to draw }
  \item{legend.name}{ optional legend heading, automatically derived otherwise }
  \item{fdots}{ should any \dots be included in the main filter call }
  \item{cdots}{ should any \dots be included in the resultant function object.
                \code{fdots=TRUE} will override this to TRUE. }
  \item{data.at}{ which arguement to the main filter function is for data. }
  \item{\dots}{ any additonal graphical parameters/default to be included. }
}
\details{
Both \code{addTA} and \code{newTA} can be used to
dynamically add custom content to a displayed chart.

\code{addTA} takes a series of values, either in
a form coercible to \code{xts} or of the same length as
the charted series has rows, and displays the results in
either a new TA sub-window, or over/underlayed on
the main price chart.  If the object can be coerced to
\code{xts}, the time values present must only be within
the original series time-range.  Internally a merge
of dates occurs and will allow for the plotting
of discontinuous series.

The \code{order} argument allows for multiple column
data to be plotted in an order that makes the most visual
sense.

Specifying a \code{legend} will override the standard
parsing of the addTA call to attempt a guess at a suitable
title for the sub-chart. Specifying this will cause the standard
last value to \emph{not} be printed. 

The \dots arg to \code{addTA} is used to set graphical parameters
interpretable by \code{lines}.

\code{newTA} acts as more of a skeleton function, taking
functions as arguments, as well as charting parameters,
and returns a function that can be called in the same
manner as the built-in TA tools, such as \code{addRSI} and
\code{addMACD}. Essentially a dynamic code generator
that allows for highly customizable chart tools with
minimal (possibly zero) coding.  It is also possible
to modify the resultant code to further change behavior.

To create a new TA function with \code{newTA} certain arguments
must be specified.

The \code{FUN} argument is a function symbol (or coercible to such)
that is the primary filter to be used on the core-data of a chartSeries
chart.  This can be like most of the functions
within the \pkg{TTR} package --- e.g. RSI or EMA.  The resultant object
of the function call will be equal to calling the function
on the original data passed into the chartSeries function that created the
chart.  It should be coercible to a matrix object, of one or more
columns of output.  By default all columns of output will be added to the chart,
unless suppressed by passing the appropriately positioned \code{type='n'} as
the \dots arg. Note that this will not suppress the labels added to the chart.

The \code{tFUN} argument will be called on the main chart's data prior
to passing it to FUN. This must be a function symbol or a character
string of the name function to be called.

The \code{on} is used to identify which subchart to add the
graphic to.  By default, \code{on=NA} will draw the series
in a new subchart below the last indicator.  Setting this
to either a positive or negative value will allow for
the series to be super-imposed on, or under, the (sub)chart
specified, respectively. A value of 1 refers to the main chart, and at present
is the only location supported.

\code{legend.name} will change the main label for a new plot.

\code{fdots} and \code{cdots} enable inclusion or suppression
of the \dots within the resulting TA code's call to \code{FUN},
or the argument list of the new TA function, respectively.
In order to facilitate user-specified graphical
parameters it is usually desireable to not impose 
artificial limits on the end-user
with constraints on types of parameters available.
By default the new TA function will include the dots
argument, and the internal FUN call will keep all arguments, including
the dots.  This may pose issues if the internal function then
passes those \dots arguments to a function that can't handle them.

The final argument is \code{data.at} which is the position
in the \code{FUN} argument list which expects the data
to be passed in at.  This default to the sensible
first position, though can be changed at the time of creation
by setting this argument to the required value.

While the above functions are usually sufficient
to construct very pleasing graphical additions to
a chart, it may be necessary to modify by-hand
the code produced.  This can be accomplished by
dumping the function to a file, or using \code{fix}
on it during an interactive session.

Another item of note, with respect to \code{newTA} is the
naming of the main legend label.  Following
addTA convention, the first \sQuote{add} is stripped
from the function name, and the rest of the call's
name is used as the label.  This can be overridden
by specifying \code{legend.name} in the construction
of the new TA call, or by passing \code{legend} into
the new TA function.  Subtle differences exist, with
the former being the preferred solution.

While both functions can be used to build new indicators
without any understanding of the internal chartSeries process,
it may be beneficial in more complex cases to have a knowledge
of the multi-step process involved in creating a chart via
chartSeries.

to be added...
}
\value{
\code{addTA} will invisibly return an S4 object of
class \code{chobTA}. If this function is called
interactively, the \code{chobTA} object will be
evaluated and added to the current chart.

\code{newTA} will return a function object that
can either be assigned or evaluated.  Evaluating
this function will follow the logic of any standard
addTA-style call, returning invisibly a \code{chobTA}
object, or adding to the chart.
}
\author{ Jeffrey A. Ryan }
\note{ 
Both interfaces are meant to fascilitate custom
chart additions.  \code{addTA} is for
adding any arbitrary series to a chart, where-as
\code{newTA} works with the underlying series with
the main chart object.  The latter also
acts as a dynamic TA skeleton generation tool
to help develop reusable TA generation code
for use on any chart.
}
\seealso{ \code{\link{chartSeries}},
          \code{\link{TA}},
          \code{\linkS4class{chob}},
          \code{\linkS4class{chobTA}} }
\examples{
\dontrun{
getSymbols('SBUX')
barChart(SBUX)
addTA(EMA(Cl(SBUX)), on=1, col=6)
addTA(OpCl(SBUX), col=4, type='b', lwd=2)
# create new EMA TA function
newEMA <- newTA(EMA, Cl, on=1, col=7)
newEMA()
newEMA(on=NA, col=5)
}
}
\keyword{ aplot }
\keyword{ dplot }
\keyword{ hplot }
