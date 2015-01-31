\name{chartTheme}
\alias{chartTheme}
\title{ Create A Chart Theme }
\description{
Create a chart.theme object for use within chartSeries
to manage desired chart colors.
}
\usage{
chartTheme(theme = "black", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theme}{ name of base theme }
  \item{\dots}{ name=value pairs to modify }
}

\details{
Used as an argument to the chartSeries family of functions,
\code{chartTheme} allows for on-the-fly modification
of pre-specified chart \sQuote{themes}.  Users can modify
a pre-built theme in-place, or copy the theme to a new
variable for use in subsequent charting calls.

Internally a chart.theme object is nothing more than
a list of values organized by chart components. The primary
purpose of this is to facilitate minor modification on
the fly, as well as provide a template for larger changes.

Setting style arguments for TA calls via chartTheme requires
the user to pass the styles as name=value pairs with a name
containing the TA call in question.  See examples for assistance.

Current components that may be modified with appropriate
values:
\itemize{
  \item{fg.col}{foreground color}
  \item{bg.col}{background color}
  \item{grid.col}{grid color}
  \item{border}{border color}
  \item{minor.tick}{minor tickmark color}
  \item{major.tick}{major tickmark color}
  \item{up.col}{up bar/candle color}
  \item{dn.col}{down bar/candle color}
  \item{up.up.col}{up after up bar/candle color}
  \item{up.dn.col}{up after down bar/candle color}
  \item{dn.dn.col}{down after down bar/candle color}
  \item{dn.up.col}{down after up bar/candle color}
  \item{up.border}{up bar/candle border color}
  \item{dn.border}{down bar/candle border color}
  \item{up.up.border}{up after up bar/candle border color}
  \item{up.dn.border}{up after down bar/candle border color}
  \item{dn.dn.border}{down after down bar/candle border color}
  \item{dn.up.border}{down after up bar/candle border color}
}
}
\value{
A chart.theme object
}
\author{ Jeffrey A. Ryan }
\seealso{ \code{\link{chartSeries}} }
\examples{
chartTheme()
chartTheme('white')
chartTheme('white',up.col='blue',dn.col='red')

# A TA example
chartTheme(addRSI.col='red')

str(chartTheme())
}
\keyword{ utilities }
