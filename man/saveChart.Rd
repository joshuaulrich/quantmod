\name{saveChart}
\alias{saveChart}
\title{ Save Chart to External File }
\description{
Save selected chart to an external file.
}
\usage{
saveChart(.type = "pdf", ..., dev = dev.cur())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{.type}{ type of export. See Details. }
  \item{\dots}{ arguments to pass to device }
  \item{dev}{ which device should be exported }
}
\details{
This function wraps the base R function \code{pdf},
\code{postscript}, \code{png}, \code{jpeg}, and \code{bitmap}.
The \code{.type} argument must specify which device
driver is desired.

The currently active device is used if \code{dev} is
missing.  The result is an exact copy (within the device limits)
of the chart specified.

The name of the resultant file is derived from the name of the chart,
with the appropriate extension appended. (from \code{.type}). Specifying
the appropriate device file/filename will override this name.

The caller may specify any parameters that are valid for the device called.
Internally, effort is made to match the dimensions of the device being
used to create the output file.  User supplied dimensions will override this
internal calculation.
}
\value{
A file in the current directory (default) matching the type of
the output requested.
}
\author{ Jeffrey A. Ryan }
\note{ 
As this uses \code{do.call} internally to create the new output device,
any device that makes use of R conventions should be acceptable as a
value for \code{.type}
}
\seealso{ \code{\link{pdf}}
          \code{\link{png}}
          \code{\link{jpeg}}
          \code{\link{bitmap}}
          \code{\link{postscript}}
 }
\examples{
\dontrun{
getSymbols("AAPL")

chartSeries(AAPL)

require(TTR)
addBBands()

saveChart('pdf')
saveChart('pdf', width=13)
}
}
\keyword{ aplot }
\keyword{ device }
\keyword{ dplot }
