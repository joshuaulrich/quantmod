\name{peak}
\alias{peak}
\alias{valley}
\title{ Find Peaks and Valleys In A Series }
\description{
Functions to find the peaks (tops) and valleys (bottoms)
of a given series.
}
\usage{
peak(x)
valley(x)
}
\arguments{
  \item{x}{ a time series }
}
\value{
A vector of integers corresponding to peaks/valleys.

As a peak[valley] is defined as the highest[lowest] value in a series,
the function can only define it after a change in direction
has occurred.  This means that the function will always
return the first period \emph{after} the peak/valley of the
data, so as not to accidentally induce a look-ahead bias.
}
\author{ Jeffrey A. Ryan }
\examples{
peak(sin(1:10))

p <- peak(sin(seq(1,10,.1)))
sin(seq(1,10,.1))[p]

plot(sin(seq(1,10,.1))[p])
plot(sin(seq(1,10,.1)),type='l')
points(p,sin(seq(1,10,.1))[p])
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ misc }
