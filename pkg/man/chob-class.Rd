\name{chob-class}
\docType{class}
\alias{chob-class}

\title{A Chart Object Class }
\description{Internal Objects for Tracking and Plotting Chart Changes }
\section{Objects from the Class}{
Objects are created internally through the charting
functions \code{chartSeries}, \code{barChart}, \code{lineChart}, and
\code{candleChart}.
}
\section{Slots}{
	 \describe{
    \item{\code{device}:}{Object of class \code{"ANY"} ~~ }
    \item{\code{call}:}{Object of class \code{"call"} ~~ }
    \item{\code{xdata}:}{Object of class \code{"ANY"} ~~ }
    \item{\code{xsubset}:}{Object of class \code{"ANY"} ~~ }
    \item{\code{name}:}{Object of class \code{"character"} ~~ }
    \item{\code{type}:}{Object of class \code{"character"} ~~ }
    \item{\code{passed.args}:}{Object of class \code{"ANY"} ~~ }
    \item{\code{windows}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{xrange}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{yrange}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{length}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{color.vol}:}{Object of class \code{"logical"} ~~ }
    \item{\code{multi.col}:}{Object of class \code{"logical"} ~~ }
    \item{\code{show.vol}:}{Object of class \code{"logical"} ~~ }
    \item{\code{show.grid}:}{Object of class \code{"logical"} ~~ }
    \item{\code{line.type}:}{Object of class \code{"character"} ~~ }
    \item{\code{bar.type}:}{Object of class \code{"character"} ~~ }
    \item{\code{xlab}:}{Object of class \code{"character"} ~~ }
    \item{\code{ylab}:}{Object of class \code{"character"} ~~ }
    \item{\code{spacing}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{width}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{bp}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{x.labels}:}{Object of class \code{"character"} ~~ }
    \item{\code{colors}:}{Object of class \code{"ANY"} ~~ }
    \item{\code{layout}:}{Object of class \code{"ANY"} ~~ }
    \item{\code{time.scale}:}{Object of class \code{"ANY"} ~~ }
    \item{\code{major.ticks}:}{Object of class \code{"ANY"} ~~ }
    \item{\code{minor.ticks}:}{Object of class \code{"logical"} ~~ }
  }
}
\section{Methods}{
No methods defined with class "chob" in the signature.
}
\author{ Jeffrey A. Ryan }
\seealso{
	\code{\link{chartSeries}},
	or \code{\linkS4class{chobTA}} for links to other classes
}
\examples{
showClass("chob")
}
\keyword{classes}
