\name{chobTA-class}
\docType{class}
\alias{chobTA-class}
\alias{show,chobTA-method}

\title{A Technical Analysis Chart Object }
\description{ Internal storage class for handling arbitrary TA objects }
\section{Objects from the Class}{
Objects of class \code{chobTA} are created and returned invisibly
through calls to addTA-style functions.
}
\section{Slots}{
	 \describe{
    \item{\code{call}:}{Object of class \code{"call"} ~~ }
    \item{\code{on}:}{Object of class \code{"ANY"} ~~ }
    \item{\code{new}:}{Object of class \code{"logical"} ~~ }
    \item{\code{TA.values}:}{Object of class \code{"ANY"} ~~ }
    \item{\code{name}:}{Object of class \code{"character"} ~~ }
    \item{\code{params}:}{Object of class \code{"ANY"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "chobTA")}: ... }
	 }
}
\author{ Jeffrey A. Ryan }
\note{ It is of no external vaule to handle chobTA objects directly }
\seealso{
\code{\link{addTA}}, ~~~
	or \code{\linkS4class{chob}} for links to other classes
}
\examples{
showClass("chobTA")
}
\keyword{classes}
