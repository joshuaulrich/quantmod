\name{quantmod-class}
\docType{class}
\alias{quantmod-class}
\alias{summary,quantmod-method}
\alias{show,quantmod-method}
\alias{fittedModel<--methods}
\alias{fittedModel<-,quantmod-method}

\alias{quantmodResults-class}
\alias{show,quantmodResults-method}

\alias{quantmodReturn-class}

\alias{tradeLog-class}
\alias{show,tradeLog-method}

\title{Class "quantmod" }
\description{   Objects of class \code{quantmod} help to 
manage the process of model building within the quantmod
package.  Created automatically by a call to \code{specifyModel}
they carry information to be used by a variety of accessor
functions and methods.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("quantmod", ...)}.

Normally objects are created as a result of a call to \code{specifyModel}.
}
\section{Slots}{
	 \describe{
    \item{\code{model.id}:}{Object of class \code{"character"} ~~ }
    \item{\code{model.spec}:}{Object of class \code{"formula"} ~~ }
    \item{\code{model.formula}:}{Object of class \code{"formula"} ~~ }
    \item{\code{model.target}:}{Object of class \code{"character"} ~~ }
    \item{\code{model.inputs}:}{Object of class \code{"character"} ~~ }
    \item{\code{build.inputs}:}{Object of class \code{"character"} ~~ }
    \item{\code{symbols}:}{Object of class \code{"character"} ~~ }
    \item{\code{product}:}{Object of class \code{"character"} ~~ }
    \item{\code{price.levels}:}{Object of class \code{"zoo"} ~~ }
    \item{\code{training.data}:}{Object of class \code{"Date"} ~~ }
    \item{\code{build.date}:}{Object of class \code{"Date"} ~~ }
    \item{\code{fitted.model}:}{Object of class \code{"ANY"} ~~ }
    \item{\code{model.data}:}{Object of class \code{"zoo"} ~~ }
    \item{\code{quantmod.version}:}{Object of class \code{"numeric"} ~~ }
  }
}
\section{Methods}{
No methods defined with class "quantmod" in the signature.
}
\author{ Jeffrey A. Ryan }
\examples{
showClass("quantmod")
}
\keyword{classes}
