\name{buildModel}
\alias{buildModel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Build quantmod model given specified fitting method }
\description{
Construct and attach a fitted model of type \code{method} to \code{quantmod} object.
}
\usage{
buildModel(x, training.per, method, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ An object of class \code{quantmod} created with \code{\link{specifyModel}} or
an \R formula }
  \item{training.per}{ character vector representing dates in ISO 8601 format
                       of length 2, e.g. c('1992-01-01','1992-04-01')  
  \item{method}{ A character string naming the fitting method.
                 See details section for available methods, and 
                 how to create new methods.}
  \item{\dots}{ Additional arguments to method call }
}
\details{
Currently available methods include:

lm, glm, loess, step, ppr,
rpart[rpart], tree[tree],
randomForest[randomForest], mars[mda],
polymars[polspline], lars[lars], rq[quantreg],
lqs[MASS], rlm[MASS], and nnet[nnet].

Additional methods wrappers can be created to allow for modelling 
using custom functions.  The only requirements are for a wrapper 
function to be constructed taking parameters \code{quantmod}, 
\code{training.data}, and \dots.  The function must return the 
fitted model object and have a predict method available.  
It is possible to add predict methods if non exist by 
adding an S3 method for predictModel. The \code{
buildModel.skeleton} function can be used for new methods.
}
\value{
An object of class \code{quantmod} with fitted model attached
}
\author{ Jeffrey Ryan }
\note{ See \code{buildModel.skeleton} for information on adding additional methods
}
\seealso{ \code{\link{specifyModel}} \code{\link{tradeModel}} }
\examples{
\dontrun{
getSymbols('QQQQ',src='yahoo')
q.model = specifyModel(Next(OpCl(QQQQ)) ~ Lag(OpHi(QQQQ),0:3))
buildModel(q.model,method='lm',training.per=c('2006-08-01','2006-09-31'))
}
} 

\keyword{ models }% at least one, from doc/KEYWORDS
