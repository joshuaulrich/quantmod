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
  \item{training.per}{ A character vector of length 2, with each element of the form \code{CCYY-MM-DD}, establishing the start and end of training data }
  \item{method}{ A character string nameing the fitting method.
                 See details section for available methods, and 
                 how to create new methods.}
  \item{\dots}{ Additional arguments to method call }
}
\details{
Currently available method include: lm, glm, ppr (stats), step, 
rpart (rpart), tree (tree), randomForest (randomForest), mars (mda), 
polymars (polspline), lars (lars), rq (quantreg),
lqs (MASS), rlm (MASS), loess, and nnet (nnet).  
\cr
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
getSymbols('QQQQ',src='yahoo')  # load QQQQ data set
q.model = specifyModel(Next(OpCl(QQQQ)) ~ Lag(OpHi(QQQQ),0:3))
} 

\keyword{ models }% at least one, from doc/KEYWORDS
