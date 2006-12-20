\name{buildModel}
\alias{buildModel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Fit model to tR.model }
\description{
Construct and attach a fitted model of type \code{method} to \code{tR.model} object.
}
\usage{
buildModel(tR.model, training.per, method, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tR.model}{ An object of class \code{tR.model} specified by \code{\link{specifyModel}} }
  \item{training.per}{ A character vector of length 2, with each element of the form \code{CCYY-MM-DD}, establishing the start and end of training data }
  \item{method}{ A character string nameing the fitting method. Currently available method include: lm, glm, ppr(stats), step, rpart(rpart), tree(tree), randomForest(randomForest), mars(mda), polymars(polspline), lars(lars), rq(quantreg), lqs(MASS), rlm(MASS), loess, and nnet(nnet).  Additional methods can be added by writing functions named tR.[name]. }
  \item{\dots}{ Additional arguments to method call }
}
\details{
Additional methods wrappers can be created to allow for modelling using custom functions.  The only requirements are for a wrapper function to be constructed taking parameters \code{tR.model}, \code{training.data}, and \dots.  The function must return the fitted model object and have a predict method available.  It is possible to add predict methods if non exist by adding an S3 method for predictModel.
}
\value{
An object of class \code{tR.model}
}
\author{ Jeffrey Ryan }
\note{ See tR.wrappers.R in the source for examples on extending modelling methods.
}
\seealso{ \code{\link{specifyModel}} \code{\link{tradeModel}} }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
data(QQQQ)  # load QQQQ data set
q.model = specifyModel(next.per(delta(Op(QQQQ),Cl(QQQQ))) ~ delta(Op(QQQQ),Cl(QQQQ)))
 

\keyword{ model }% at least one, from doc/KEYWORDS
