\name{getSymbols}
\alias{getSymbols}
\alias{getSymbols.MySQL}
\alias{getSymbols.Oracle}
\alias{getSymbols.postgresql}
\alias{getSymbols.sqlite}
\alias{getSymbols.RData}
\alias{getSymbols.csv}
\alias{getSymbols.yahoo}
\alias{getSymbols.google}
\alias{getSymbols.economagic}
\alias{getSymbols.statlib}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Load symbol(s) into environment }
\description{
Construct and attach a fitted model of type \code{method} to \code{quanmod} object.
}
\usage{
getSymbols(symbol,
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{quantmod}{ An object of class \code{quantmod} specified by \code{\link{specifyModel}} }
  \item{training.per}{ A character vector of length 2, with each element of the form \code{CCYY-MM-DD}, establishing the start and end of training data }
  \item{method}{ A character string nameing the fitting method. Currently available method include: lm, glm, ppr(stats), step, rpart(rpart), tree(tree), randomForest(randomForest), mars(mda), polymars(polspline), lars(lars), rq(quantreg), lqs(MASS), rlm(MASS), loess, and nnet(nnet).  Additional methods can be added by writing functions named buildModel.[name] and predictModel.[name]. }
  \item{\dots}{ Additional arguments to method call }
}
\details{
Additional methods wrappers can be created to allow for modelling using custom functions.  The only requirements are for a wrapper function to be constructed taking parameters \code{quantmod}, \code{training.data}, and \dots.  The function must return the fitted model object and have a predict method available.  It is possible to add predict methods if non exist by adding an S3 method for predictModel.
}
\value{
An object of class \code{quantmod}
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
