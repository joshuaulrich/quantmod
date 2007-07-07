\name{specifyModel}
\alias{specifyModel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Specify Model Formula For quantmod Process }
\description{
Create a single reusable model specification for subsequent buildModel calls. 
An object of class \code{quantmod} is created that can be then be reused with different
modelling methods and parameters.  No data frame is specified, as data is retrieved
from potentially multiple environments, and internal calls to getSymbols.
}
\usage{
specifyModel(formula, na.rm=TRUE)
}
\arguments{
    \item{formula}{an object of class \code{formula} (or one that can be coerced to
                   that class): a symbolic description of the model to be fitted.
                   The details of model specifcation are given under Details.}
    \item{na.rm}{remove all incomplete rows.  defaults to TRUE}
}
\details{
Models are specified through the standard formula mechanism. 

As financial models may include a variety of financial and economic indicators,
each differing in source, frequency and/or class,
a single mechanism to specify sources is included within a call to specifyModel.
All symbols are first retrieved from the global environment, without inheritence.
If an object is not found in the global environment, it is added to a
list of objects to load through the getSymbols function. getSymbols retrieves each
object specified by using information as to its location specified apriori 
via getSymbols.Default or quantmod.Default

Current transforms to \code{quantmod.OHLC} objects include:

\itemize{
    \item \code{Op:} Open
    \item \code{Lo} Low
    \item \code{Hi} High
    \item \code{Cl} Close
    \item \code{Vo} Volume
    \item \code{Ad} Adj. Close
    \item \code{OpCl} Open to Close 
    \item \code{OpHi} Open to High 
    \item \code{OpLo} Open to Low 
    \item \code{LoHi} Low to High 
    \item \code{LoCl} Low to Close 
    \item \code{HiCl} High to Close
    \item \code{OpOp} Open to Open
    \item \code{ClCl} Close to Close
    \item \code{Next}(x, k=1) next (k-)period's value
    \item \code{Lag}(x, k=1) previous (k-)period's value
    \item \code{Delt} change over (k-)periods
}
}

\value{
Returns object of class \code{quantmod}.  Use \code{modelData} to extract
full data set as \code{quantmod.OHLC} object.
}

\author{ Jeffrey Ryan }
\note{ It is possible to include additional series by simply specifying
       the objects symbol in the formula.  
       Use \code{getSymbols.skeleton} to create additional methods.
       See \code{getSymbols.MySQL} and \code{getSymbols.yahoo} for examples}


\seealso{ \code{\link{getModelData}} \code{\link{getSymbols}}
 \code{\link{buildModel}} \code{\link{tradeModel}} \code{\link{formula}}}
\examples{
\dontrun{
specifyModel(Next(OpCl(QQQQ)) ~ Lag(OpHi(QQQQ),0:3) + Hi(DIA))
}
}

}
\keyword{ models }% at least one, from doc/KEYWORDS
