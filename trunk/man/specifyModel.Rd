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
    \item{na.rm}{remove all incomplete rows.}
}
\details{
Models are specified through the standard formula mechanism.

As financial models may include a
variety of financial and economic indicators,
each differing in source, frequency, and/or class,
a single mechanism to specify sources is
included within a call to specifyModel.
See \code{getModelData} for details of
how this process works.

Currently, objects of class \code{quantmod.OHLC},
\code{zoo}, \code{ts} and \code{its} are supported within the
model formula.

All symbols are first retrieved from the global environment, without inheritence.
If an object is not found in the global environment, it is added to a
list of objects to load through the \code{getSymbols}
function. getSymbols retrieves each
object specified by using information
as to its location specified apriori 
via \code{setDefaults} or \code{setSymbolLookup}.

Internally all data is coerced to \code{zoo},\code{data.frame},
or \code{numeric} classes.
}

\value{
Returns an object of class \code{quantmod}.
Use \code{modelData} to extract
full data set as \code{zoo} object.
}

\author{ Jeffrey Ryan }
\references{
  quantmod.com \url{http://www.quantmod.com}
}
\note{ It is possible to include any supported series
       in the formula by simply specifying
       the object's symbol. See *Details* for a list
       of currently supported classes.

       Use \code{getSymbols.skeleton} to create additional 
       methods of data sourcing, e.g. from a proprietary
       data format or currently unimplemented source
       (Bloomberg, Oracle).

       See \code{getSymbols.MySQL} and \code{getSymbols.yahoo}
       for examples of adding additional functionality}


\seealso{ \code{\link{getModelData}},\code{\link{getSymbols}},
\code{\link{buildModel}},\code{\link{tradeModel}},\code{\link{formula}}
\code{\link{setSymbolLookup}}}
\examples{
\dontrun{
# if QQQQ is not in the Global environment, an attempt will be made
# to retrieve it from the source specified with getSymbols.Default

specifyModel(Next(OpCl(QQQQ)) ~ Lag(OpHi(QQQQ),0:3) + Hi(DIA))
}
}

\keyword{ models }% at least one, from doc/KEYWORDS
