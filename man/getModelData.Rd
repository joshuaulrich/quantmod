\name{getModelData}
\alias{getModelData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Update model's dataset }
\description{
Update currently specified or built model with most recent data.  
}
\usage{
getModelData(x, na.rm = TRUE)
}
\arguments{
  \item{x}{ An object of class \code{quantmod} }
  \item{na.rm}{ Boolean. Remove NA values. Defaults to TRUE }
}
\details{
Primarily used within specify model calls,
\code{getModelData} is used to retrieve the appropriate underlying variables, 
and apply model specified transformations automatically.
It can be used to also update a current model in memory with the most recent data.
}
\value{
Returns object of class \code{quantmod.OHLC}
}
\author{ Jeffrey Ryan }

\seealso{ \code{\link{getSymbols}} \code{\link{specifyModel}} \code{\link{buildModel}}}
\examples{
}
\keyword{ datasets }% at least one, from doc/KEYWORDS
