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

\seealso{
\code{\link{getSymbols}} load data
\code{\link{specifyModel}} create model structure
\code{\link{buildModel}} construct model
\code{\link{modelData}} extract model dataset
}
\examples{
\dontrun{
my.model <- specifyModel(Next(OpCl(QQQQ)) ~ Lag(Cl(NDX),0:5))
getModelData(my.model)
}
}
\keyword{ datasets }% at least one, from doc/KEYWORDS
