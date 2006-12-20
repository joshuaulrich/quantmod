\name{getModelData}
\alias{getModelData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Update model's dataset }
\description{
Update currently specified or built model with most recent data.  Defaults to using database connection specified in tR.options.  If fitted model has fewer independent variables than originally specified, only those variables and transformations will be saved. 
}
\usage{
getModelData(model, na.rm = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{ An object of class \code{tR.model} }
  \item{na.rm}{ Boolean. Remove NA values. Defaults to TRUE }
}
\details{
Primarily used within specify model calls, \code{getModelData} is used to retrieve the appropriate underlying variables, and apply model specified transformations automatically.  It can be used to also update a current model in memory with the most recent data.
}
\value{
Returns object of class \code{tR.model}
}
\author{ Jeffrey Ryan }

\seealso{ \code{\link{tR.model}} \code{\link{specifyModel}} \code{\link{buildModel}}}
\examples{

\keyword{ datasets }% at least one, from doc/KEYWORDS
