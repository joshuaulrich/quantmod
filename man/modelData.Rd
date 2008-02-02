\name{modelData}
\alias{modelData}
\title{ Extract Dataset Created by specifyModel }
\description{
Extract from a \code{quantmod} object the
dataset created for use in modelling.

specifyModel creates a \code{zoo} object for
use in subsequent workflow stages (
\code{buildModel},\code{tradeModel}) that
combines all model inputs, from a variety
of sources, into one model frame.

\code{modelData} returns this object.
}
\usage{
modelData(x, data.window = NULL, exclude.training = FALSE)
}
\arguments{
  \item{x}{ a \code{quantmod} object } 
  \item{data.window}{ a character vector of subset start
                      and end dates to return }
  \item{exclude.training}{ remove training period }
}
\details{
When a model is created by \code{specifyModel}, it
is attached to the returned object. One of the
slots of this S4 class is \code{model.data}.
}
\value{
an object of class \code{zoo} containing all
transformations to data specified in
\code{specifyModel}.
}
\author{ Jeffrey A. Ryan }
\seealso{ \code{\link{specifyModel}},\code{\link{getModelData}} }
\examples{
\dontrun{
m <- specifyModel(Next(OpCl(SPY)) ~ Cl(SPY) + OpHi(SPY) + Lag(Cl(SPY)))
modelData(m) 
}
}
\keyword{ data}
\keyword{ utilities }% __ONLY ONE__ keyword per line
