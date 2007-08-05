\name{buildData}
\alias{buildData}
\title{ Create Data Object for Modelling }
\description{
Create one data object from multiple sources, applying
transformations via standard \R formula mechanism.
}
\usage{
buildData(formula, na.rm = TRUE, return.class = "zoo")
}
\arguments{
  \item{formula}{ an object of class \code{formula} (or one
that can be coerced to that class): a symbolic description
of the desired output data object, with the \code{dependent} side
corresponding to first column, and the \code{independent} parameters of
the formula to assigned to the remaining columns. }
  \item{na.rm}{ drop rows with missing values? }
  \item{return.class}{ one of the followign character strings:
zoo, data.frame, ts, its, timeSeries}
}
\details{
Makes available for use outside the \pkg{quantmod} workflow a dataset
of appropriately transformed variables, using the same mechanism
underlying \code{specifyModel}. Offering the ability to apply transformations
to raw data using a common formula mechanism, without having to explicitly
merge different data objects, or in some instances, does not require for
the desired underlying data to currently loaded in the users environment.
See \code{getSymbols} and \code{specifyModel} for more information
regarding proper usage.

Calls \code{specifyModel} followed by \code{modelData}, with the
returned object being coerced internally to the desired 'return.class'.
}
\value{
An object of class \code{return.class}.
}
\author{ Jeffrey A. Ryan }
\seealso{ \code{\link{getSymbols}}, \code{\link{specifyModel}},
          \code{\link{modelData}} }
\examples{
\dontrun{
  buildData(Next(OpCl(DIA)) ~ Lag(TBILL) + I(Lag(OpHi(DIA))^2))
}
}
\keyword{ datagen }
