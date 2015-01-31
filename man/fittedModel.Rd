\name{fittedModel}
\alias{fittedModel}
\alias{fittedModel<-}
\alias{formula.quantmod}
\alias{plot.quantmod}
\alias{coefficients.quantmod}
\alias{coef.quantmod}
\alias{residuals.quantmod}
\alias{resid.quantmod}
\alias{fitted.values.quantmod}
\alias{fitted.quantmod}
\alias{anova.quantmod}
\alias{logLik.quantmod}
\alias{vcov.quantmod}
\title{ quantmod Fitted Objects }
\description{
Extract and replace fitted models from
\code{quantmod} objects built with
\code{buildModel}. All objects fitted
through methods specified in \code{buildModel}
calls can be extracted for further analysis.
}
\usage{
fittedModel(object)


\method{formula}{quantmod}(x, \dots)

\method{plot}{quantmod}(x, \dots)

\method{coefficients}{quantmod}(object, \dots)

\method{coef}{quantmod}(object, \dots)

\method{residuals}{quantmod}(object, \dots)

\method{resid}{quantmod}(object, \dots)

\method{fitted.values}{quantmod}(object, \dots)

\method{fitted}{quantmod}(object, \dots)

\method{anova}{quantmod}(object, \dots)

\method{logLik}{quantmod}(object, \dots)

\method{vcov}{quantmod}(object, \dots)
}
\arguments{
  \item{object}{ a \code{quantmod} object }
  \item{x}{ a suitable object }
  \item{\dots}{ additional arguments }
}
\details{
Most often used to extract the final fitted
object of the modelling process, usually for
further analysis with tools outside
the \pkg{quantmod} package.

Most common methods to apply to fitted objects
are available to the parent \code{quantmod}
object. At present, one can call directly the
following S3 methods on a built model as if
calling directly on the fitted object.
See *Usage* section.

It is also \emph{possible} to add
a fitted model to an object. This
may be of value when applying heuristic
rule sets for trading approaches, or when
fine tuning already fit models by hand.
}
\value{
Returns an object matching that returned
by a call to the method specified in
\code{buildModel}.
}
\author{ Jeffrey A. Ryan }
\note{ 
The replacement function \code{fittedModel<-}
is highly experimental, and may or may not
continue into further releases.

The fitted model added \emph{must} use the
same names as appear in the \code{quantmod}
object's dataset.
}
\seealso{ \code{\link{quantmod}},\code{\link{buildModel}} }
\examples{
\dontrun{
x <- specifyModel(Next(OpCl(DIA)) ~ OpCl(VIX))
x.lm <- buildModel(x,method='lm',training.per=c('2001-01-01','2001-04-01'))

fittedModel(x.lm)

coef(fittedModel(x.lm))
coef(x.lm)                  # same

vcov(fittedModel(x.lm))
vcov(x.lm)                  # same
}
}
\keyword{ models }
