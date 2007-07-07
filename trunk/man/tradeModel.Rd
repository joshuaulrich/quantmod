\name{tradeModel}
\alias{tradeModel}
\title{ Simulate Trading of Fitted quantmod Object }
\description{
Simulated trading of fitted quantmod object.  Given a fitted model, 
tradeModel calculates the signal generated over a given historical
period, then applies specified \code{trade.rule} to calculate
and return a \code{tradeLog} object.  Additional methods can then 
be called to evaluate the performance of the model's strategy.
}
\usage{
tradeModel(x,
           signal.threshold = c(0, 0),
           leverage = 1,
           return.model = TRUE, 
           plot.model = FALSE,
           trade.dates = NULL,
           exclude.training = TRUE,
           ret.type = c("weeks", "months", "quarters", "years"),
           ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{  }
  \item{signal.threshold}{ ~~Describe \code{signal.threshold} here~~ }
  \item{leverage}{ ~~Describe \code{leverage} here~~ }
  \item{return.model}{ ~~Describe \code{return.model} here~~ }
  \item{plot.model}{ ~~Describe \code{plot.model} here~~ }
  \item{trade.dates}{ ~~Describe \code{trade.dates} here~~ }
  \item{exclude.training}{ ~~Describe \code{exclude.training} here~~ }
  \item{ret.type}{ ~~Describe \code{ret.type} here~~ }
  \item{\dots}{ ~~Describe \code{\dots} here~~ }
}
\details{
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\seealso{  \code{\link{specifyModel}} \code{\link{buildModel}} }
\examples{
}
\keyword{ models }% at least one, from doc/KEYWORDS
