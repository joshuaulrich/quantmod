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
  \item{x}{ a quantmod object from \code{buildModel}  }
  \item{signal.threshold}{ a numeric vector describing simple lower
                           and upper thresholds before trade occurs}
  \item{leverage}{ amount of leverage to apply - currently a constant }
  \item{return.model}{ should the full model be returned? }
  \item{plot.model}{ plot the model? }
  \item{trade.dates}{ specific trade interval - defaults to full dataset }
  \item{exclude.training}{ exclude the period trained on? }
  \item{ret.type}{ a table of period returns }
  \item{\dots}{ additional parameters needed by the underlying modelling function, if any }
}
\details{
Still highly experimental and changing. The purpose is to apply
a newly contructed model from \code{buildModel} to a new dataset
to investigate the model's trading potential.

At present all parameters are very basic. The near term changes include
allowing for a trade.rule argument to allow for a dynamic trade rule given
a set of signals. Additional the application of variable leverage and costs
will become part of the final structure.

Any suggestions as to inclusions or alterations are appreciated and should
be directed to the maintainer of the package.
}
\value{
A quantmodResults object
}
\author{ Jeffrey A. Ryan }
\seealso{  \code{\link{specifyModel}} \code{\link{buildModel}} }
\examples{
\dontrun{
m <- specifyModel(Next(OpCl(QQQQ)) ~ Lag(OpHi(QQQQ)))
m.built <- buildModel(m,method='rpart',training.per=c('2007-01-01','2007-04-01'))

tradeModel(m.built)
tradeModel(m.built,leverage=2)
}
}
\keyword{ models }% at least one, from doc/KEYWORDS
