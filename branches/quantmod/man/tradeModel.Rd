\name{tradeModel}
\alias{tradeModel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
tradeModel(tR.model, signal.threshold = c(0, 0), leverage = 1, return.model = TRUE, plot.model = FALSE, trade.dates = NULL, exclude.training = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tR.model}{ ~~Describe \code{tR.model} here~~ }
  \item{signal.threshold}{ ~~Describe \code{signal.threshold} here~~ }
  \item{leverage}{ ~~Describe \code{leverage} here~~ }
  \item{return.model}{ ~~Describe \code{return.model} here~~ }
  \item{plot.model}{ ~~Describe \code{plot.model} here~~ }
  \item{trade.dates}{ ~~Describe \code{trade.dates} here~~ }
  \item{exclude.training}{ ~~Describe \code{exclude.training} here~~ }
  \item{\dots}{ ~~Describe \code{\dots} here~~ }
}
\details{
  ~~ If necessary, more details than the description above ~~
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
\note{ ~~further notes~~ 

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (tR.model, signal.threshold = c(0, 0), leverage = 1, 
    return.model = TRUE, plot.model = FALSE, trade.dates = NULL, 
    exclude.training = TRUE, ...) 
{
    trade.offset = 0
    if (class(tR.model) != "tR.model") 
        stop("model must be of class tR.model")
    if (!is.null(trade.dates) & length(trade.dates) < 2) 
        stop("trade.dates must be of length 2")
    model.data <- modelData(tR.model, trade.dates, exclude.training = exclude.training)
    fitted.zoo <- predict(tR.model@fitted.model, model.data[, 
        -1])
    if (class(fitted.zoo) != "zoo") {
        fitted.zoo <- zoo(fitted.zoo, index(model.data))
    }
    signal.zoo <- ifelse(fitted.zoo < signal.threshold[1] | fitted.zoo > 
        signal.threshold[2], ifelse(fitted.zoo > 0, 1, -1), 0)
    tmp.index <- index(signal.zoo)[-(1 + trade.offset)]
    market.zoo <- model.data[-(nrow(model.data) + trade.offset), 
        1]
    signal.zoo <- signal.zoo[-c(length(index(signal.zoo)) - trade.offset, 
        length(index(signal.zoo)))]
    signal.zoo = merge(market.zoo, signal.zoo)
    index(signal.zoo) <- tmp.index
    tR.results <- new("tR.results", model = tR.model, signal = signal.zoo)
    model.returns <- modelReturn(tR.results, trade.dates = trade.dates, 
        leverage = leverage, ...)
    tR.results@return <- model.returns
    return(tR.results)
  }
}
\keyword{ ~kwd1 }% at least one, from doc/KEYWORDS
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
