\name{breakpoints}
\alias{breakpoints}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
breakpoints(x, by = c(weekdays, weeks, months, quarters, years), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ ~~Describe \code{x} here~~ }
  \item{by}{ ~~Describe \code{by} here~~ }
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
function (x, by = c(weekdays, weeks, months, quarters, years), 
    ...) 
{
    if (length(by) != 1) 
        stop("choose ONE method for \"by\"")
    by <- match.fun(by)
    breaks <- which(diff(as.numeric(by(x, ...))) != 0)
    breaks <- c(0, breaks, NROW(x))
    return(breaks)
  }
}
\keyword{ ~kwd1 }% at least one, from doc/KEYWORDS
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
