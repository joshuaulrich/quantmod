\name{Delt}
\alias{Delt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Calculate Percent Change }
\description{
Calculate the k-period percent difference within one series, or
between two series.  Primarily used to calculate the percent change
from one period to another of a given series, or to calculate
the percent difference between two series over the full series.
}
\usage{
Delt(x1, x2 = NULL, k = 0, type = c("arithmetic", "log"))
}
\arguments{
  \item{x1}{ \emph{m x 1} vector }
  \item{x2}{ \emph{m x 1} vector }
  \item{k}{ change over \code{k}-periods. default k=1 when x2 is NULL. }
  \item{type}{ type of difference. log or arithmetic (defauly). }
}
\details{
When called with only \code{x1}, the one period percent change of the
series is returned by default. Internally this happens by copying
x1 to x2. A two period difference would be specified with \code{k=2}.

If called with both \code{x1} and \code{x2}, the difference between
the two is returned.  That is, k=0. A one period difference would be
specified by \code{k=1}. \code{k} may also be a vector to calculate
more than one period at a time. The results will then be an m x length(k)
 
Log differences are used by default:
Lag = log(x2(t)/x1(t-k))


Arithmetic differences are calculated:
Lag = (x2(t) - x1(t-k))/x1(t-k)
}
\value{
An matrix of \code{length(x1)} rows and \code{length(k)} columns.
}
\author{ Jeffrey A. Ryan }
\seealso{ \code{\link{OpOp}} \code{\link{OpCl}} }
\examples{
Stock.Open <- c(102.25,102.87,102.25,100.87,103.44,103.87,103.00)
Stock.Close <- c(102.12,102.62,100.12,103.00,103.87,103.12,105.12)

Delt(Stock.Open)                    #one period pct. price change
Delt(Stock.Open,k=1)                #same
Delt(Stock.Open,type='arithmetic')  #using arithmetic differences

Delt(Stock.Open,Stock.Close)        #Open to Close pct. change
Delt(Stock.Open,Stock.Close,k=0:2)  #...for 0,1, and 2 periods

}
\keyword{ utilities }
