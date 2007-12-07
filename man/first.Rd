\name{first}
\alias{first}
\alias{first.default}
\alias{first.zoo}
\alias{last}
\alias{last.default}
\alias{last.zoo}
\title{ Return First or Last n Elements of A Data Object }
\description{
A generic function to return the first or last
elements or rows of a vector or two-dimensional
data object.

A more advanced subsetting is available for
zoo objects with indexes inheriting from POSIXt
or Date classes.
}
\usage{
first(x,...)
last(x,...)

\method{first}{default}(x,n=1,keep=FALSE,...)

\method{last}{default}(x,n=1,keep=FALSE,...)

\method{first}{zoo}(x,n=1,keep=FALSE,...)

\method{last}{zoo}(x,n=1,keep=FALSE,...)
}
\arguments{
  \item{x}{ 1 or 2 dimensional data object }
  \item{n}{ number of periods to return }
  \item{keep}{ should removed values be kept? }
  \item{\dots}{ additional args - unused }
}
\details{
Provides the ability to identify the first or last
\code{n} rows or observations of a data set.  The generic
method behaves much like \code{head} and \code{tail} from
\pkg{base}, except by default only the \emph{first} or
\emph{last} observation will be returned.

The more useful method for the zoo class allows for
time based subsetting, given a zoo object whose index is
inherited from either \code{POSIXt} or \code{Date}.

\code{n} may be either a numeric value, indicating the number of
observations to return - forward from \code{first}, or backwards from \code{last},
or it may be a character string describing the number and type of periods to
return.

\code{n} may be positive or negative, in either numeric or character
contexts. When positive it will return the result expected - e.g.
\code{last(X,'1 month')} will return the last month's data. If negative,
all data will be returned \emph{except} for the last month. It is important
to note that this is not the same as calling \code{first(X,'1 month')} or
\code{first(X,'-1 month')}. All 4 variations return different subsets of
data and have distinct purposes.

If \code{n} is a character string, it must be of the form \sQuote{n period.type}
or \sQuote{period.type},
where \code{n} is a numeric value (defaults to 1 if not provided)
describing the number of \code{period.types}
to move forward (first) or back (last).

For example, to return the last 3 weeks of a time oriented zoo object, one
could call \code{last(X,'3 weeks')}. Valid period.types are: secs, seconds,
mins, minutes, hours, days, weeks, months, quarters, and years.

It is possible to use any frequency specification (secs, mins, days, \ldots)
for the period.type portion of the string, even if the original data is in
a higher frequency. This makes it possible to return the last \sQuote{2 months}
of data from an oject that has a daily periodicity.

It should be noted that it is only possible to extract data with methods
equal to or less than the frequency of the original data set. Attempting otherwise
will result in error.

Requesting more data than is in the original data object will produce a warning
advising as such, and the object returned will simply be the original data.
}
\value{
A subset of elements/rows of the original data. 
}
\author{ Jeffrey A. Ryan }
\examples{
first(1:100)
last(1:100)

data(LakeHuron)
first(LakeHuron,10)
last(LakeHuron)
}
\keyword{ utilities }
