\name{attachSymbols}
\alias{attachSymbols}
\alias{flushSymbols}
\title{ Attach and Flush DDB }
\description{
Attach a demand database (lazy load) as a new environment.
}
\usage{
attachSymbols(DB = DDB_Yahoo(),
              pos = 2,
              prefix = NULL,
              postfix = NULL,
              mem.cache = TRUE,
              file.cache = FALSE,
              cache.dir = tempdir())
}

flushSymbols(DB = DDB_Yahoo())
\arguments{
  \item{DB}{ A \code{DDB} data base object }
  \item{pos}{ position in search path to attach DB }
  \item{prefix}{ character to prefix all symbols with }
  \item{postfix}{ character to postfix all symbols with }
  \item{mem.cache}{ should objects be cached in memory }
  \item{file.cache}{ should objects be cached in on disk }
  \item{cache.dir}{ directory to use for \code{file.cache=TRUE} }
}
\details{
An experimental function to allow access to remote objects without
requiring explicit calls to a loading function.

\code{attachSymbols} requires a \code{DDB} object
to define where the data is to come from, as well as what symbols are
loaded on-demand.

attachSymbols calls the method referred to
by the \code{DDB} object.  In the default case this is
\code{DDB_Yahoo}.  See this function for specific details
about the Yahoo implementation.

The individual methods make use of \code{getSymbols} to
load the data.  This requires a corresponding
getSymbols method.

Internally, attachSymbols makes use of quantmod's unexported
create.bindings to dynamically create active
bindings to each symbol listed in the DDB object. 

In turn, create.bindings uses one of two \R methods to
create the binding to the names required.  This depends
on the cache method requested.

Immediately after a call to attachSymbols, a new
environment is attached that contains the names of
objects yet to be loaded.  This is similar to the
lazy-load mechanism in \R, though extended to be both
more general and easier to use.

It is important to note that no data is loaded at this stage.
What occurs instead is that these symbols now have active bindings
using either \code{delayedAssign} (mem.cache) or
\code{makeActiveBinding} (file.cache).

During all future requests for the object(s) in question,
the binding will be used to determine how this data is
loaded into R.  \code{mem.cache} will simply load the
data from its corresponding source (as defined by the DDB object)
and leave it in the environment specified in the original call.
The effect of this is to allow lazy-loading of data from
a variety of external sources (Yahoo in the default case).
Once loaded, these are cached in \R's memory.  Nothing further
differentiates these from standard variables.  This also means
that the environment will grow as more symbols are loaded.

If the \code{file.cache} option is set, the data is loaded from
its source the first time the symbol is referenced.  The difference
is that the data is then written to a temporary file and maintained
there.  Data is loaded and subsequently removed upon each
request for the object.  See \code{makeActiveBinding} for details
of how this occurs at the \R level.

A primary advantage of using the file.cache option is the ability
to maintain hundreds or thousands of
objects in your current session without using
memory, or explicitly loading and removing.
The main downside of this approach is the that data must be loaded
from disk each time, with the corresponding (if generally negligible)
overhead of file access.
}
%\value{
%
%}
\references{ Luke's stuff and Mark Brevington and Roger Peng }
\author{ Jeffrey A. Ryan }
\note{ 
This function is new, and all aspects may change in the near future.
}
\seealso{ \code{delayedAssign}, \code{makeActiveBinding} }
\examples{
\dontrun{
attachSymbols()
SBUX
QQQQ
ls()
}
}
\keyword{ misc }
