\name{Defaults}
\alias{importDefaults}
\alias{getDefaults}
\alias{setDefaults}
\alias{unsetDefaults}
\title{ Manage quantmod Default Argument Values }
\description{
Use globally specified defaults, if set, in place of formally specified
default argument values.  Allows user to specify function defaults
different than formally supplied values, e.g. to change poorly performing
defaults, or satisfy a different preference.
}
\usage{
importDefaults(calling.fun)
getDefaults(name = NULL, arg = NULL)
setDefaults(name, ...)
unsetDefaults(name, confirm = TRUE)
}
\arguments{
  \item{calling.fun}{ name of function to act upon }
  \item{name}{ name of function, quoted or unquoted }
  \item{arg}{ values to retrieve } 
  \item{\dots}{ name=value default pairs }
  \item{confirm}{ prompt before unsetting defaults }
}
\details{
Placed immediately after the function declaration, a call to
\code{importDefaults} checks the user's environment for globally
specified default values for the called function.  These defaults can be
specified by the user with a call to \code{setDefaults}, and will override
any default formal parameters, in effect replacing the original defaults
with user supplied values instead.

Any values specified by the user in a in the parent function (that is, the
function containing \code{importDefaults}) will override the values
set in the global default environment.
}
\value{
\code{importDefaults} is used for its side-effect of loading all non-NULL
default values specified by the user into the current function's
environment, effectively changing the default values passed in the parent
function call.  \code{importDefaults} values, like formally defined
defaults in the function definition, take lower precedence than arguments
specified by the user in the function call.

\code{getDefaults} returns a named list of defaults and associated
values, similar to \code{formals}, only returning \code{setDefaults} set
values for the \code{name} function. Single arguments need not be quoted,
multiples must be as a character vector. Calling \code{getDefaults()}
without arguments results in a character vector of all functions
currently having Defaults set (by \code{setDefaults}).

\code{setDefaults} is used for its side-effect of setting a list of
default arguments by function.
}
\author{ Jeffrey A. Ryan }
\note{ 
It is important to note that when a function implements
\code{importDefaults}, non-named arguments \emph{may} be ignored if a
global default has been set (i.e. not NULL).  If this is the case, simply
name the arguments in the calling function.

This \emph{should} also work for functions
retrieving formal parameter values
from \code{options}, as
it assigns a value to the parameter in
a way that looks like it was passed in
the function call. So any check on \code{options}
would presumably disregard \code{importDefaults}
values if an argument
was passed to the function (what \code{useDefaults}
does)

Like \code{options}, default settings are \emph{NOT} kept across sessions.
Currently, it is \emph{NOT} possible to pass values for \dots arguments,
only formally specified arguments in the original function definition.

\code{unsetDefaults} removes the \emph{all} entries from the \code{options}
lists for the specified function. To remove single function default values
simply set the name of the argument to NULL in \code{setDefaults}.
}
\seealso{ 
  \code{\link{options}}
}
\examples{
my.fun <- function(x=3)
{
  importDefaults('my.fun')
  x ^ 2
}

my.fun()        #returns 9

setDefaults(my.fun,x=10)
my.fun()        #returns 100
my.fun(x=4)     #returns 16

getDefaults(my.fun)
formals(my.fun)
unsetDefaults(my.fun,confirm=FALSE)
getDefaults(my.fun)

my.fun()        #returns 9
}
\keyword{ utilities }
