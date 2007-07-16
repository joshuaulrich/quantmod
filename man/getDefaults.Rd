\name{getDefaults}
\alias{getDefaults}
\alias{setDefaults}
\alias{unsetDefaults}
\alias{useDefaults}
\alias{unDefaults}
\title{ Use and Manage Global Function Defaults }
\description{
Allows for the use of globally managed default 
values for formal function arguments. Adds
the ability to pre-specify a value for any formal argument
as if it were specified in the function call.
} 
\usage{
getDefaults(name, arg = NULL)
setDefaults(name,...)
unsetDefaults(name, confirm=TRUE)

useDefaults(name)
unDefaults(name)
}
\arguments{
  \item{name}{ name of function, quoted or unquoted }
  \item{arg}{ values to retrieve } 
  \item{\dots}{ new default argument-value pairs } 
  \item{confirm}{ Prompt before unsetting defaults. }
}
\details{
Defaults are set inside the named function with
a call to \code{importDefaults}.  This may be hard
coded into the function by the author, or may be
dynamically added with a call to \code{useDefaults}.
The later copies the original to a hidden function
in the global namespace, and creates a modified version
with the same name as the original in the current 
environment.

\code{useDefaults} replaces all formal functional
arguments with all non-NULL globally specified ones
after first checking that these global defaults have
not been overridden with new values in the function 
call.

The order of lookup is as follows, with the lookup
halted once a specified value is found:

1. Check for arguments specified in the actual call.
2. Check for arguments specified by setDefaults.
3. Use original function defaults. (if any)
}

Setting default values is accompished via 
\code{setDefaults}, with the values being written
to \R's \code{options} list as a named list set
to the function's name appended with a .Default,
all managed automatically.  It is possible to view
and delete defaults with the functions
\code{getDefaults} and \code{unsetDefaults}, 
respectively.

To return a function enabled by \code{useDefaults}
to its original state, call \code{unDefaults}. 
Conceptually this is similar to \code{debug} and 
\code{debug}, though implemented entirely in \R.
\value{
With the exception of \code{getDefaults}
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
function (name, arg = NULL) 
{
    if (is.function(name)) 
        name <- deparse(substitute(name))
    default.name <- paste(name, "Default", sep = ".")
    if (is.null(arg)) {
        return(options(default.name)[[1]])
    }
    else {
        default.list <- list()
        for (each.arg in arg) {
            default.list[[each.arg]] <- options(default.name)[[1]][[each.arg]]
        }
        return(default.list)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
