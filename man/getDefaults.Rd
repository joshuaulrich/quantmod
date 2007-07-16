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
the ability to pre-specify a value for any formal
argument as if it were specified in the function call.
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
With the exception of \code{getDefaults}, these functions
are used for their side effects.

\code{getDefaults} returns a named list of defaults
and associated values, similar to \code{formals}, only
returning \code{setDefaults} set values.
}
\author{ Jeffrey A. Ryan }
\note{
\code{useDefaults} only works on functions defined
with opening and closing braces.  At present it does
not work with simple on line functions or some S3
methods.

Additionally, it is \emph{NOT} possible to pass
values for \dots arguments, only formally specified
arguments in the original function definition.
}
\seealso{ \code{\link{importDefaults}},
          \code{\link{options}},
          \code{\link{formals}},
          \code{\link{body}},
          \code{\link{as.function}}
}
\examples{
my.fun <- function(x=2,y=1) { x ^ y }
my.fun()            #returns 2
my.fun(x=2,y=10)    #returns 1024

setDefaults(my.fun,x=2,y=3)
useDefaults(my.fun)
my.fun

my.fun()            #returns 8
my.fun(y=10)        #returns 1024
my.fun(x=2,y=10)    #returns 1024

unDefaults(my.fun)
my.fun
my.fun()            #returns 2

getDefaults(my.fun)
unsetDefaults(my.fun,confirm=FALSE)  
getDefaults(my.fun)
}
\keyword{ utilities }
