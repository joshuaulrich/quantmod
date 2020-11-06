\name{Defaults}
\alias{importDefaults}
\alias{getDefaults}
\alias{setDefaults}
\alias{unsetDefaults}
\title{ Manage Default Argument Values for quantmod Functions }
\description{
Use globally specified defaults, if set, in place of formally specified
default argument values. Allows user to specify function defaults
different than formally supplied values, e.g. to change poorly performing
defaults, or satisfy a different preference.
}
\usage{
setDefaults(name, ...)
unsetDefaults(name, confirm = TRUE)
getDefaults(name = NULL, arg = NULL)
importDefaults(calling.fun)
}
\arguments{
  \item{name}{ name of function, quoted or unquoted (see Details) }
  \item{\dots}{ name=value default pairs }
  \item{confirm}{ prompt before unsetting defaults }
  \item{arg}{ values to retrieve } 
  \item{calling.fun}{ name of function to act upon }
}
\details{
  \describe{
    \item{setDefaults}{
      Provides a wrapper to \R \code{options} that allows the user to
      specify any name=value pair for a function's formal arguments.
      Only formal name=value pairs specified will be updated.

      Values do not have to be respecified in subsequent calls to
      \code{setDefaults}, so it is possible to add new defaults for each
      function one at a time, without having to retype all previous values.
      Assigning \code{NULL} to any argument will remove the argument from
      the defaults list.

      \code{name} can be an unquoted, bare symbol only at the top-level. It
      must be a quoted character string if you call \code{setDefaults} inside
      a function. 
    }
    \item{unsetDefaults}{
      Removes name=value pairs from the defaults list.
    }
    \item{getDefaults}{
      Provides access to the stored user defaults. Single arguments need
      not be quoted, multiple arguments must be in a character vector.
    }
    \item{importDefaults}{
      A call to \code{importDefaults} should be placed on the first line
      in the body of the function. It checks the user's environment for
      globally specified default values for the called function. These
      defaults can be specified by the user with a call to
      \code{setDefaults}, and will override any default formal
      parameters, in effect replacing the original defaults with user
      supplied values instead. Any user-specified values in the parent
      function (that is, the function containing \code{importDefaults})
      will override the values set in the global default environment.
    }
  }
}
\value{
  \item{setDefaults}{
    None. Used for it's side effect of setting a list of default
    arguments by function.
  }
  \item{unsetDefaults}{
    None. Used for it's side effect of unsetting default arguments by
    function.
  }
  \item{getDefaults}{
    A named list of defaults and associated values, similar to
    \code{formals}, but only returning values set by \code{setDefaults}
    for the \code{name} function. Calling \code{getDefaults()} (without
    arguments) returns in a character vector of all functions currently
    having defaults set (by \code{setDefaults}).

    This \emph{does not} imply that the returned function names are able
    to accept defaults (via \code{importDefaults}), rather that they have
    been set to store user defaults. All values can also be viewed with a
    call to \code{getOption(name_of_function.Default)}.
  }
  \item{importDefaults}{
    None. Used for its side-effect of loading all non-\code{NULL} user-
    specified default values into the current function's environment,
    effectively changing the default values passed in the parent function
    call. Like formally defined defaults in the function definition,
    default values set by \code{importDefaults} take lower precedence
    than arguments specified by the user in the function call.
  }
}
\author{ Jeffrey A. Ryan }
\note{
  \describe{
    \item{setDefaults}{
      At present it is not possible to specify \code{NULL} as a replacement
      for a non-\code{NULL} default, as the process interprets \code{NULL}
      values as being not set, and will simply use the value specified
      formally in the function. If \code{NULL} is what is desired, it is
      necessary to include this in the function call itself.

      Any arguments included in the actual function call will take
      precedence over \code{setDefaults} values, as well as the standard
      formal function values. This conforms to the current user
      experience in \R. 

      Like \code{options}, default settings are \emph{not} kept across
      sessions. Currently, it is \emph{not} possible to pass values for
      \dots arguments, only formally specified arguments in the original
      function definition.
    }
    \item{unsetDefaults}{
      \code{unsetDefaults} removes the \emph{all} entries from the
      \code{options} lists for the specified function. To remove single
      function default values simply set the name of the argument to
      \code{NULL} in \code{setDefaults}.
    }
    \item{importDefaults}{
      When a function implements \code{importDefaults}, non-named
      arguments \emph{may} be ignored if a global default has been set
      (i.e. not \code{NULL}). If this is the case, simply name the
      arguments in the calling function.

      This \emph{should} also work for functions retrieving formal
      parameter values from \code{options}, as it assigns a value to the
      parameter in a way that looks like it was passed in the function
      call. So any check on \code{options} would presumably disregard
      \code{importDefaults} values if an argument was passed to the
      function.
    }
  }
}
\seealso{ 
  \code{\link{options}}
}
\examples{
my.fun <- function(x=3)
{
  importDefaults('my.fun')
  x^2
}

my.fun()        # returns 9

setDefaults(my.fun, x=10)
my.fun()        # returns 100
my.fun(x=4)     # returns 16

getDefaults(my.fun)
formals(my.fun)
unsetDefaults(my.fun, confirm=FALSE)
getDefaults(my.fun)

my.fun()        # returns 9
}
\keyword{ utilities }
