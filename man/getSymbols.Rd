\name{getSymbols}
\alias{getSymbols}
\alias{getSymbols.Bloomberg}
\alias{loadSymbols}
\alias{showSymbols}
\alias{removeSymbols}
\alias{saveSymbols}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Load and Manage Data from Multiple Sources }
\description{
Functions to load and manage \code{Symbols} in
specified environment. Used by \code{\link{specifyModel}}
to retrieve symbols specified in first step of modelling 
procedure.  Not a true S3 method, but methods for different
data sources follow an S3-like naming convention. Additional 
methods can be added by simply adhering to the convention.

Current \code{src} methods available are: yahoo, google,
MySQL, FRED, csv, RData, and oanda.

Data is loaded silently \emph{without} user
assignment by default.
}
\usage{
getSymbols(Symbols = NULL, 
           env = parent.frame(),
           reload.Symbols = FALSE,
           verbose = FALSE,
           warnings = TRUE,
           src = "yahoo",
           symbol.lookup = TRUE,
           auto.assign = TRUE,
           ...)

loadSymbols(Symbols = NULL, 
            env = parent.frame(),
            reload.Symbols = FALSE,
            verbose = FALSE,
            warnings = TRUE,
            src = "yahoo",
            symbol.lookup = TRUE,
            auto.assign = TRUE,
            ...)

showSymbols(env=parent.frame())
removeSymbols(Symbols=NULL,env=parent.frame())
saveSymbols(Symbols = NULL,
            file.path=stop("must specify 'file.path'"),
            env = parent.frame())
}
\arguments{
       \item{Symbols}{ a character vector specifying
                       the names of each symbol to be loaded}
       \item{env}{ where to create objects.  Setting env=NULL is equal to auto.assign=FALSE }
       \item{reload.Symbols}{ boolean to reload current symbols
                              in specified environment. (FALSE)}
       \item{verbose}{ boolean to turn on status of retrieval.
                       (FALSE)}
       \item{warnings}{ boolean to turn on warnings. (TRUE)}
       \item{src}{ character string specifying sourcing method.
                   (yahoo)}
       \item{symbol.lookup}{ retrieve symbol's sourcing method
                             from external lookup (TRUE) }
       \item{auto.assign}{ should results be loaded to \code{env}
                           If \code{FALSE}, return results instead.
                           As of 0.4-0, this is the same as setting env=NULL }
       \item{file.path}{ character string of file location }
       \item{\dots}{ additional parameters }
}
\details{
\code{getSymbols} is a wrapper to load data from
various sources, local or remote. Data is
fetched via one of the available \code{getSymbols} methods
and either saved in the \code{env} specified - the \code{parent.frame()}
by default -- or returned to the caller.  The functionality derives from \code{base::load}
behavior and semantics, i.e. is assigned automatically
to a variable in the specified environment \emph{without} the
user explicitly assigning the returned data to a variable. The assigned variable
name is that of the respective Symbols value.

The previous sentence's point warrants repeating - getSymbols is called
for its side effects, and by default\emph{does not} return the data object
loaded. The data is \sQuote{loaded} silently by the function
into the environment specified.

If automatic assignment is not desired, \code{env} may be set to NULL, or
\code{auto.assign} set to FALSE.

The early versions of getSymbols assigned each object into the user's
.GlobalEnv by name (pre 2009 up to versions less than 0.4-0).  
This behavior is now supported by manually setting
env=.GlobalEnv. As of version 0.4-0, the environment is set
to parent.frame(), which preserved the user workspace when
called within another scope. 

\emph{This behavior is expect to change as of 0.5-0, and all results will instead be explicitly returned to the caller
unless a valid \code{env=} argument is passed or set.}
Many thanks to Kurt Hornik and Achim Zeileis for suggesting this change, and
further thanks to Dirk Eddebuettel for encouraging the move to a more functional
default by 0.5-0.

Using auto.assign=TRUE, the variable chosen is an \R-legal name derived
from the symbol being loaded. It is possible, using
\code{setSymbolLookup} to specify an alternate
name if the default is not desired. See that function for
details. 

If auto.assign=FALSE or env=NULL (as of 0.4-0)
the data will be returned from the call, and will require
the user to assign the results himself. Note that only \emph{one} symbol
at a time may be requested when auto assignment is disabled.

Most, if not all, documentation and functionality related
to model construction and testing in \pkg{quantmod}
assumes that auto.assign remains set to TRUE and \code{env} is
a valid environment object for the calls related to those functions.

Upon completion a list of
loaded symbols is stored in the specified environment
under the name \code{.getSymbols}.

Objects loaded by \code{getSymbols} with auto.assign=TRUE
can be viewed with
\code{showSymbols} and
removed by a call to \code{removeSymbols}.  Additional
data loading \dQuote{methods} can be
created simply by following the S3-like naming
convention where getSymbols.NAME
is used for your function NAME. See \code{getSymbols}
source code.

\code{setDefaults(getSymbols)} can be used to
specify defaults for \code{getSymbols} arguments.
\code{setDefaults(getSymbols.MySQL)} may be used for arguments
specific to \code{getSymbols.MySQL}, etc.

The \dQuote{sourcing} of data is managed internally
through a complex lookup procedure. If \code{symbol.lookup}
is TRUE (the default), a check is made if any symbol
has had its source specified by \code{setSymbolLookup}.

If not set, the process continues by checking to see if
\code{src} has been specified by the user in the
function call. If not, any \code{src} defined with
\code{setDefaults(getSymbols,src=)} is used.

Finally, if none of the other source rules apply
the default \code{getSymbols} \code{src} method is
used (\sQuote{yahoo}).
}
\value{
Called for its side-effect with \code{env} set to a
valid environment and auto.assign=TRUE,
\code{getSymbols} will load into the specified
\code{env} one object for each
\code{Symbol} specified, with class defined
by \code{return.class}. Presently this may be \code{ts},
\code{its}, \code{zoo}, \code{xts}, or \code{timeSeries}.

If env=NULL or auto.assign=FALSE an object of type
\code{return.class} will be returned.
}
\author{ Jeffrey A. Ryan }
\note{
As of version 0.4-0, the default \code{env} value is now
\code{parent.frame()}.  In interactive use this should provide
the same functionality as the previous version. 

While it is possible to load symbols as classes other
than \code{zoo}, \pkg{quantmod} requires most, if not
all, data to be of class \code{zoo} or inherited
from \code{zoo} - e.g. \code{xts}. The additional
methods are meant mainly to be of use for those
using the functionality outside of the \pkg{quantmod} workflow.
}
\seealso{ \code{\link{getModelData}},\code{\link{specifyModel}},
\code{\link{setSymbolLookup}},
\code{\link{getSymbols.csv}},
\code{\link{getSymbols.RData}},
\code{\link{getSymbols.oanda}},
\code{\link{getSymbols.yahoo}},
\code{\link{getSymbols.google}},
\code{\link{getSymbols.FRED}},
\code{\link{getFX}},
\code{\link{getMetals}},
 }
\examples{
\dontrun{
setSymbolLookup(QQQ='yahoo',SPY='google')

# loads QQQQ from yahoo (set with setSymbolLookup)
# loads SPY from MySQL (set with setSymbolLookup)
getSymbols(c('QQQ','SPY'))                

# loads Ford market data from yahoo (the formal default)
getSymbols('F')       

# loads symbol from MySQL database (set with setDefaults)
getSymbols('DIA', verbose=TRUE, src='MySQL')                 

# loads Ford as time series class ts 
getSymbols('F',src='yahoo',return.class='ts') 

# load into a new environment
data.env <- new.env()
getSymbols("YHOO", env=data.env)
ls.str(data.env)

# constrain to local scope
local( {
  getSymbols("AAPL")
  str(AAPL)
  })

exists("AAPL")  # FALSE

# assign into an attached environment
attach(NULL, name="DATA.ENV")
getSymbols("AAPL", env=as.environment("DATA.ENV"))
ls("DATA.ENV")

# directly return to caller
str( getSymbols("AAPL", env=NULL) )

}
}
\keyword{ data }
