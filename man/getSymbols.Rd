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
           env = .GlobalEnv,
           reload.Symbols = FALSE,
           verbose = FALSE,
           warnings = TRUE,
           src = "yahoo",
           symbol.lookup = TRUE,
           auto.assign = TRUE,
           ...)

loadSymbols(Symbols = NULL, 
            env = .GlobalEnv,
            reload.Symbols = FALSE,
            verbose = FALSE,
            warnings = TRUE,
            src = "yahoo",
            symbol.lookup = TRUE,
            auto.assign = TRUE,
            ...)

showSymbols(env=.GlobalEnv)
removeSymbols(Symbols=NULL,env=.GlobalEnv)
saveSymbols(Symbols = NULL,
            file.path=stop("must specify 'file.path'"),
            env = .GlobalEnv)
}
\arguments{
       \item{Symbols}{ a character vector specifying
                       the names of each symbol to be loaded}
       \item{env}{ where to create objects. (new.env()) }
       \item{reload.Symbols}{ boolean to reload current symbols
                              in specified environment. (FALSE)}
       \item{verbose}{ boolean to turn on status of retrieval.
                       (FALSE)}
       \item{warnings}{ boolean to turn on warnings. (TRUE)}
       \item{src}{ character string specifying sourcing method.
                   (yahoo)}
       \item{symbol.lookup}{ retrieve symbol's sourcing method
                             from external lookup (TRUE) }
       \item{auto.assign}{ should results be loaded to the
                           environment }
       \item{file.path}{ character string of file location }
       \item{\dots}{ additional parameters }
}
\details{
\code{getSymbols} is a wrapper to load data from
different sources - be them local or remote. Data is
fetched through one of the available \code{getSymbols} methods
and saved in the \code{env} specified - the .GlobalEnv
by default. Data is loaded in much the same way that \code{load}
behaves. By default, it is assigned automatically
to a variable in the specified environment, \emph{without} the
user explicitly assigning the returned data to a variable.

The previous sentence's point warrants repeating - getSymbols is called
for its side effects, and \emph{does not} return the data object
loaded. The data is \sQuote{loaded} silently by the function
into a new environment by default - or the environment specified. This
behavior can be overridden by setting auto.assign to FALSE,
though it is not advised.

Previous versions of getSymbols assigned each object into the user's
.GlobalEnv by name.  This behavior is now supported by setting
env=.GlobalEnv (the current transitional default),
or by using the wrapper \code{loadSymbols}.  Many
thanks to Kurt Hornik and Achim Zeileis for suggesting this change.

By default the variable chosen is an \R-legal name derived
from the symbol being loaded. It is possible, using
\code{setSymbolLookup} to specify an alternate
name if the default is not desired, see that function for
details. 

The result of a call to \code{getSymbols} when auto.assign
is set to TRUE (the default) is a new object
or objects in the user's specified environment - with the
loaded symbol(s) names returned upon exit if that environment
is the .GlobalEnv.

By default, a new environment is returned which contains
all the objects loaded into it.

If auto.assign is set to FALSE
the data will be returned from the call, and will require
the user to assign the results himself.

Most, if not all, documentation and functionality in \pkg{quantmod}
assumes that auto.assign remains set to TRUE.

Upon completion a list of
loaded symbols is stored in the global environment
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
A call to getSymbols will load into the specified
environment one object for each
\code{Symbol} specified, with class defined
by \code{return.class}. Presently this may be \code{ts},
\code{its}, \code{zoo}, \code{xts}, or \code{timeSeries}.

If \code{auto.assign} is set to FALSE an object of type
\code{return.class} will be returned.
}
\author{ Jeffrey A. Ryan }
\note{
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
setSymbolLookup(QQQQ='yahoo',SPY='MySQL')

getSymbols(c('QQQQ','SPY'))                
# loads QQQQ from yahoo (set with setSymbolLookup)
# loads SPY from MySQL (set with setSymbolLookup)

getSymbols('F')       
# loads Ford market data from yahoo (the formal default)

setDefaults(getSymbols,verbose=TRUE,src='MySQL')
getSymbols('DIA')                 
# loads symbol from MySQL database (set with setDefaults)

getSymbols('F',src='yahoo',return.class='ts') 
# loads Ford as time series class ts 

}
}
\keyword{ data }
