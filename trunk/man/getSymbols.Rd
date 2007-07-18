\name{getSymbols}
\alias{getSymbols}
\alias{showSymbols}
\alias{removeSymbols}
\alias{saveSymbols}
\alias{getSymbols.yahoo}
\alias{getSymbols.MySQL}
\alias{getSymbols.csv}
\alias{getSymbols.RData}
\alias{getSymbols.Oracle}
\alias{getSymbols.Postgresql}
\alias{getSymbols.economagic}
\alias{getSymbols.stlfed}
\alias{getSymbols.Default}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Manage Data from Multiple Sources }
\description{
Functions to manage \code{Symbols} in
specified environment. Used by \code{\link{specifyModel}}
to retrieve symbols specified in first step of modelling 
procedure.  Not a true S3 method, but methods for different
data sources follow an S3-like naming convention. Additional 
methods can be added by simply adhering to the convention.
}
\usage{
getSymbols(Symbols = NULL, 
           env = .GlobalEnv,
           return.class=c('quantmod.OHLC','zoo'),
           reload.Symbols = FALSE,
           verbose = FALSE,
           warnings = TRUE,
           src = c("yahoo", "MySQL", "google", "economagic"),
           symbol.lookup = TRUE,
           ...)

showSymbols(env=.GlobalEnv)
removeSymbols(Symbols=NULL,env=.GlobalEnv)
saveSymbols(Symbols = NULL,
            file.path=stop("must specify 'file.path'"),
            env = .GlobalEnv)

setSymbolLookup(...)
getSymbolLookup(Symbols=NULL)

unsetSymbolLookup(Symbols=NULL)

loadSymbolLookup(filename)
saveSymbolLookup(filename)

}
\arguments{
         \item{Symbols}{ a character vector specifying the names of each symbol to be loaded}
             \item{env}{ where to create objects.  Defaults to .GlobalEnv}
    \item{return.class}{ desired class of returned object.  Can be quantmod.OHLC (default
                       for \dQuote{yahoo} OHLC data),
                       zoo, data.frame, ts, or its }
  \item{reload.Symbols}{ boolean to reload current symbols in specified environment. Defaults
                         to FALSE } 
         \item{verbose}{ boolean to turn on status of retrieval. Defaults to FALSE }
        \item{warnings}{ boolean to turn on warnings. Defaults to TRUE }
             \item{src}{ character string specifying sourcing method.}
   \item{symbol.lookup}{ retrieve symbol's sourcing method from external lookup }
       \item{file.path}{ character string of file location }
           \item{\dots}{ additional parameters (currently not used) }
}
\details{
\code{getSymbols} first looks to the local environment for
matching symbols. If found, the next symbol in Symbols 
is attempted, if not found, it is skipped and a warning will
be displayed.  Upon completion a list of 
loaded symbols is stored in the global environment 
under the name \code{.getSymbols}.

Objects loaded by \code{getSymbols} can be viewed with 
\code{showSymbols} and
removed by a call to \code{removeSymbols}.  Additional
retrieval methods can be
created simply following the S3-like naming 
convention where getSymbols.NAME 
is used for your function NAME.  See \code{getSymbols.skeleton}.

\code{setDefaults(getSymbols)} can be used to
specify defaults for all passed \code{getSymbols} arguments.
\code{setDefaults(getSymbols.MySQL)} for arguments
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
A call to getSymbols will load into the specified environment one object for each
\code{Symbol} specified, with class defined by \code{return.class}.
}
\author{ Jeffrey A. Ryan }
\note{
While it is possible to load symbols as classes other
\code{zoo}, \pkg{quantmod} requires most, if not
all, data to be of class \code{zoo}. The additional
methods are meant mainly to be of use for those
using the functionality elsewhere.
}
\seealso{ \code{\link{getModelData}},\code{\link{specifyModel}}
          \code{\link{setDefaults}},\code{\link{setSymbolLookup}} }
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
