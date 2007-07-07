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
           db.fields = c("date", "o", "h", "l", "c", "v", "a"),
           return.type=c('quantmod.OHLC','zoo'),
           field.names = NULL,
           reload.Symbols = TRUE,
           verbose = FALSE,
           warnings = TRUE,
           src = c("MySQL", "yahoo", "google", "economagic"),
           ...)

showSymbols(env=.GlobalEnv)
removeSymbols(Symbols=NULL,env=.GlobalEnv)
saveSymbols(Symbols = NULL,
            file.path=stop("must specify 'file.path'"),
            env = .GlobalEnv)
}
\arguments{
  \item{Symbols}{ a character vector specifying the names of each symbol to be loaded}
  \item{env}{ where to create objects.  Defaults to .GlobalEnv}
  \item{db.fields}{ character vector of names of fields to load  }
  \item{return.type}{ character string specifying class of returned object }
  \item{field.names}{ character vector of colnames to be used in created data.frame }
  \item{reload.Symbols}{ boolean to reload current symbols in specified environment. Defaults
                         to TRUE } 
  \item{verbose}{ boolean to turn on status of retrieval. Defaults to FALSE }
  \item{warnings}{ boolean to turn on warnings. Defaults to TRUE }
  \item{src}{ character vector specifying method of retrieval.  }
  \item{file.path}{ character string of file location }
  \item{\dots}{ additional parameters (currently not used) }
}
\details{
\code{getSymbols} first looks to the local environment for
matching symbols. If found, the next symbol in Symbols 
is attempted.  Upon completion a list of 
loaded symbols is stored in the global environment 
under the name \code{.getSymbols}.

Objects loaded by \code{getSymbols} can be viewed with 
\code{showSymbols} and
removed by a call to \code{removeSymbols}.  Additional
 retrieval methods can be
created simply following the S3-like naming 
convention where getSymbols.NAME 
is used for your function NAME.  See \code{getSymbols.skeleton}.

getSymbols.Default can be used to specify defaults for all arguments.

}
\value{
A call to getSymbols will load into the specified environment one object for each
\code{Symbol} specified.
}
\author{ Jeffrey A. Ryan }
\note{
}
\seealso{ \code{\link{getModelData}} \code{\link{specifyModel}} }
\examples{
\dontrun{
getSymbols('QQQQ')  # loads symbol from default source
getSymbols('F',src='yahoo') # loads Ford market data from yahoo

getSymbols.Defaults(list(QQQQ='yahoo'))
}
}
\keyword{ data }
