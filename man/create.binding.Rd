\name{create.binding}
\alias{create.binding}
\title{ Create DDB Bindings }
\description{
Internal function used in \code{attachSymbols}
to create active bindings for symbols defined in a \code{DDB} object.
}
\usage{
create.binding(s,
               lsym,
               rsym,
               mem.cache = TRUE,
               file.cache = FALSE,
               cache.dir = tempdir(),
               envir)
}
\arguments{
  \item{s}{ symbol name }
  \item{lsym}{ function to convert to local name (legal R name) }
  \item{rsym}{ function to convert to remote name (source name) }
  \item{mem.cache}{ cache to memory }
  \item{file.cache}{ cache to disk }
  \item{cache.dir}{ directory to cache to/from }
  \item{envir}{ environment name (character) }
}
\details{
Low level function to create bindings during initial
demand-database construction.
}
\value{
Called for its side effect of creating active bindings
to symbols.
}
\references{ Mark, Roger, ? }
\author{ Jeffrey A. Ryan }
\note{ 
This is code used internally by attachSymbols.  User's may modify
this to accomodate different systems.  The upstream functions
needn't maintain consistency with this interface.

Use as a guide or template.
}
\keyword{ misc }
