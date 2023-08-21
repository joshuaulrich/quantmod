\name{quantmod-defunct}
\alias{quantmod-defunct}
\title{Defunct Functions in Package \pkg{quantmod}}
% NOTE: need \alias{.} here for each function
\alias{getSymbols.google}
\alias{getFin.google}
\alias{getFinancials.google}
%
\description{
  The functionality listed here is no longer part of \pkg{quantmod}.
}
\usage{
  # Defunct in 0.4-13
  # getFin(Symbol, src = "google")
  # getFinancials(Symbol, src = "google")
  getSymbols.google(Symbols, env, return.class = "xts", from = "2007-01-01",
    to = Sys.Date(), ...)
}
\details{
  Google Finance stopped providing data in March, 2018. This included historical
  price data, and also financial information. This affected \code{getFinancials},
  \code{getFin}, and \code{getSymbols} when \code{src = "google"}.
}
\seealso{
  \code{\link{Defunct}}
}
\keyword{internal}
