\name{quantmod-defunct}
\alias{quantmod-defunct}
\title{Defunct Functions in Package \pkg{quantmod}}
% NOTE: need \alias{.} here for each function
\alias{getSymbols.google}
\alias{getFin.google}
\alias{getFinancials.google}
%
\description{
  The functions listed here are no longer part of \pkg{quantmod}.
}
\usage{
  # Defunct in 0.4-13
  getFin(Symbol, env = parent.frame(), src = "google", auto.assign = TRUE, ...)
  getFinancials(Symbol, env = parent.frame(), src = "google", auto.assign = TRUE, ...)
  getSymbols.google(Symbols, env, return.class = "xts", from = "2007-01-01",
    to = Sys.Date(), ...)
}
\details{
  Google Finance stopped providing data in March, 2018. This included historical
  price data, and also financial information. This affected \code{getFinancials},
  \code{getFin}, and \code{getSymbols.google}.
}
\seealso{
  \code{\link{Defunct}}
}
\keyword{internal}
