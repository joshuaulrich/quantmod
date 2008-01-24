\name{addTA}
\alias{addTA}
\alias{addTRIX}
\alias{addATR}
\alias{addCCI}
\alias{addCMF}
\alias{addDPO}
\alias{addCMO}
\alias{addMomentum}

\title{ Add Technical Indicator to Chart } 
\description{
Functions to add technical indicators to a chart.
}
\details{
The general mechanism to add technical analysis studies or overlays
to a financial chart created with \code{chartSeries}.

All technical functionality is via the \pkg{TTR} package. 

Current technical indicators include:
\itemize{
  \item{addVo:}{add Volume if available}
  \item{addBBands:}{add Bollinger Bands}
  \item{addMACD:}{add Moving Average Convergence Divergence}
  \item{addROC:}{add Rate of Change}
  \item{addRSI}{add Relative Strength Indicator}
  \item{addSMI}{add Stochastic Momentum Index}
  \item{addExpiry}{add options or futures expiration lines}
  \item{addADX}{add Welles Wilder's Directional Movement Indicator}
  \item{addWPR}{add Williams Percentage R}
  \item{addSAR}{add Parabolic SAR}
  \item{addVert}{add vertical line}
  \item{addHoriz}{add horizontal line}
}
See the individual functions for specific implementation and argument details.  Details
of the underlying TTR implementations can be found in \pkg{TTR}.
}
\value{
Called for its side effects, an object to class \code{chobTA}
will be returned invisibly.  If called from the \R command line
the method will draw the appropriate indicator on the current chart.
}
\references{ Josh Ulrich - TTR package }
\author{ Jeffrey A. Ryan }
\keyword{ utilities }
