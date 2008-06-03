\name{addTA}
\alias{dropTA}
\alias{moveTA}
\alias{swapTA}
\alias{addTA}
\alias{newTA}
\alias{addTRIX}
\alias{addATR}
\alias{addCCI}
\alias{addCMF}
\alias{addDPO}
\alias{addCMO}
\alias{addLines}
\alias{addPoints}
\alias{addMomentum}
\alias{addEnvelope}

\title{ Add Technical Indicator to Chart } 
\description{
Functions to add technical indicators to a chart.
}
\details{
The general mechanism to add technical analysis studies or overlays
to a financial chart created with \code{chartSeries}.

Functionality marked with a \sQuote{*} is via the \pkg{TTR} package. 

General TA charting tool functions:
\itemize{
  \item{addTA}{add technical indicator}
  \item{dropTA}{remove technical indicator}
  \item{moveTA}{move a technical indicator}
  \item{swapTA}{swap two technical indicators}
}

Current technical indicators include:
\itemize{
  \item{addADX}{add Welles Wilder's Directional Movement Indicator*}
  \item{addATR}{add Average True Range *}
  \item{addBBands:}{add Bollinger Bands *}
  \item{addCCI}{add Commodity Channel Index *}
  \item{addCMF}{add Chaiken Money Flow *}
  \item{addCMO}{add Chande Momentum Oscillator *}
  \item{addDEMA}{add Double Exponential Moving Average *}
  \item{addDPO}{add Detrended Price Oscillator *}
  \item{addEMA}{add Exponential Moving Average *}
  \item{addEnvelope}{add Moving Average Envelope}
  \item{addEVWMA}{add Exponential Volume Weighted Moving Average *}
  \item{addExpiry}{add options or futures expiration lines}
  \item{addLines}{add line(s)}
  \item{addMACD:}{add Moving Average Convergence Divergence *}
  \item{addMomentum}{add Momentum *}
  \item{addPoints}{add point(s) }
  \item{addROC:}{add Rate of Change *}
  \item{addRSI}{add Relative Strength Indicator *}
  \item{addSAR}{add Parabolic SAR *}
  \item{addSMA}{add Simple Moving Average *}
  \item{addSMI}{add Stochastic Momentum Index *}
  \item{addTRIX}{add Triple Smoothed Exponential Oscillator *}
  \item{addVo:}{add Volume if available}
  \item{addWMA}{add Weighted Moving Average *}
  \item{addWPR}{add Williams Percent R *}
  \item{addZLEMA}{add ZLEMA *}
}
See the individual functions for specific implementation and argument details.  Details
of the underlying TTR implementations can be found in \pkg{TTR}.

The primary changes between the add*** version of an indicator and the
\pkg{TTR} base function is the absense of the data argument in the former.

Notable additions include \code{on}, \code{with.col} and \code{overlay} (deprecated).
}
\value{
Called for its side effects, an object to class \code{chobTA}
will be returned invisibly.  If called from the \R command line
the method will draw the appropriate indicator on the current chart.
}
\note{
Calling any of the above methods from within a function or script
will generally require them to be wrapped in a \code{plot} call
as they rely on the context of the call to initiate the actual
charting addition.
}
\references{ Josh Ulrich - TTR package }
\author{ Jeffrey A. Ryan }
\keyword{ aplot }
