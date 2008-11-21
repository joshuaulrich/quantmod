\name{TA}
\alias{TA}
\alias{dropTA}
\alias{moveTA}
\alias{swapTA}
\alias{addTRIX}
\alias{addATR}
\alias{addCCI}
\alias{addCMF}
\alias{addDPO}
\alias{addCMO}
\alias{addCLV}
\alias{addEMV}
\alias{addAroon}
\alias{addAroonOsc}
\alias{addVolatility}
\alias{addOBV}
\alias{addChVol}
\alias{addChAD}
\alias{addKST}
\alias{addMFI}
\alias{addTDI}
\alias{addZigZag}

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
  \item{addTA}{add data as custom indicator }
  \item{dropTA}{remove technical indicator}
  \item{moveTA}{move a technical indicator}
  \item{swapTA}{swap two technical indicators}
}

Current technical indicators include:
\itemize{
  \item{addADX}{add Welles Wilder's Directional Movement Indicator*}
  \item{addATR}{add Average True Range *}
  \item{addAroon}{add Aroon Indicator *}
  \item{addAroonOsc}{add Aroon Oscillator *}
  \item{addBBands:}{add Bollinger Bands *}
  \item{addCCI}{add Commodity Channel Index *}
  \item{addCMF}{add Chaiken Money Flow *}
  \item{addChAD}{add Chaiken Accumulation Distribution Line *}
  \item{addChVol}{add Chaiken Volatility *}
  \item{addCMO}{add Chande Momentum Oscillator *}
  \item{addDEMA}{add Double Exponential Moving Average *}
  \item{addDPO}{add Detrended Price Oscillator *}
  \item{addEMA}{add Exponential Moving Average *}
  \item{addEMV}{add Arm's Ease of Movement *}
  \item{addEnvelope}{add Moving Average Envelope}
  \item{addEVWMA}{add Exponential Volume Weighted Moving Average *}
  \item{addExpiry}{add options or futures expiration lines}
  \item{addKST}{add Know Sure Thing *}
  \item{addLines}{add line(s)}
  \item{addMACD:}{add Moving Average Convergence Divergence *}
  \item{addMFI}{add Money Flow Index *}
  \item{addMomentum}{add Momentum *}
  \item{addOBV}{add On-Balance Volume *}
  \item{addPoints}{add point(s) }
  \item{addROC:}{add Rate of Change *}
  \item{addRSI}{add Relative Strength Indicator *}
  \item{addSAR}{add Parabolic SAR *}
  \item{addSMA}{add Simple Moving Average *}
  \item{addSMI}{add Stochastic Momentum Index *}
  \item{addTDI}{add Trend Direction Index *}
  \item{addTRIX}{add Triple Smoothed Exponential Oscillator *}
  \item{addVo}{add Volume if available}
  \item{addVolatility}{add volatility *}
  \item{addWMA}{add Weighted Moving Average *}
  \item{addWPR}{add Williams Percent R *}
  \item{addZigZag}{add Zig Zag *}
  \item{addZLEMA}{add ZLEMA *}
}
See the individual functions for specific implementation and argument details.  Details
of the underlying TTR implementations can be found in \pkg{TTR}.

The primary changes between the add*** version of an indicator and the
\pkg{TTR} base function is the absense of the data argument in the former.

Notable additions include \code{on}, \code{with.col}.
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
