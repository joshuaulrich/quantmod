\name{TA}
\alias{TA}
\alias{dropTA}
\alias{moveTA}
\alias{swapTA}
\alias{addTRIX}
\alias{addATR}
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

General TA charting tool functions:
\itemize{
  \item \code{addTA}: add data as custom indicator
  \item \code{dropTA}: remove technical indicator
  \item \code{moveTA}: move a technical indicator
  \item \code{swapTA}: swap two technical indicators
}

Current technical indicators include:
\itemize{
  \item \code{addADX}: add Welles Wilder's Directional Movement Indicator
  \item \code{addATR}: add Average True Range
  \item \code{addAroon}: add Aroon Indicator
  \item \code{addAroonOsc}: add Aroon Oscillator
  \item \code{addBBands}: add Bollinger Bands
  \item \code{addCCI}: add Commodity Channel Index
  \item \code{addCMF}: add Chaiken Money Flow
  \item \code{addChAD}: add Chaiken Accumulation Distribution Line
  \item \code{addChVol}: add Chaiken Volatility
  \item \code{addCMO}: add Chande Momentum Oscillator
  \item \code{addDEMA}: add Double Exponential Moving Average
  \item \code{addDPO}: add Detrended Price Oscillator
  \item \code{addEMA}: add Exponential Moving Average
  \item \code{addEMV}: add Arm's Ease of Movement
  \item \code{addEnvelope}: add Moving Average Envelope
  \item \code{addEVWMA}: add Exponential Volume Weighted Moving Average
  \item \code{addExpiry}: add options or futures expiration lines
  \item \code{addKST}: add Know Sure Thing
  \item \code{addLines}: add line(s)
  \item \code{addMACD}: add Moving Average Convergence Divergence
  \item \code{addMFI}: add Money Flow Index
  \item \code{addMomentum}: add Momentum
  \item \code{addOBV}: add On-Balance Volume
  \item \code{addPoints}: add point(s)
  \item \code{addROC}: add Rate of Change
  \item \code{addRSI}: add Relative Strength Indicator
  \item \code{addSAR}: add Parabolic SAR
  \item \code{addSMA}: add Simple Moving Average
  \item \code{addSMI}: add Stochastic Momentum Index
  \item \code{addTDI}: add Trend Direction Index
  \item \code{addTRIX}: add Triple Smoothed Exponential Oscillator
  \item \code{addVo}: add Volume (if available)
  \item \code{addVolatility}: add volatility
  \item \code{addWMA}: add Weighted Moving Average
  \item \code{addWPR}: add Williams Percent R
  \item \code{addZigZag}: add Zig Zag
  \item \code{addZLEMA}: add ZLEMA
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
