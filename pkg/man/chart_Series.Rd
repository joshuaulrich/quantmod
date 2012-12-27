\name{chart_Series}
\alias{chart_Series}
\alias{add_Series}
\alias{add_MACD}
\alias{add_BBands}
\alias{add_RSI}
\alias{add_SMI}
\alias{add_TA}
\alias{add_Vo}
\alias{add_axis}
\alias{zoom_Chart}
\alias{chart_theme}
\alias{chart_pars}
\alias{axTicksByTime2}
\alias{axTicksByValue}
\alias{new.replot}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Experimental Charting Version 2
}
\description{
These are experimental functions for a new version of chartSeries
in quantmod. Interface, behavior, and functionality will change.
}
\usage{
chart_Series(x,
             name = deparse(substitute(x)), 
             type = "candlesticks", 
             subset = "",
             TA = "", 
             pars = chart_pars(), 
             theme = chart_theme(), 
             clev = 0,
             ...)
}
\arguments{
  \item{x}{
time series object
}
  \item{name}{
name for chart
}
  \item{type}{
one of: 
}
  \item{subset}{
an ISO8601 style character string indicating date range
}
  \item{TA}{
a character string of semi-colon seperated TA calls.
}
  \item{pars}{
chart parameters
}
  \item{theme}{
chart theme
}
  \item{clev}{
color level (experimental).  Indicates the degree of brightness 0 is darkest color.
}
  \item{\dots}{
additional parameters
}
}
\details{
These functions, when complete, will revert back to the original chartSeries naming
convention.
}
\value{
Called for graphical side effects.
}
\author{
Jeffrey A. Ryan
}
\note{
Highly experimental (read: alpha) use with caution.
}

\keyword{ dplot }% __ONLY ONE__ keyword per line
