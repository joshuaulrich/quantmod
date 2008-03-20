\name{getFinancials}
\alias{getFinancials}
\alias{getFin}
\title{ Download Company Financial Statements }
\description{
Download Income Statement, Balance Sheet, and
Cash Flow Statements from Google Finance.
}
\usage{
getFinancials(Symbol, src = "google", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Symbol}{ a valid google symbol }
  \item{src}{ currently unused }
  \item{\dots}{ currently unused }
}
\details{
A utility to download financial statements for
publically traded companies. The data is directly
from Google finance.  All use of the data is
under there Terms of Service, available at
\url{http://www.google.com/accounts/TOS}.

Individual statements can be accessed using
standard \R list extraction tools.
}
\value{
  Six individual matricies organized in a list of class \sQuote{getFin}:
  \item{ IS }{ a list containing (Q)uarterly and (A)nnual Income Statements }
  \item{ BS }{ a list containing (Q)uarterly and (A)nnual Balance Sheets }
  \item{ CF }{ a list containing (Q)uarterly and (A)nnual Cash Flow Statements }
}
\references{ Google Finance BETA: \url{http://finance.google.com/finance}}
\author{ Jeffrey A. Ryan }
\note{
As with all free data, you may be getting exactly what you pay for.
}
\examples{
\dontrun{
JAVA <- getFinancials('JAVA')
AAPL <- getFin('AAPL')

JAVA$IS$Q   # Quarterly Income Statement
AAPL$CF$A   # Annual Cash Flows

str(AAPL)
}
}
\keyword{ utilities }
