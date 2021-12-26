\name{getFinancials}
\alias{getFinancials}
\alias{viewFinancials}
\alias{getFin}
\alias{viewFin}
\title{ Download and View Financial Statements }
\description{
Download Income Statement, Balance Sheet, and Cash Flow Statements.
}
\usage{
getFinancials(Symbol, env = parent.frame(), src = "tiingo", 
              auto.assign = TRUE,
              ...)

viewFinancials(x, type=c('BS','IS','CF'), period=c('A','Q'),
               subset = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Symbol}{ one or more valid google symbol, as a character vector or
  semi-colon delimited string }
  \item{env}{ where to create the object }
  \item{src}{ currently no sources are implemented }
  \item{auto.assign}{ should results be loaded to the environment }
  \item{\dots}{ currently unused }
  \item{x}{ an object of class financials }
  \item{type}{ type of statement to view }
  \item{period}{ period of statement to view }
  \item{subset}{ \sQuote{xts} style subset string }
}
\details{
A utility to download financial statements for publicly traded companies.

Individual statements can be accessed using
standard \R list extraction tools, or by
using \code{viewFinancials}.

\code{viewFinancials} allows for the use of
date subsetting as available in the \pkg{xts}
package, as well as the specification of
the type of statement to view. BS for balance
sheet, IS for income statement, and CF for cash flow
statement. The period argument is used to identify
which statements to view - (A) for annual and (Q)
for quarterly.
}
\value{
  Six individual matrices and a data.frame organized in a list of class \sQuote{financials}:
  \item{ periods }{ a data.frame containing Fiscal year and quarter corresponding to each statement contained in the results. 
  The \code{ending} column in this data.frame matches the column name in the statement matrix corresponding to period.
   }
  \item{ IS }{ a list containing (Q)uarterly and (A)nnual Income Statements }
  \item{ BS }{ a list containing (Q)uarterly and (A)nnual Balance Sheets }
  \item{ CF }{ a list containing (Q)uarterly and (A)nnual Cash Flow Statements }

  Only the data structure itself is normalized. Individual statement matrixes use field names from the underlying source. 
  Please refer to the source documentation for definitions and explanations.
}
\author{ Jeffrey A. Ryan, Ethan Smith }
\note{
currently only implemented for tiingo, which requres a paid license.
}
\examples{
\dontrun{
getFinancials('BA')  # returns JAVA.f to "env"
getFin('AAPL') # returns AAPL.f to "env"

viewFin(BA.f, "IS", "Q")  # Quarterly Income Statement
viewFin(AAPL.f, "CF", "A")   # Annual Cash Flows

str(AAPL.f)
}
}
\keyword{ utilities }
