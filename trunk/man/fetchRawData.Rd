\name{fetchRawData}
\alias{fetchRawData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fetchRawData(symbols = NULL, cols = "ohlcva")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{symbols}{ ~~Describe \code{symbols} here~~ }
  \item{cols}{ ~~Describe \code{cols} here~~ }
}
\details{
  ~~ If necessary, more details than the __description__  above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ }

 ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ ~~objects to See Also as \code{\link{~~fun~~}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(symbols=NULL,cols="ohlcva") {
  if(!is.null(symbols)) {
    suppressWarnings(library(DBI))  
    suppressWarnings(library(RMySQL))

    output <- NULL
    from <- " FROM "
    where <- NULL
    symbols <- as.list(symbols)
    query <- paste("SELECT ",symbols[[1]],".date",sep="")
    orderby <- paste(" ORDER BY ",symbols[[1]],".date",sep="")

    ## build col selections
    if(length(cols)==1 || (length(cols) != length(symbols))) {
      cols <- rep(cols[1],length(symbols))     
    }

    for(i in 1:length(symbols)) {
#      if(grep("[^ohlcva]*",cols[i]) > 0) stop("Columns specified must be at least one of characters [ohlcva] only")
      columns = unlist(strsplit(cols[i],NULL))
      query <- paste(query,", ",sep="")
      for(j in 1:length(columns)) {
        if( j<length(columns) ) {
          sp = ", "        		## used to seperate columns in SELECT
        } else {
          sp = ""
        }
        query <- paste(query,symbols[[i]],".",columns[j]," AS ",columns[j],symbols[[i]],sp,sep="")      
#        query <- paste(query,symbols[[i]],".",columns[j],sp,sep="")      
      }
      if( i<length(symbols) ) {
        sp = ", "			## used to seperate tables in FROM        
        nd = " AND "			## used to seperate date test in WHERE
      } else {
        sp = ""
        nd = ""
      }
      from <- paste(from,symbols[[i]],sp,sep="")
      where <- paste(where,symbols[[i]],".date=",symbols[[1]],".date ",nd,sep="")
    }
    
  }
  query <- paste(query,from," WHERE (",where,") ",orderby,sep="")

    con <- suppressWarnings(dbConnect(MySQL()))
    rs <- dbSendQuery(con, query)
    fr <- fetch(rs, n=-1)
    dbDisconnect(con)
    return(fr)
  }
}
\keyword{ ~kwd1 }% at least one, from doc/KEYWORDS
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
