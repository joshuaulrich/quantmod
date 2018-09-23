`getFinancials` <-
getFin <- function(Symbol, env=parent.frame(), src="google", auto.assign=TRUE, ...) {
  src <- match.arg(src, "google")
  if (src != "google") {
    stop("src = ", sQuote(src), " is not implemented")
  }
  getFinancials.google(Symbol, env, auto.assign = auto.assign, ...)
}

getFinancials.google <-
function(Symbol, env=parent.frame(), src="google", auto.assign=TRUE, ...) {
  msg <- paste0(sQuote("getFinancials.google"), " is defunct.",
         "\nGoogle Finance stopped providing data in March, 2018.",
         "\nYou could try some of the data sources via Quandl instead.",
         "\nSee help(\"Defunct\") and help(\"quantmod-defunct\")")
  .Defunct("Quandl", "quantmod", msg = msg)
}


`print.financials` <- function(x, ...) {
  cat('Financial Statement for',attr(x,'symbol'),'\n')
  cat('Retrieved from',attr(x,'src'),'at',format(attr(x,'updated')),'\n')
  cat('Use "viewFinancials" or "viewFin" to view\n')
}

`viewFin` <-
`viewFinancials` <- function(x, type=c('BS','IS','CF'), period=c('A','Q'),
                             subset = NULL) {
  if(!inherits(x,'financials')) stop(paste(sQuote('x'),'must be of type',sQuote('financials')))
  type <- match.arg(toupper(type[1]),c('BS','IS','CF'))
  period <- match.arg(toupper(period[1]),c('A','Q')) 


  statements <- list(BS='Balance Sheet',
                     IS='Income Statement',
                     CF='Cash Flow Statement',
                     A='Annual',
                     Q='Quarterly')

  if(is.null(subset)) {
    message(paste(statements[[period]],statements[[type]],'for',attr(x,'symbol')))
    return(x[[type]][[period]])
  } else {
    tmp.table <- as.matrix(as.xts(t(x[[type]][[period]]),dateFormat='Date')[subset])
    dn1 <- rownames(tmp.table)
    dn2 <- colnames(tmp.table)
    tmp.table <- t(tmp.table)[, NROW(tmp.table):1]
    if(is.null(dim(tmp.table))) {
      dim(tmp.table) <- c(NROW(tmp.table),1)
      dimnames(tmp.table) <- list(dn2,dn1)
    }
    message(paste(statements[[period]],statements[[type]],'for',attr(x,'symbol')))
    return(tmp.table)
  }
}
