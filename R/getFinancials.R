`getFinancials` <-
getFin <-
  function(Symbols, env=parent.frame(), src = "tiingo", auto.assign=TRUE, from = Sys.Date()-720, to=Sys.Date(), ...) {
  importDefaults("getFinancials")
  #TODO: add documentation and tests
  src <- match.arg(src, "tiingo")
  if (src != "tiingo") stop("src = ", sQuote(src), " is not implemented")

  if(is.null(env))
    auto.assign <- FALSE
  if(!auto.assign && length(Symbols)>1)
    stop("must use auto.assign=TRUE for multiple Symbols requests")

  Symbols <- strsplit(Symbols, ";")
  ret.sym <- list()
  for(sym in Symbols) {
    z <- try(structure(do.call(paste("getFinancials", src, sep = "."),
                               args = list(Symbol = sym, from = from, to = to, ...)),
             symbol = sym, class = "financials", src = src, updated = Sys.time()))
    if (auto.assign) {
      if (inherits(z, "financials")) {
        new.sym <- paste(gsub(":", ".", sym), "f", sep = ".")
        assign(new.sym, z, env)
        ret.sym[[length(ret.sym) + 1]] <- new.sym
        }
    } else {
        return(z)
    }
  }
  return(unlist(ret.sym))
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

  message(paste(statements[[period]],statements[[type]],'for',attr(x,'symbol')))
  r <- x[[type]][[period]]
  if (is.null(subset))
    return(r)
  else
    return(t(as.xts(t(r))[subset]))
}

getFinancials.tiingo <- function(Symbol, from, to, api.key, ...) {
  importDefaults("getFinancials.tiingo")
  URL <- sprintf("https://api.tiingo.com/tiingo/fundamentals/%s/statements?startDate=%s&endDate=%s&token=%s", Symbol, from, to, api.key)
  d <- jsonlite::fromJSON(URL)
  if (length(d) == 0) stop("No data returned for Symbol:", Symbol)

  r <- list(periods = data.frame(
                          type = ifelse(d$quarter == 0, "A", "Q"),
                          year = d$year,
                          quarter = ifelse(d$quarter == 0, NA_integer_, d$quarter),
                          ending = as.Date(d$date)
                          )
            )

  #tiiingo section names
  name.map <- list(balanceSheet = "BS",
                   incomeStatement = "IS",
                   cashFlow ="CF")

  #merge into a single df with columns for each period
  for (st in names(name.map)) {
    nm <- name.map[[st]]
    if (!is.null(nm)) {
      # suppress duplicate column names on merge
      mdf <- suppressWarnings(Reduce(function(x,y){merge(x, y, all = TRUE, by = "dataCode")}, d$statementData[[st]]))
      m <- sapply(mdf[,-1], as.numeric) #convert merged dataframe to numeric matrix
      rownames(m) <- mdf[[1]]
      colnames(m) <- as.character(r$periods$ending)
      q.idx <- which(r$periods$type == "Q")
      r[[nm]] <- list(Q = m[,q.idx, drop = FALSE],A = m[,-q.idx, drop = FALSE])
    }
  }
  return(r)
}
