`getFinancials` <-
  getFin <-
  function(Symbols, env=parent.frame(), src = "tiingo", auto.assign=TRUE, from = Sys.Date()-720, to=Sys.Date(), ...) {
  importDefaults("getFinancials")
  #As much desired generic functionality, erro handlign and recovery has been moved into the master function
  #source specific fucntions should just fetch data for a single symbol and be as lightweight as possible
  #TODO: add tests
  src <- match.arg(src, "tiingo")
  if (src != "tiingo") stop("src = ", sQuote(src), " is not implemented")

  if(is.null(env))
    auto.assign <- FALSE
  if(!auto.assign && length(Symbols)>1)
    stop("must use auto.assign=TRUE for multiple Symbols requests")

  Symbols <- strsplit(Symbols, ";")[[1]]
  ret.sym <- list()
  failed.sym <- list()
  for(sym in Symbols) {
    z <- try(structure(do.call(paste("getFinancials", src, sep = "."),
                               args = list(Symbol = sym, from = from, to = to, ...)),
                       symbol = sym, class = "financials", src = src, updated = Sys.time()))
    if (auto.assign) {
      if (inherits(z, "financials")) {
        new.sym <- paste(gsub(":", ".", sym), "f", sep = ".")
        assign(new.sym, z, env)
        ret.sym[[length(ret.sym) + 1]] <- new.sym
      } else {
        failed.sym[[length(failed.sym) + 1]] <- sym
      }
    } else {
      return(z)
    }
  }
  if (length(failed.sym) > 0) {
    warning("Failed getting financials for ", paste(unlist(failed.sym), collapse = ";"))
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
  if (is.null(r) || is.null(subset))
    return(r)
  else
    return(t(as.xts(t(r))[subset]))
}

getFinancials.tiingo <- function(Symbol, from, to, as.reported = FALSE, api.key, ...) {
  #API Documentation: https://api.tiingo.com/documentation/fundamentals
  importDefaults("getFinancials.tiingo")
  URL <- sprintf("https://api.tiingo.com/tiingo/fundamentals/%s/statements?format=csv&startDate=%s&endDate=%s&asReported=%s&token=%s",
                 Symbol, from, to, tolower(as.reported), api.key)
  d <- read.csv(URL)
  if (ncol(d) == 1 && colnames(d) == "None") stop("No data returned for Symbol: ", Symbol)

  #normalized to tiingo mappings
  statement.types <- c(balanceSheet = "BS",
                          incomeStatement = "IS",
                          cashFlow = "CF")

  d <- d[d$statementType %in% names(statement.types) & d$quarter %in% (0:4),]
  d$period <- ifelse(d$quarter == 0, "A", "Q")
  #partition by statement type
  tsubs <- split(d[, c("date", "dataCode","value", "period")], statement.types[d$statementType])
  r <- lapply(tsubs, function(tsub) {
    dsubs <- split(tsub, tsub$period)
    #partition by period (Q or A), pivot and convert to a matrix
    lapply(dsubs, function(dsub) {
      pivot <- reshape(dsub[, c("date", "dataCode", "value")],
                timevar = "date", idvar = "dataCode",  direction = "wide")
      rownames(pivot) <- pivot[[1]]
      pivot <- pivot[, -1, drop = FALSE]
      colnames(pivot) <- gsub("^value\\.", "", colnames(pivot))
      return(as.matrix(pivot))
    })
  })

  r$periods <- unique(d[, c("date", "year", "quarter", "period")])
  return(r)
}
