`getFinancials` <- getFin <-
  function(Symbols, env=parent.frame(), src="tiingo", auto.assign=TRUE, from=Sys.Date()-720, to=Sys.Date(), ...) {
  #As much desired generic functionality, error handlign and recovery has been moved into the master function
  #source specific fucntions should just fetch data for a single symbol and be as lightweight as possible
  importDefaults("getFinancials")

  call.name <- paste("getFinancials", src, sep = ".")
  Symbols <- strsplit(Symbols, ";")[[1]]
  if (length(find(call.name, mode = "function")) < 1)
    stop("src = ", sQuote(src), " is not implemented")
  if(is.null(env))
    auto.assign <- FALSE
  if(!auto.assign && length(Symbols) > 1)
    stop("must use auto.assign=TRUE for multiple Symbols requests")

  ret.sym <- list()
  for (sym in Symbols) {
    args <- list(Symbol = sym, from = from, to = to, ...)
    fin <- try(structure(do.call(call.name, args = args),
      symbol = sym, class = "financials", src = src, updated = Sys.time()))
    if (auto.assign) {
      if (inherits(fin, "financials")) {
        new.sym <- paste(gsub(":", ".", sym), "f", sep = ".")
        assign(new.sym, fin, env)
        ret.sym[[length(ret.sym) + 1]] <- new.sym
      }
    } else {
      return(fin)
    }
  }
  return(unlist(ret.sym))
}

`viewFin` <- `viewFinancials` <-
  function(x, type=c('BS','IS','CF'), period=c('A','Q'), subset = NULL) {
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

getFinancials.tiingo <- function(Symbol, from, to, as.reported=FALSE, api.key, ...) {
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
      if (NROW(dsub) < 1) return(NULL)
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
