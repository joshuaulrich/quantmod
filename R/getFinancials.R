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

getFinancials.tiingo <- function(Symbol, from, to, api.key, ...) {
  #API Documentation: https://api.tiingo.com/documentation/fundamentals
  importDefaults("getFinancials.tiingo")
  #while the api supports CSV, json is a bit more effecient over the wire
  URL <- sprintf("https://api.tiingo.com/tiingo/fundamentals/%s/statements?startDate=%s&endDate=%s&token=%s",
                 Symbol, from, to, api.key)
  d <- jsonlite::fromJSON(URL)
  if (length(d) == 0) stop("No data returned for Symbol:", Symbol)

  periods = data.frame(
    period = ifelse(d$quarter == 0, "A", "Q"),
    year = d$year,
    quarter = ifelse(d$quarter == 0, NA_integer_, d$quarter),
    ending = as.Date(d$date)
  )

  #normalized to tiingo mappings
  statement.types <- list(BS = "balanceSheet",
                          IS = "incomeStatement",
                          CF = "cashFlow")
  statement.periods <- list(A='Annual',
                            Q='Quarterly')

  r <- lapply(statement.types,  function(st) {
    #warning, some periods may be missing statement types
    statements <- d$statementData[[st]]
    if (length(statements) < 1) {
      warning("No", statement.types[[statement.type]], "data for", Symbol)
      z <- vector(mode='list', length = length(period.names))
    } else {
      missing <- sapply(statements, is.null)
      z <- lapply(names(statement.periods), function(pn) {
        selected <- which(periods$period == pn & !missing)
        if (length(selected) > 0) {
          merged <- Reduce(function(x,y) {
            suppressWarnings(merge(x, y, all = TRUE, by = "dataCode")) #warnings on duplicate column names
          }, statements[selected])
          #convert merged dataframe to numeric matrix, warnings on NA numeric conversion
          m <- suppressWarnings(sapply(merged[,-1], as.double))
          if (is.null(dim(m))) m <- as.matrix(m, nrow = nrow(merged))
          rownames(m) <- merged[[1]] #names in first column
          colnames(m) <- as.character(periods$ending[selected])
        } else m <- NULL
        return(m)
      })
    }
    names(z) <- names(statement.periods)
    return(z)
  })
  names(r) <- names(statement.types)
  r$periods <- periods
  return(r)
}
