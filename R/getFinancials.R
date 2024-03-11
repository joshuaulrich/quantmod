`getFinancials` <- getFin <-
  function(Symbols, env=parent.frame(), src="tiingo", auto.assign=TRUE, from=Sys.Date()-720, to=Sys.Date(), ...) {
  #As much generic functionality and error handlign has been moved into the master function
  #source specific fucnimplementations should just fetch data for a single symbol and be as lightweight as possible
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

`viewFinancials` <- `viewFin` <-
  function(x, type=c('BS','IS','CF'), period=c('A','Q'), subset = NULL) {
  importDefaults("viewFinancials")
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

as.data.frame.financials <- function(x) {
  #reshape nested wide matrices to a long data.frame, adding columns for each nesting level
  do.call("rbind", args = lapply(c("BS", "IS", "CF"), function(st) { #statement type loop
    if(is.null(x[[st]])) return(NULL)
    r <- do.call("rbind", lapply(c("A","Q"), function(p) { #period loop
      #convert wide matrix to long dataframe
      p.df <- as.data.frame(x[[st]][[p]])
      if (is.null(p.df) || nrow(p.df) < 1 || ncol(p.df) < 1 ) return(NULL)
      cn <- colnames(p.df)
      p.df <- reshape(p.df, direction = "long", varying = cn, times = cn, v.names = "value", ids = rownames(p.df))
      rownames(p.df) <- NULL
      p.df$time <- as.Date(p.df$time)
      p.df$period <- p
      p.df <- p.df[, c("time", "id", "period", "value")]
      colnames(p.df) <- c("date", "entry", "period", "value")
      return(p.df)
    }))
    r$type <- st
    return(r)
  }))
}

getFinancials.tiingo <- function(Symbol, from, to, as.reported=FALSE, api.key, ...) {
  #API Documentation: https://api.tiingo.com/documentation/fundamentals
  importDefaults("getFinancials.tiingo")
  URL <- sprintf("https://api.tiingo.com/tiingo/fundamentals/%s/statements?format=csv&startDate=%s&endDate=%s&asReported=%s&token=%s",
                 Symbol, from, to, tolower(as.reported), api.key)
  d <- suppressWarnings(read.csv(URL))
  if (ncol(d) == 1 && colnames(d) == "None") stop("No data returned for Symbol: ", Symbol)

  #reshape long dataframe to nested wide matrices, moving column into list elemnt names
  stypes <- c(balanceSheet = "BS", incomeStatement = "IS", cashFlow = "CF")
  d <- d[d$statementType %in% names(stypes) & d$quarter %in% (0:4),]
  d$period <- ifelse(d$quarter == 0, "A", "Q")
  d$statementType <- stypes[d$statementType]

  #partition and format output
  tsubs <- split(d[, c("date", "dataCode","value", "period")], d$statementType)
  r <- lapply(tsubs, function(tsub) {
    dsubs <- split(tsub, tsub$period)
    #partition by period (Q or A), pivot and convert to a matrix
    lapply(dsubs, function(dsub) {
      if (NROW(dsub) < 1) return(NULL)
      pivot <- reshape(dsub[, c("date", "dataCode", "value")],
                       timevar = "date", idvar = "dataCode",  direction = "wide")
      rownames(pivot) <- pivot[[1]] #row names should be unique at this point. an assumption has been violated if not
      pivot <- pivot[, -1, drop = FALSE]
      colnames(pivot) <- gsub("^value\\.", "", colnames(pivot))
      return(as.matrix(pivot))
    })
  })

  r$periods <- unique(d[, c("date", "year", "quarter", "period")])
  return(r)
}
