getFin <- function(Symbol, env=.GlobalEnv, src="google", auto.assign=TRUE) {
  Symbol <- strsplit(Symbol,";")[[1]]
  if(length(Symbol)>1)
    return(unlist(lapply(Symbol, getFin, env=env, src=src, auto.assign=auto.assign)))
  Symbol.name <- Symbol
  google.fin <- "http://finance.google.com/finance?fstype=ii&q=" 
  tmp <- tempfile()
  download.file(paste(google.fin,Symbol,sep=""),quiet=TRUE,destfile=tmp)
  Symbol <- readLines(tmp)

  # thead contains the column names
  # tbody contains the data
  thead <- grep('thead', Symbol)
  tbody <- grep('tbody', Symbol)

  # extract the column names
  c1 <- lapply(seq(1,11,2), function(x) Symbol[thead[x]:thead[x+1]])
  c2 <- lapply(c1,gsub,pattern="<.*?>",replacement="")
  cnames <- lapply(c2,function(x) x[-which(x=="")][-1])

  # extract the data.  fnames if financial names (rownames)
  d1 <- lapply(seq(1,11,2), function(x) { Symbol[tbody[x]:tbody[x+1]]})
  d2 <- lapply(d1, gsub, pattern="<.*?>", replacement="", perl=TRUE)
  d3 <- lapply(d2, function(x) x[-which(x=="")])
  fnames <- lapply(d3, function(x) {
                   gsub("&amp;","&",x[grep("[A-Za-z]",x)])} )
  # extract data and fill NAs where needed
  vals   <- lapply(d3, function(x) {
            as.numeric(gsub(",","",
            gsub("^-$",NA,x[-grep("[A-Za-z]",x)]))) })

  # convert to a matrix with correct dim and names
  make_col_names <- function(name) {
    substr(name, nchar(name)-9, nchar(name))
  }
  fin <- lapply(1:6,
         function(x) {
           structure(matrix(vals[[x]],nr=length(fnames[[x]]),byrow=TRUE),
                     .Dimnames=list(fnames[[x]],make_col_names(cnames[[x]])),
                     col_desc=cnames[[x]])})
  fin <- list(IS=list(Q=fin[[1]], A=fin[[2]]),
              BS=list(Q=fin[[3]], A=fin[[4]]),
              CF=list(Q=fin[[5]], A=fin[[6]]))
  if (auto.assign) {
    assign(paste(gsub(":", ".", Symbol.name), "f", sep = "."), 
           structure(fin, symbol = Symbol.name, class = "financials", 
           src = "google", updated = Sys.time()), env)
    return(paste(gsub(":", ".", Symbol.name), "f", sep = "."))
  } else {
    return(structure(fin, symbol = Symbol.name, class = "financials", 
           src = "google", updated = Sys.time()))
  }
}

`.getFin` <-
function(Symbol, env = .GlobalEnv, src='google', auto.assign = TRUE, ...) {
  tmp <- tempfile()
  download.file(paste('http://finance.google.com/finance?fstype=ii&q=',Symbol,sep=''),
                quiet=TRUE,destfile=tmp)
  Symbol.name <- Symbol
  Symbol <- readLines(tmp)

  # strip all html and commas
  symbol <- gsub('(&nbsp;)|(<.*?>)|(,)','',Symbol,perl=TRUE)

  # Income Statement Fields
  IScols <- c('Revenue','Other Revenue','Total Revenue','Cost of Revenue','Gross Profit',
              'Selling','Research','Depreciation/Amortization','Interest Expense','Unusual Expense',
              'Other Operating Expenses','Total Operating Expense','Operating Income',
              'Interest Income','Gain \\(Loss\\) on Sale','Other Net','Income Before Tax','Income After Tax',
              'Minority Interest','Equity In Affiliates','Net Income Before Extra',
              'Accounting Change','Discontinued Operations','Extraordinary Item',
              'Net Income','Preferred Dividends','Income Available to Common Excl',
              'Income Available to Common Incl','Basic Weighted Average Shares',
              'Basic EPS Excluding','Basic EPS Including','Dilution Adjustment',
              'Diluted Weighted Average','Diluted EPS Exluding','Diluted EPS Including',
              'Dividends per Share','Gross Dividends','Net Income after Stock',
              'Basic EPS','Diluted EPS','Depreciation Supplemental','Total Special Items',
              'Normalized Income Before Taxes','Effect of Special Items on Income Taxes',
              'Income Taxes Ex','Normalized Income After Taxes','Normalized Income Avail to Common',
              'Basic Normalized EPS','Diluted Normalized EPS'
)
  # cat('IS: ',length(IScols),'\n')
  ISregx <- paste('(^',IScols,')',sep='',collapse='|')

  ISindex <- grep(ISregx,symbol)
  # create single IS matrix 
  IS <- t(sapply(lapply(ISindex,function(x) seq(x,x+8)), function(x) symbol[x]))

  # Balance Sheet Fields
  BScols <- c('Cash & Equivalents','Short Term Investments','Cash and Short Term',
              'Accounts Receivable','Receivables','Total Receivables','Total Inventory',
              'Prepaid Expenses','Other Current Assets Total','Total Current Assets',
              'Property/Plant','Goodwill Net','Intangibles Net','Long Term Investments',
              'Other Long Term Assets','Total Assets','Accounts Payable','Accrued Expenses',
              'Notes Payable','Current Port','Other Current .iabilities',
              'Total Current Liabilities','Long Term Debt','Capital Lease Obligations',
              'Total Long Term Debt','Total Debt','Deferred Income Tax','Other Liabilities',
              'Total Liabilities','Redeemable Preferred Stock','Preferred Stock',
              'Common Stock','Additional Paid-In','Retained Earnings','Treasury Stock',
              'Other Equity','Total Equity','Total Liabilities & Shareholder',
              'Shares Outs','Total Common Shares Outstanding')

  # cat('BS: ',length(BScols),'\n')
  BSregx <- paste('(^',BScols,')',sep='',collapse='|')

  BSindex <- grep(BSregx,symbol)
  # create single BS matrix
  BS <- t(sapply(lapply(BSindex,function(x) seq(x,x+8)), function(x) symbol[x]))


  # Cash Flow Fields
  CFcols <- c('Net Income/Starting','Depreciation/Depletion','Amortization',
              'Deferred Taxes','Non-Cash Items','Changes in Working Capital','Cash from Operating Activities',
              'Capital Expenditures','Other Investing Cash Flow Items','Cash from Investing Activities',
              'Financing Cash Flow Items','Total Cash Dividends','Issuance .*? Stock','Issuance .*? Debt',
              'Cash from Financing Activities','Foreign Exchange Effects','Net Change in Cash','Cash Interest Paid',
              'Cash Taxes Paid')

  #cat('CF: ',length(CFcols),'\n')
  CFregx <- paste('(^',CFcols,')',sep='',collapse='|')

  CFindex <- grep(CFregx,symbol)
  # create single CF matrix
  CF <- t(sapply(lapply(CFindex,function(x) seq(x,x+8)), function(x) symbol[x]))

  # create list of list of matrix objects [IS=[Q,A],BS=[Q,A],CF=[Q,A]]
  ret <- (list(IS=list(Q=IS[1:49,],A=IS[50:98,]),BS=list(Q=BS[1:40,],A=BS[41:80,]),CF=list(Q=CF[1:19,],A=CF[20:38,])))

  # identify the colnames for IS and CF matricies
  epEnding <- c(0,which(diff(grep('ending',symbol, ignore.case=TRUE)) > 5), length(grep('ending',symbol, ignore.case=TRUE)))
  indexISCF <- lapply(1:(length(epEnding)-1), function(x) seq(epEnding[x],epEnding[x+1])[-1])
  colnamesISCF <- lapply(indexISCF,function(x) symbol[grep('ending',symbol, ignore.case=TRUE)][x])

  # Income Statement Quarters
  # remove empty columns
  ret$IS$Q <- ret$IS$Q[,apply(ret$IS$Q,2,function(x) all(x != ''))]
  # create numeric matrix with appropriate row and col names
  ret$IS$Q <- structure(matrix(as.numeric(gsub('^-$','',as.matrix(ret$IS$Q[,-1]))),nr=49),
                        dimnames=list(ret$IS$Q[,1],gsub('.*(\\d{4}-\\d{2}-\\d{2})','\\1',colnamesISCF[[1]],perl=TRUE)))

  # Income Statement Annual
  # remove empty columns
  ret$IS$A <- ret$IS$A[,apply(ret$IS$A,2,function(x) all(x != ''))]
  # create numeric matrix with appropriate row and col names
  ret$IS$A <- structure(matrix(as.numeric(gsub('^-$','',as.matrix(ret$IS$A[,-1]))),nr=49),
                        dimnames=list(ret$IS$A[,1],gsub('.*(\\d{4}-\\d{2}-\\d{2})','\\1',colnamesISCF[[2]],perl=TRUE)))

  # identify the colnames for BS
  epAsOf <- c(0,which(diff(grep('As of',symbol)) > 5), length(grep('As of',symbol)))
  indexBS <- lapply(1:(length(epAsOf)-1), function(x) seq(epAsOf[x],epAsOf[x+1])[-1])
  colnamesBS <- lapply(indexBS,function(x) symbol[grep('As of',symbol)][x])

  # Balance Sheet Quarters
  # remove empty columns
  ret$BS$Q <- ret$BS$Q[,apply(ret$BS$Q,2,function(x) all(x != ''))]
  # create numeric matrix with appropriate row and col names
  ret$BS$Q <- structure(matrix(as.numeric(gsub('^-$','',as.matrix(ret$BS$Q[,-1]))),nr=40),
                        dimnames=list(ret$BS$Q[,1],gsub('.*(\\d{4}-\\d{2}-\\d{2})','\\1',colnamesBS[[1]],perl=TRUE)))

  # Balance Sheet Annual
  # remove empty columns
  ret$BS$A <- ret$BS$A[,apply(ret$BS$A,2,function(x) all(x != ''))]
  # create numeric matrix with appropriate row and col names
  ret$BS$A <- structure(matrix(as.numeric(gsub('^-$','',as.matrix(ret$BS$A[,-1]))),nr=40),
                        dimnames=list(ret$BS$A[,1],gsub('.*(\\d{4}-\\d{2}-\\d{2})','\\1',colnamesBS[[2]],perl=TRUE)))

  # Cash Flow Quarters
  # remove empty columns
  ret$CF$Q <- ret$CF$Q[,apply(ret$CF$Q,2,function(x) all(x != ''))]
  # create numeric matrix with appropriate row and col names
  ret$CF$Q <- structure(matrix(as.numeric(gsub('^-$','',as.matrix(ret$CF$Q[,-1]))),nr=19),
                        dimnames=list(ret$CF$Q[,1],gsub('.*(\\d{4}-\\d{2}-\\d{2})','\\1',colnamesISCF[[3]],perl=TRUE)))

  # Cash Flow Annual
  # remove empty columns
  ret$CF$A <- ret$CF$A[,apply(ret$CF$A,2,function(x) all(x != ''))]
  # create numeric matrix with appropriate row and col names
  ret$CF$A <- structure(matrix(as.numeric(gsub('^-$','',as.matrix(ret$CF$A[,-1]))),nr=19),
                        dimnames=list(ret$CF$A[,1],gsub('.*(\\d{4}-\\d{2}-\\d{2})','\\1',colnamesISCF[[4]],perl=TRUE)))

  if(auto.assign) {  
    assign(paste(gsub(":",".",Symbol.name),'f',sep='.'),
           structure(ret,symbol=Symbol.name,class='financials',src='google',updated=Sys.time()),env)
    return(paste(gsub(":",".",Symbol.name),'f',sep='.'))
  } else {
    return(structure(ret,symbol=Symbol.name,class='financials',src='google',updated=Sys.time()))
  } 
}

`getFinancials` <- getFin

`print.financials` <- function(x, ...) {
  cat('Financial Statement for',attr(x,'symbol'),'\n')
  cat('Retrieved from',attr(x,'src'),'at',format(attr(x,'updated')),'\n')
  cat('Use "viewFinancials" or "viewFin" to view\n')
}

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

`viewFin` <- viewFinancials
