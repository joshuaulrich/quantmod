`getFin` <-
function(Symbol, src='google', ...) {
  tmp <- tempfile()
  download.file(paste('http://finance.google.com/finance?fstype=ii&q=',Symbol,sep=''),
                quiet=TRUE,destfile=tmp)
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
  IS <- t(sapply(lapply(ISindex,function(x) seq(x,x+6)), function(x) symbol[x]))

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
  BS <- t(sapply(lapply(BSindex,function(x) seq(x,x+6)), function(x) symbol[x]))


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
  CF <- t(sapply(lapply(CFindex,function(x) seq(x,x+6)), function(x) symbol[x]))

  # create list of list of matrix objects [IS=[Q,A],BS=[Q,A],CF=[Q,A]]
  ret <- (list(IS=list(Q=IS[1:49,],A=IS[50:98,]),BS=list(Q=BS[1:40,],A=BS[41:80,]),CF=list(Q=CF[1:19,],A=CF[20:38,])))

  # identify the colnames for IS and CF matricies
  epEnding <- c(0,which(diff(grep('Ending',symbol)) > 5), length(grep('Ending',symbol)))
  indexISCF <- lapply(1:(length(epEnding)-1), function(x) seq(epEnding[x],epEnding[x+1])[-1])
  colnamesISCF <- lapply(indexISCF,function(x) symbol[grep('Ending',symbol)][x])

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

  
  return(structure(ret,class='getFin'))
}

`getFinancials` <- getFin

