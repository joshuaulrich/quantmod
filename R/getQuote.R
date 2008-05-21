#`.getQuote` <-
#function(Symbols,src='yahoo',fields) {
#  if(src != 'yahoo') stop('no additional src methods available yet')
#  if(missing(fields))
#    fields <- paste('aa2a5bb2b3b4b6cc1c3c6',
#                    'c8dd1d2ee1e7e8e9',
#                    sep='')
#  tmp <- tempfile()
#  download.file(paste(
#                "http://download.finance.yahoo.com/d/quotes.csv?s=",
#                Symbols,
#                "&f=",fields,"&e=.csv",sep=""),
#                #"&f=sl1d1t1c1ohgv&e=.csv",sep=""),
#                dest=tmp,quiet=TRUE)
#  sq <- as.list(read.table(file=tmp,sep=',',stringsAsFactors=FALSE))
#  unlink(tmp)
#  #Qposix <- (paste(sub('(.+)/(.+)/(....)','\\3/\\1/\\2',s[,3]),
#  #                           sq[,4]))
#  #class(Qposix) <- c("POSIXt","POSIXct")
#  #Q.zoo <- zoo(s.quote[,c(2,5,6,7,8,9)],Qposix)
#  #dim(Q.zoo) <- c(1,6)
#  #colnames(Q.zoo) <- paste(Symbols,c("Last","Change","Open","High","Low","Volume"),sep=".")
#  #structure(Q.zoo,
#  #list(s.quote 
#  #Qlist <- list(timestamp=Qposix,
#  #              Last=s.quote[,2],
#  #              Change=
#  names(sq) <- c('ask','avedailyvol','asksize','bid','askRT','bidRT',
#                 'bookvalue','bidsize','cpc','c','comm','cRT','ahcRT',
#                 'DPS','ltdate','tdate','EPS','Error','EPSest.cy','EPSest.ny',
#                 'EPSest.nq','float','daysLow','daysHigh','fiftytwoweekLow',
#                 'fiftytwoweekHigh','annualizedGain','orderBook','marketCap',
#                 'marketCapRT','EBITDA','chg.fiftytwoweekLow','pchg.fiftytwoweekLow',
#                 'lttimeRT','pchRT','ltSize','chg.fiftytwoweekHigh','pchg.fiftytwoweekHigh',
#                 'ltPriceTime','ltPrice','highLimit','lowLimit','daysRange','daysRangeRT',
#                 'movingAve50day','movingAve200day','chg.200dayMA','pch.200dayMA',
#                 'chg.50dayMA','pchg.50dayMA','name','open','prevClose','pricePaid',
#                 'chg.Percent','priceToSales','priceToBook','PE.RT','PEGRatio','Price.EPS.cy',
#                 'Price.EPS.ny','symbol','shortRatio','lastTradeTime','oneYrTarget',
#                 'Volume','fiftytwoWeekRange','DaysValueChg','DaysValueChg.RT','Exchange',
#                 'DivYield')[1:21]
#  sq
#}

`getQuote` <-
function(Symbols,src='yahoo',what='nd1t1l1c1p2ohgv') {
  if(src != 'yahoo') stop('no additional src methods available yet')
  tmp <- tempfile()
  Symbols <- paste(strsplit(Symbols,';')[[1]],collapse="+")
  download.file(paste(
                "http://finance.yahoo.com/d/quotes.csv?s=",
                Symbols,
                "&f=",what,sep=""),
                dest=tmp,quiet=TRUE)
  sq <- read.csv(file=tmp,sep=',',stringsAsFactors=FALSE,header=FALSE)
  unlink(tmp)
  Qposix <- strptime(paste(sq[,1],sq[,2]),format='%m/%d/%Y %H:%M')
  Symbols <- unlist(strsplit(Symbols,'\\+'))
  df <- data.frame(Qposix,sq[,3:NCOL(sq)])
  rownames(df) <- Symbols
  if(what=='nd1t1l1c1p2ohgv') {
    colnames(df) <- c('Name','Trade Time','Last','Change','% Change','Open','High','Low','Volume')
  }
  df
}

`yahooQuoteFormat` <- function() {
   optnames <- c("Ask", "Average Daily Volume", "Ask Size", "Bid", "Ask (Real-time)", 
              "Bid (Real-time)", "Book Value", "Bid Size", "Change & Percent Change", 
              "Change", "Commission", "Change (Real-time)", "After Hours Change (Real-time)", 
              "Dividend/Share", "Last Trade Date", "Trade Date", "Earnings/Share", 
              "Error Indication (returned for symbol changed / invalid)", "EPS Estimate Current Year", 
              "EPS Estimate Next Year", "EPS Estimate Next Quarter", "Float Shares", 
              "Days Low,h,Days High", "52-week Low", "52-week High", "Holdings Gain Percent", 
              "Annualized Gain", "Holdings Gain", "Holdings Gain Percent (Real-time)", 
              "Holdings Gain (Real-time)", "More Info", "Order Book (Real-time)", 
              "Market Capitalization", "Market Cap (Real-time)", "EBITDA", 
              "Change From 52-week Low", "Percent Change From 52-week Low", 
              "Last Trade (Real-time) With Time", "Change Percent (Real-time)", 
              "Last Trade Size", "Change From 52-week High", "Percent Change From 52-week High", 
              "Last Trade (With Time)", "Last Trade (Price Only)", "High Limit", 
              "Low Limit", "Days Range","Days Range (Real-time)", "50-day Moving Average", 
              "200-day Moving Average", "Change From 200-day Moving Average", 
              "Percent Change From 200-day Moving Average", "Change From 50-day Moving Average", 
              "Percent Change From 50-day Moving Average", "Name", "Notes", 
              "Open", "Previous Close", "Price Paid", "Change in Percent", 
              "Price/Sales", "Price/Book", "Ex-Dividend Date", "P/E Ratio", 
              "Dividend Pay Date", "P/E Ratio (Real-time)", 
              "PEG Ratio", "Price/EPS Estimate Current Year", 
              "Price/EPS Estimate Next Year", "Symbol", "Shares Owned", "Short Ratio", 
              "Last Trade Time", "Trade Links", "Ticker Trend", "1 yr Target Price", 
              "Volume", "Holdings Value", "Holdings Value (Real-time)", "52-week Range", 
              "Days Value Change", "Days Value Change (Real-time)", "Stock Exchange", 
              "Dividend Yield")
  optcodes <- c("a", "a2", "a5", "b", "b2", "b3", "b4", "b6", "c", "c1", "c3", 
              "c6", "c8", "d", "d1", "d2", "e", "e1", "e7", "e8", "e9", "f6", 
              "g", "j", "k", "g1", "g3", "g4", "g5", "g6", "i", "i5", "j1", 
              "j3", "j4", "j5", "j6", "k1", "k2", "k3", "k4", "k5", "l", "l1", 
              "l2", "l3", "m", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "n", "n4", 
              "o", "p", "p1", "p2", "p5", "p6", "q", "r", "r1", "r2", "r5", 
              "r6", "r7", "s", "s1", "s7", "t1", "t6", "t7", "t8", "v", "v1", 
              "v7", "w", "w1", "w4", "x", "y")
  w <- which(optnames %in% select.list(optnames, multiple=TRUE))
  str <- paste(optcodes[w],collapse='')
  nms <- optnames[w]
  return(structure(list(str,nms),class='yahooQuoteFormat'))
}
