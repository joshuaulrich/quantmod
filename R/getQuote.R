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
function(Symbols,src='yahoo',what='d1t1l1c1ohgv') {
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
  #Qposix <- (paste(sub('(.+)/(.+)/(....)','\\3/\\1/\\2',sq[,3]),
  #                           sq[,4]))
  #class(Qposix) <- c("POSIXt","POSIXct")
  #Q.xts <- xts(sq[,c(2,5,6,7,8,9)],Qposix)
  #dim(Q.xts) <- c(1,6)
  #if(what=='d1t1l1c1ohgv')
  #  colnames(Q.xts) <- paste(Symbols,c("Last","Change","Open","High","Low","Volume"),sep=".")
  #Q.xts
  data.frame(Qposix,sq[,3:NCOL(sq)])
}

