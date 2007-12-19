`getQuote` <-
function(Symbols,src='yahoo') {
  if(src != 'yahoo') stop('no additional src methods available yet')
  tmp <- tempfile()
  download.file(paste(
                "http://download.finance.yahoo.com/d/quotes.csv?s=",
                Symbols,
                "&f=sl1d1t1c1ohgv&e=.csv",sep=""),
                dest=tmp,quiet=TRUE)
  s.quote <- read.table(file=tmp,sep=',',stringsAsFactors=FALSE)
  unlink(tmp)
  Qposix <- (paste(sub('(.+)/(.+)/(....)','\\3/\\1/\\2',s.quote[,3]),
                             s.quote[,4]))
  class(Qposix) <- c("POSIXt","POSIXct")
  Q.zoo <- zoo(s.quote[,c(2,5,6,7,8,9)],Qposix)
  dim(Q.zoo) <- c(1,6)
  colnames(Q.zoo) <- paste(Symbols,c("Last","Change","Open","High","Low","Volume"),sep=".")
  Q.zoo
}

