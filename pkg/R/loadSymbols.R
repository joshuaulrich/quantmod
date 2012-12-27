`download.SymbolNames` <-
function(exchange, cache.file='', quiet=TRUE) {

  exchange <- match.arg(toupper(exchange),
              c("NYSE","NASDAQ","AMEX","OTC","MF","MP"))
  
  if(cache.file == '') {
    cache.file <- getOption('symbolNamesFile')
    if(is.null(cache.file)) cache.file <- tempfile()
  }
  if(!is.null(cache.file)) {
    eval(parse(text=
         paste("options(symbolNamesFile.",exchange," = cache.file)",sep="")))
  }

  FILE <- switch(exchange,
         NASDAQ="nasdaqlisted.txt",
         AMEX  =,
         NYSE  ="otherlisted.txt", 
         OTC   ="otclisted.txt",
         MF    ="mfundslist.txt",
         MP    ="mpidlist.txt")

  download.file(paste("ftp://ftp.nasdaqtrader.com/SymbolDirectory/",FILE,sep=""),
                      destfile=cache.file, quiet=quiet)
  
}

`download.OptionSymbols` <-
function(cache.file=NULL) {
  src <- 'http://www.cboe.com/publish/ScheduledTask/MktData/cboesymboldir2.csv'
  
}

#`getSymbolName` <-
#function(name,exchange) {
  
#  found <- grep(name,db,perl=TRUE)
#}

`getOptionSymbol` <-
function(desc,src='yahoo',optionSymbolsFile) {
 
  # source: http://www.optionsxpress.com/educate/opt_symbols_aspx

  dd <- paste(strsplit(desc,';'))[[1]]
  d.out <- ''

  for(d in dd) {
  d <- strsplit(d,' ')[[1]]

  Symbol <- d[1]  # lookup this
  month  <- match.arg(toupper(d[2]),toupper(month.abb))
  strike <- as.numeric(d[3])
  right  <- match.arg(toupper(d[4]),c("CALL","PUT"))

  exp.codes <- data.frame(toupper(month.abb),c(rep('CALL',12),rep('PUT',12)),LETTERS[1:24])
  exp.code <- LETTERS[intersect(which(exp.codes==month,TRUE)[,1],
                                which(exp.codes==right,TRUE)[,1])]

  strike.codes <- rbind(
                  matrix(rep(c(0,100,200,300,400,500),20),nrow=20,byrow=TRUE)+seq(5,100,5),
                  matrix(rep(seq(0,150,30),6),nrow=6,byrow=TRUE)+seq(7.5,32.5,5))
  strike.code <- LETTERS[which(strike.codes==strike,TRUE)[,1]]

  if(src=='yahoo') src <- ".X"

  d.out <- paste(d.out,paste(Symbol,exp.code,strike.code,src,sep=''),sep=';')
  }
  d.out
}
