`getSplits` <-
function(Symbol,from='1970-01-01',to=Sys.Date(),env=.GlobalEnv,src='yahoo',
         auto.assign=TRUE,auto.update=TRUE,verbose=FALSE,...) {

  Symbol.name <- ifelse(!is.character(Symbol),
                        deparse(substitute(Symbol)),
                        as.character(Symbol))

  yahoo.URL <- 'http://ichart.finance.yahoo.com/x?s='
  from.y <- as.numeric(strsplit(as.character(from), "-", )[[1]][1])
  from.m <- as.numeric(strsplit(as.character(from), "-", )[[1]][2])-1
  from.d <- as.numeric(strsplit(as.character(from), "-", )[[1]][3])
  to.y <- as.numeric(strsplit(as.character(to), "-", )[[1]][1])
  to.m <- as.numeric(strsplit(as.character(to), "-", )[[1]][2])
  to.d <- as.numeric(strsplit(as.character(to), "-", )[[1]][3])

  tmp <- tempfile()
  download.file(paste(yahoo.URL,Symbol.name, "&a=", 
            from.m, "&b=", sprintf("%.2d", from.d), "&c=", from.y, 
            "&d=", to.m, "&e=", sprintf("%.2d", to.d), "&f=", 
            to.y, "&g=v&y=0&z=30000", 
            sep = ""), destfile = tmp, quiet = !verbose)
  fr <- read.table(tmp, skip=1, fill=TRUE, as.is=TRUE, sep=",")
  fr <- fr[order(fr[,"V2"]),]
  unlink(tmp)

  spl <- data.frame( split=fr[fr[,"V1"]=="SPLIT","V3"],
    row.names=as.Date(as.character(fr[fr[,"V1"]=="SPLIT","V2"]),"%Y%m%d"),
    stringsAsFactors=FALSE )
  colnames(spl) <- paste(Symbol.name,'spl',sep='.')

  if(NROW(spl)==0) {
    spl <- NA
  } else {
    spl[,1] <- sub(":","/", spl[,1])
    spl[,1] <- 1 / sapply( parse( text=spl[,1] ), eval )
    spl <- as.xts(spl)
  }

  if(is.xts(Symbol)) {
    if(auto.update) {
      xtsAttributes(Symbol) <- list(dividends=spl)
      assign(Symbol.name,Symbol,envir=env)
    }
  } else if(auto.assign) {
      assign(paste(Symbol.name,'spl',sep='.'),spl,envir=env)
  } else spl
}
