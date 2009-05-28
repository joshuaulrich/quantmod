`getDividends` <-
function(Symbol,from='1970-01-01',to=Sys.Date(),env=.GlobalEnv,src='yahoo',
         auto.assign=FALSE,auto.update=FALSE,verbose=FALSE,...) {

  Symbol.name <- ifelse(!is.character(Symbol),
                        deparse(substitute(Symbol)),
                        as.character(Symbol))

  yahoo.URL <- 'http://ichart.finance.yahoo.com/table.csv?s='
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
            to.y, "&g=v&ignore=.csv", 
            sep = ""), destfile = tmp, quiet = !verbose)
  fr <- read.csv(tmp)
  unlink(tmp)
  fr <- xts(fr[,2],as.Date(fr[,1]))
  if(is.xts(Symbol)) {
    if(auto.update) {
      xtsAttributes(Symbol) <- list(dividends=fr)
      assign(Symbol.name,Symbol,envir=env)
    }
  } else if(auto.assign) {
      assign(paste(Symbol.name,'div',sep='.'),fr,envir=env)
  } else fr
}
