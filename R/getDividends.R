`getDividends` <-
function(Symbol,from='1970-01-01',to=Sys.Date(),env=parent.frame(),src='yahoo',
         auto.assign=FALSE,auto.update=FALSE,verbose=FALSE,...) {

  if(missing(env))
    env <- parent.frame(1)
  if(is.null(env))
    auto.assign <- FALSE
  Symbol.name <- ifelse(!is.character(Symbol),
                        deparse(substitute(Symbol)),
                        as.character(Symbol))

  yahoo.URL <- 'http://ichart.finance.yahoo.com/table.csv?s='
  from.y <- as.numeric(strsplit(as.character(from), "-", )[[1]][1])
  from.m <- as.numeric(strsplit(as.character(from), "-", )[[1]][2])-1
  from.d <- as.numeric(strsplit(as.character(from), "-", )[[1]][3])
  to.y <- as.numeric(strsplit(as.character(to), "-", )[[1]][1])
  to.m <- as.numeric(strsplit(as.character(to), "-", )[[1]][2])-1
  to.d <- as.numeric(strsplit(as.character(to), "-", )[[1]][3])

  tmp <- tempfile()
  on.exit(unlink(tmp))
  download.file(paste(yahoo.URL,Symbol.name, "&a=", 
            from.m, "&b=", sprintf("%.2d", from.d), "&c=", from.y, 
            "&d=", to.m, "&e=", sprintf("%.2d", to.d), "&f=", 
            to.y, "&g=v&ignore=.csv", 
            sep = ""), destfile = tmp, quiet = !verbose)
  fr <- read.csv(tmp)
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
