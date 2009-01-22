getSymbolsDB <-
function(cache.dir=tempdir(), cacheOK=TRUE, verbose=getOption('verbose')) { 
  tmp <- file.path(cache.dir,'nasdaqlisted.txt')
  NASDAQ <- "http://ftp.nasdaqtrader.com/dynamic/SymDir/nasdaqlisted.txt"
  if(!file.exists(tmp))
    download.file(NASDAQ, dest=tmp,quiet=!verbose)
  NQ <- read.delim(tmp,sep="|",stringsAsFactors=FALSE)
  test_issues <- which(NQ[,4] != "N")
  NQ <- NQ[-test_issues,]
  if(!cacheOK)
    unlink(tmp)

  tmp <- file.path(cache.dir,'otherlisted.txt')
  OTHER  <- "http://ftp.nasdaqtrader.com/dynamic/SymDir/otherlisted.txt"
  if(!file.exists(tmp))
    download.file(OTHER, dest=tmp, quiet=!verbose)
  OT <- read.delim(tmp,sep="|",stringsAsFactors=FALSE)
  
  test_issues <- which(OT[,7] != "N")
  OT <- OT[-test_issues,]
  if(!cacheOK)
    unlink(tmp)

  local.s <- gsub("\\$","_P",c(NQ[,1],OT[,1]))
  local.s
}

create.binding <- function(s, lsym, rsym, 
                           mem.cache=TRUE,
                           file.cache=FALSE, 
                           cache.dir=tempdir(),
                           envir) {
  if((mem.cache + file.cache) != 1) stop("only mem or file caching supported")
    if(missing(rsym) || !is.function(rsym)) {
      rsym <- function(x) x
    }
    if(missing(lsym) || !is.function(lsym)) {
      lsym <- function(x) x
    }
      
    if(file.cache) {
          f <- function(value) {
          if(missing(value)) {
            if(!file.exists(file.path(cache.dir, paste(s,"rda",sep=".")))) {
              assign(lsym(s), getSymbols(rsym(s), auto.assign=FALSE))
              save(list=lsym(s), file=file.path(cache.dir, paste(s,"rda",sep=".")))
              get(lsym(s))
            } else {
              load(file.path(cache.dir, paste(lsym(s),"rda",sep=".")))
              get(lsym(s))
            }
          } else {
            return(message("assignment not possible with 'fetch' databases"))
          }}
          makeActiveBinding(lsym(s), f, as.environment(envir))
    }
    if(mem.cache) {
      envir <- as.environment(envir)
      delayedAssign(lsym(s), { 
                    assign(lsym(s),getSymbols(rsym(s),auto.assign=FALSE), env=envir)
                    get(lsym(s), env=envir) },
                    assign.env=envir)
    }
}

attachSymbols <- function(DB=DDB_Yahoo(),pos=2,prefix=NULL,postfix=NULL, 
                          mem.cache=TRUE, file.cache=FALSE, cache.dir=tempdir())
{
  # this will be the function exported in quantmod  
  if(!inherits(DB, 'DDB'))
    stop("DB must be of class 'DDB'")
  do.call(paste("attachSymbols",DB$src,sep="."), 
          list(DB=DB,pos=pos,prefix=prefix,postfix=postfix,
               mem.cache=mem.cache,
               file.cache=file.cache,
               cache.dir=cache.dir)
         )
}

flushSymbols <- function(DB=DDB_Yahoo())
{
  pos=match(DB$name, search())
  detach(pos=pos)
  attachSymbols(DB=DB,pos=pos)
}

attachSymbols.yahoo <- function(DB,pos,prefix,postfix,mem.cache,file.cache,cache.dir,...) {
  attach(NULL, pos=pos, name=DB$name)
  # convert underscore to hyphen for Yahoo fetch
  rsym <- function(x) gsub("_","-",x,perl=TRUE)
  lsym <- function(x) paste(prefix,as.character(x),postfix,sep="")
  invisible(sapply(DB$db, create.binding, lsym=lsym, 
                       rsym=rsym, mem.cache=mem.cache, file.cache=file.cache,
                       cache.dir=cache.dir, envir=DB$name))
}

DDB_Yahoo <- function(prefix, postfix, 
                      cache.dir=tempdir(), 
                      cacheOK=TRUE, verbose=getOption("verbose"))
{
  db <- getSymbolsDB(cache.dir, cacheOK, verbose)
  db <- db[-grep("_|\\.",db)]
  structure(list(name="DDB:Yahoo", src="yahoo", db=db), class="DDB")
}


# Desired use case
# package: fetch
# description: on demand data loading, caching, and management
#
# makeDB(src="yahoo")  ==>> makeBD_yahoo
# attachDB(YAHOO_US_EQUITY)
# refreshDB() or refreshDB(YAHOO_US_EQUITY)
# 
# These would create and attach a new environment to the search path
# that would contain the symbols and how they should be lazy loaded/cached
#
# Structure of YAHOO_US_EQUITY: (class = "fetchDB")
#
#  DB name
#  Symbol list (character vector)
#  Symbol assignment environment
#  Symbol_prefix
#  Symbol_postfix
#  Additional processing functions
#  fetch mechanism (getSymbols in quantmod)
#  fetch args
#  cache mechanism (file, memory, none)
#  cache directory (applicable only to 'file' cache mechanism)
#  cache rule: maximum object in memory vs. load costs, FIFO, Last Access/Aging
#  refresh rule: function returning T/F (only for 'file' cache data) (time-stamps?)
#
# quantmod::createSymbolsDB(src='quantmod.com/symbols/yahoo_symbols.rda') 
# quantmod::attachSymbols(db=createSymbolsDB())
# quantmod::pkg/symbolDB/YAHOO_US_EQUITY

