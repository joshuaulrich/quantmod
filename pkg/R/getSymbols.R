# getSymbols {{{
"getSymbols" <-
function(Symbols=NULL,
         env=.GlobalEnv,
         reload.Symbols=FALSE,
         verbose=FALSE,
         warnings=TRUE,
         src="yahoo",
         symbol.lookup=TRUE,
         auto.assign=TRUE,
         ...)  {
      importDefaults("getSymbols")
      if(!auto.assign && length(Symbols)>1)
        stop("must use auto.assign=TRUE for multiple Symbols requests")
      if(symbol.lookup && missing(src)) {
        # if src is specified - override symbol.lookup
        symbols.src <- getOption('getSymbols.sources')
      } else {
        symbols.src <- src[1]
      }
      #src <- src[1]
      if(is.character(Symbols)) {
      # at least one Symbol has been specified
        Symbols <- unlist(strsplit(Symbols,';'))
        tmp.Symbols <- vector("list")
        for(each.symbol in Symbols) {
          if(each.symbol %in% names(symbols.src)) {
            tmp.src <- symbols.src[[each.symbol]]$src[1]
            #tmp.src <- symbols.src[[each.symbol]]
            if(is.null(tmp.src)) {
              tmp.Symbols[[each.symbol]] <- src[1]
            } else {
              tmp.Symbols[[each.symbol]] <- tmp.src
            }
          } else {
            tmp.Symbols[[each.symbol]] <- src[1]
          }
        }
        Symbols <- tmp.Symbols
      }
      old.Symbols <- NULL
      if(exists('.getSymbols',env,inherits=FALSE)) {
        old.Symbols <- get('.getSymbols',env)
      }
      if(reload.Symbols) {
        Symbols <- c(Symbols, old.Symbols)[unique(names(c(Symbols,old.Symbols)))]
      }
      if(!auto.assign && length(Symbols) > 1)
        stop("must use auto.assign=TRUE when reloading multiple Symbols")
      if(!is.null(Symbols)) {
        #group all Symbols by source
        Symbols <- as.list(unlist(lapply(unique(as.character(Symbols)),
                           FUN=function(x) {
                             Symbols[Symbols==x]
                           }
                           )))
        #Symbols <- as.list(Symbols)
        all.symbols <- list()
        for(symbol.source in unique(as.character(Symbols))) {
          current.symbols <- names(Symbols[Symbols==symbol.source])
          symbols.returned <- do.call(paste('getSymbols.',symbol.source,sep=''),
                                      list(Symbols=current.symbols,env=env,
                                           #return.class=return.class,
                                           #reload.Symbols=reload.Symbols,
                                           verbose=verbose,warnings=warnings,
                                           auto.assign=auto.assign,
                                           ...))
          if(!auto.assign)
            return(symbols.returned)
          for(each.symbol in symbols.returned) all.symbols[[each.symbol]] <- symbol.source 
        }
        req.symbols <- names(all.symbols)
        all.symbols <- c(all.symbols,old.Symbols)[unique(names(c(all.symbols,old.Symbols)))]
        if(auto.assign) {
          assign('.getSymbols',all.symbols,env);
          if(identical(env, .GlobalEnv))
            return(req.symbols)
          return(env)
        }
      } else {
        warning('no Symbols specified')
      }
}
#}}}

loadSymbols <- getSymbols
loadSymbols.formals <- formals(getSymbols)
loadSymbols.formals$env <- substitute(.GlobalEnv)
formals(loadSymbols) <- loadSymbols.formals


# getSymbols.yahoo {{{
"getSymbols.yahoo" <-
function(Symbols,env,return.class='xts',index.class="Date",
         from='2007-01-01',
         to=Sys.Date(),
         adjust=FALSE,
         ...)
{
     importDefaults("getSymbols.yahoo")
     this.env <- environment()
     for(var in names(list(...))) {
        # import all named elements that are NON formals
        assign(var, list(...)[[var]], this.env)
     }

     default.return.class <- return.class
     default.from <- from
     default.to <- to

     if(missing(verbose)) verbose <- FALSE
     if(missing(auto.assign)) auto.assign <- TRUE
     yahoo.URL <- "http://chart.yahoo.com/table.csv?"
     for(i in 1:length(Symbols)) {
       return.class <- getSymbolLookup()[[Symbols[[i]]]]$return.class
       return.class <- ifelse(is.null(return.class),default.return.class,
                              return.class)
       from <- getSymbolLookup()[[Symbols[[i]]]]$from
       from <- ifelse(is.null(from),default.from,from)
       to <- getSymbolLookup()[[Symbols[[i]]]]$to
       to <- ifelse(is.null(to),default.to,to)
   
       from.y <- as.numeric(strsplit(as.character(as.Date(from,origin='1970-01-01')),'-',)[[1]][1])
       from.m <- as.numeric(strsplit(as.character(as.Date(from,origin='1970-01-01')),'-',)[[1]][2])-1
       from.d <- as.numeric(strsplit(as.character(as.Date(from,origin='1970-01-01')),'-',)[[1]][3])
       to.y <- as.numeric(strsplit(as.character(as.Date(to,origin='1970-01-01')),'-',)[[1]][1])
       to.m <- as.numeric(strsplit(as.character(as.Date(to,origin='1970-01-01')),'-',)[[1]][2])-1
       to.d <- as.numeric(strsplit(as.character(as.Date(to,origin='1970-01-01')),'-',)[[1]][3])
       
       Symbols.name <- getSymbolLookup()[[Symbols[[i]]]]$name
       Symbols.name <- ifelse(is.null(Symbols.name),Symbols[[i]],Symbols.name)
       if(verbose) cat("downloading ",Symbols.name,".....\n\n")
       tmp <- tempfile()
       download.file(paste(yahoo.URL,
                           "s=",Symbols.name,
                           "&a=",from.m,
                           "&b=",sprintf('%.2d',from.d),
                           "&c=",from.y,
                           "&d=",to.m,
                           "&e=",sprintf('%.2d',to.d),
                           "&f=",to.y,
                           "&g=d&q=q&y=0",
                           "&z=",Symbols.name,"&x=.csv",
                           sep=''),destfile=tmp,quiet=!verbose)
       fr <- read.csv(tmp)
       unlink(tmp)
       if(verbose) cat("done.\n")
       fr <- xts(as.matrix(fr[,-1]),
                 as.POSIXct(fr[,1], tz=Sys.getenv("TZ")),
                 src='yahoo',updated=Sys.time())
       colnames(fr) <- paste(toupper(gsub('\\^','',Symbols.name)),
                             c('Open','High','Low','Close','Volume','Adjusted'),
                             sep='.')
       if(adjust) {
         # Adjustment algorithm by Joshua Ulrich
         div <- getDividends(Symbols[[i]], from=from, to=to, auto.assign=FALSE)
         spl <- getSplits(Symbols[[i]],    from=from, to=to, auto.assign=FALSE)
         adj <- na.omit(adjRatios(spl, div, Cl(fr)))

         fr[,1] <- fr[,1] * adj[,'Split'] * adj[,'Div']  # Open
         fr[,2] <- fr[,2] * adj[,'Split'] * adj[,'Div']  # High
         fr[,3] <- fr[,3] * adj[,'Split'] * adj[,'Div']  # Low
         fr[,4] <- fr[,4] * adj[,'Split'] * adj[,'Div']  # Close
         fr[,5] <- fr[,5] * ( 1 / adj[,'Div'] )          # Volume
       }

       fr <- convert.time.series(fr=fr,return.class=return.class)
       if(is.xts(fr))
         indexClass(fr) <- index.class

       Symbols[[i]] <-toupper(gsub('\\^','',Symbols[[i]])) 
       if(auto.assign)
         assign(Symbols[[i]],fr,env)
       if(i >= 5 && length(Symbols) > 5) {
         message("pausing 1 second between requests for more than 5 symbols")
         Sys.sleep(1)
       }
     }
     if(auto.assign)
       return(Symbols)
     return(fr)
}
# }}}

# getSymbols.google {{{
"getSymbols.google" <-
function(Symbols,env,return.class='xts',
         from='2007-01-01',
         to=Sys.Date(),
         ...)
{
     fix.google.bug <- TRUE
     importDefaults("getSymbols.google")
     this.env <- environment()
     for(var in names(list(...))) {
        # import all named elements that are NON formals
        assign(var, list(...)[[var]], this.env)
     }
     if(missing(verbose)) verbose <- FALSE
     if(missing(auto.assign)) auto.assign <- TRUE
     google.URL <- "http://finance.google.com/finance/historical?"
     from.y <- as.numeric(strsplit(as.character(from),'-',)[[1]][1])
     from.m <- as.numeric(strsplit(as.character(from),'-',)[[1]][2])
     from.d <- as.numeric(strsplit(as.character(from),'-',)[[1]][3])
     to.y <- as.numeric(strsplit(as.character(to),'-',)[[1]][1])
     to.m <- as.numeric(strsplit(as.character(to),'-',)[[1]][2])
     to.d <- as.numeric(strsplit(as.character(to),'-',)[[1]][3])
     for(i in 1:length(Symbols)) {
       Symbols.name <- getSymbolLookup()[[Symbols[[i]]]]$name
       Symbols.name <- ifelse(is.null(Symbols.name),Symbols[[i]],Symbols.name)
       if(verbose) cat("downloading ",Symbols.name,".....\n\n")
       tmp <- tempfile()
       download.file(paste(google.URL,
                           "q=",Symbols.name,
                           "&startdate=",month.abb[from.m],
                           "+",sprintf('%.2d',from.d),
                           ",+",from.y,
                           "&enddate=",month.abb[to.m],
                           "+",sprintf('%.2d',to.d),
                           ",+",to.y,
                           "&output=csv",
                           sep=''),destfile=tmp,quiet=!verbose)
       fr <- read.csv(tmp)
       unlink(tmp)
       if(verbose) cat("done.\n")
       fr <- fr[nrow(fr):1,] #google data is backwards
       if(fix.google.bug) {
         bad.dates <- c('29-Dec-04','30-Dec-04','31-Dec-04')
         if(as.Date(from,origin='1970-01-01') < as.Date("2003-12-28",origin='1970-01-01') &&
            as.Date(to,origin='1970-01-01') > as.Date("2003-12-30",origin='1970-01-01')) {
           dup.dates <- which(fr[,1] %in% bad.dates)[(1:3)]
           fr <- fr[-dup.dates,]
           warning("google duplicate bug - missing Dec 28,29,30 of 2003")
         }
       }
       fr <- xts(as.matrix(fr[,-1]),
                 as.Date(strptime(fr[,1],"%d-%B-%y"),origin='1970-01-01'),
                 src='google',updated=Sys.time())
       colnames(fr) <- paste(toupper(gsub('\\^','',Symbols.name)),
                             c('Open','High','Low','Close','Volume'),
                             sep='.')
       fr <- convert.time.series(fr=fr,return.class=return.class)
       Symbols[[i]] <-toupper(gsub('\\^','',Symbols[[i]])) 
       if(auto.assign)
         assign(Symbols[[i]],fr,env)
     }
     if(auto.assign)
       return(Symbols)
     return(fr)
}
# }}}

# getSymbols.SQLite {{{
"getSymbols.SQLite" <- function(Symbols,env,return.class='xts',
                               db.fields=c('row_names','Open','High',
                                           'Low','Close','Volume','Adjusted'),
                               field.names = NULL,
                               dbname=NULL,
                               POSIX = TRUE,
                               ...) {
     importDefaults("getSymbols.SQLite")
     this.env <- environment()
     for(var in names(list(...))) {
        # import all named elements that are NON formals
        assign(var, list(...)[[var]], this.env)
     }
     if(missing(verbose)) verbose <- FALSE
     if(missing(auto.assign)) auto.assign <- TRUE
        if('package:DBI' %in% search() || require('DBI',quietly=TRUE)) {
          if('package:RSQLite' %in% search() || require('RSQLite',quietly=TRUE)) {
          } else { warning(paste("package:",dQuote("RSQLite"),"cannot be loaded" )) }
        } else {
          stop(paste("package:",dQuote('DBI'),"cannot be loaded."))
        }
        drv <- dbDriver("SQLite")
        con <- dbConnect(drv,dbname=dbname)
        db.Symbols <- dbListTables(con)
        if(length(Symbols) != sum(Symbols %in% db.Symbols)) {
          missing.db.symbol <- Symbols[!Symbols %in% db.Symbols]
                warning(paste('could not load symbol(s): ',paste(missing.db.symbol,collapse=', ')))
                Symbols <- Symbols[Symbols %in% db.Symbols]
        }
        for(i in 1:length(Symbols)) {
            if(verbose) {
                cat(paste('Loading ',Symbols[[i]],
                    paste(rep('.',10-nchar(Symbols[[i]])),collapse=''),
                    sep=''))
            }
            query <- paste("SELECT ",
                           paste(db.fields,collapse=','),
                           " FROM ",Symbols[[i]],
                           " ORDER BY row_names")
            rs <- dbSendQuery(con, query)
            fr <- fetch(rs, n=-1)
            #fr <- data.frame(fr[,-1],row.names=fr[,1])
            if(POSIX) {
              d <- as.numeric(fr[,1])
              class(d) <- c("POSIXt","POSIXct")
              fr <- xts(fr[,-1],order.by=d)
            } else {
              fr <- xts(fr[,-1],order.by=as.Date(as.numeric(fr[,1]),origin='1970-01-01'))
            }
            colnames(fr) <- paste(Symbols[[i]],
                                  c('Open','High','Low','Close','Volume','Adjusted'),
                                  sep='.')
            fr <- convert.time.series(fr=fr,return.class=return.class)
            if(auto.assign)
              assign(Symbols[[i]],fr,env)
            if(verbose) cat('done\n')
        }
        dbDisconnect(con)
        if(auto.assign)
          return(Symbols)
        return(fr)
}
"getSymbols.sqlite" <- getSymbols.SQLite
# }}}

# getSymbols.MySQL {{{
"getSymbols.MySQL" <- function(Symbols,env,return.class='xts',
                               db.fields=c('date','o','h','l','c','v','a'),
                               field.names = NULL,
                               user=NULL,password=NULL,dbname=NULL,
                               ...) {
     importDefaults("getSymbols.MySQL")
     this.env <- environment()
     for(var in names(list(...))) {
        # import all named elements that are NON formals
        assign(var, list(...)[[var]], this.env)
     }
     if(missing(verbose)) verbose <- FALSE
     if(missing(auto.assign)) auto.assign <- TRUE
        if('package:DBI' %in% search() || require('DBI',quietly=TRUE)) {
          if('package:RMySQL' %in% search() || require('RMySQL',quietly=TRUE)) {
          } else { warning(paste("package:",dQuote("RMySQL"),"cannot be loaded" )) }
        } else {
          stop(paste("package:",dQuote('DBI'),"cannot be loaded."))
        }
        if(is.null(user) || is.null(password) || is.null(dbname)) {
          stop(paste(
              'At least one connection argument (',sQuote('user'),
              sQuote('password'),sQuote('dbname'),
              ") is not set"))
        }
        con <- dbConnect(MySQL(),user=user,password=password,dbname=dbname)
        db.Symbols <- dbListTables(con)
        if(length(Symbols) != sum(Symbols %in% db.Symbols)) {
          missing.db.symbol <- Symbols[!Symbols %in% db.Symbols]
                warning(paste('could not load symbol(s): ',paste(missing.db.symbol,collapse=', ')))
                Symbols <- Symbols[Symbols %in% db.Symbols]
        }
        for(i in 1:length(Symbols)) {
            if(verbose) {
                cat(paste('Loading ',Symbols[[i]],paste(rep('.',10-nchar(Symbols[[i]])),collapse=''),sep=''))
            }
            query <- paste("SELECT ",paste(db.fields,collapse=',')," FROM ",Symbols[[i]]," ORDER BY date")
            rs <- dbSendQuery(con, query)
            fr <- fetch(rs, n=-1)
            #fr <- data.frame(fr[,-1],row.names=fr[,1])
            fr <- xts(as.matrix(fr[,-1]),
                      order.by=as.Date(fr[,1],origin='1970-01-01'),
                      src=dbname,updated=Sys.time())
            colnames(fr) <- paste(Symbols[[i]],
                                  c('Open','High','Low','Close','Volume','Adjusted'),
                                  sep='.')
            fr <- convert.time.series(fr=fr,return.class=return.class)
            if(auto.assign)
              assign(Symbols[[i]],fr,env)
            if(verbose) cat('done\n')
        }
        dbDisconnect(con)
        if(auto.assign)
          return(Symbols)
        return(fr)
}
"getSymbols.mysql" <- getSymbols.MySQL
# }}}

# getSymbols.FRED {{{
`getSymbols.FRED` <- function(Symbols,env,
     return.class="xts", ...) {
     importDefaults("getSymbols.FRED")
     this.env <- environment()
     for(var in names(list(...))) {
        # import all named elements that are NON formals
        assign(var, list(...)[[var]], this.env)
     }
     if(missing(verbose)) verbose <- FALSE
     if(missing(auto.assign)) auto.assign <- TRUE
     FRED.URL <- "http://research.stlouisfed.org/fred2/series"
     for(i in 1:length(Symbols)) {
       if(verbose) cat("downloading ",Symbols[[i]],".....\n\n")
       tmp <- tempfile()
       download.file(paste(FRED.URL,"/",
                            Symbols[[i]],"/",
                            "downloaddata/",
                            Symbols[[i]],".csv",sep=""),
                            destfile=tmp,quiet=!verbose)
       fr <- read.csv(tmp,na.string=".")
       unlink(tmp)
       if(verbose) cat("done.\n")
       fr <- xts(as.matrix(fr[,-1]),
                 as.Date(fr[,1],origin='1970-01-01'),
                 src='FRED',updated=Sys.time())
       dim(fr) <- c(NROW(fr),1)
       colnames(fr) <- as.character(toupper(Symbols[[i]]))
       fr <- convert.time.series(fr=fr,return.class=return.class)
       Symbols[[i]] <-toupper(gsub('\\^','',Symbols[[i]])) 
       if(auto.assign)
         assign(Symbols[[i]],fr,env)
     }
     if(auto.assign)
       return(Symbols)
     return(fr)
} #}}}

"getSymbols.cache" <- function() {}

# getFX {{{
`getFX` <-
function(Currencies,from=Sys.Date()-500,to=Sys.Date(),
         env=.GlobalEnv,
         verbose=FALSE,warning=TRUE,
         auto.assign=TRUE,...) {
  importDefaults("getFX")
  if(!auto.assign && length(Currencies) > 1)
    stop("must use auto.assign=TRUE for multiple currency requests")
  #src <- c('oanda','FRED')[pmatch(src,c('oanda','FRED'))[1]]
  # parse Symbols
  # make symbols conform to service naming conventions
  # e.g. USD/JPY for oanda
  #
  #      DEXUSJP for FRED
  #
  #if(src[1]=="oanda") {
  getSymbols.oanda(Symbols=Currencies,from=from,to=to,
                   env=env,verbose=verbose,warning=warning,
                   auto.assign=auto.assign,...)
  #} else {
  #  getSymbols.FRED(Symbols=Currencies,env=env,verbose=verbose,warning=warning,...)
  #}  
}
#}}}

# getMetals {{{
`getMetals` <-
function(Metals,from=Sys.Date()-500,to=Sys.Date(),
         base.currency="USD",env=.GlobalEnv,
         verbose=FALSE,warning=TRUE,
         auto.assign=TRUE,...) {
  importDefaults("getMetals")
  metals <- c("XAU-GOLD","XPD-PALLADIUM","XPT-PLATINUM","XAG-SILVER")
  metals <- metals[sapply(Metals, function(x) grep(x,metals,ignore.case=TRUE))]
  metals <- as.character(sapply(metals,
                   function(x) {
                     paste(strsplit(x,'-')[[1]][1],base.currency,sep="/")
                   }))
  getSymbols.oanda(Symbols=metals,from=from,to=to,auto.assign=auto.assign,
                   env=env,verbose=verbose,warning=warning,...) 
}
#}}}

# getRates {{{
`getRates` <-
function() {

}
#}}}

# getSymbols.csv {{{
"getSymbols.csv" <-
function(Symbols,env,
         dir="",
         return.class="xts",
         extension="csv",
         ...) {
  importDefaults("getSymbols.csv")
  this.env <- environment()
  for(var in names(list(...))) {
    assign(var,list(...)[[var]], this.env)
  }

  default.return.class <- return.class
  default.dir <- dir
  default.extension <- extension

  if(missing(verbose)) verbose <- FALSE
  if(missing(auto.assign)) auto.assign <- TRUE

  for(i in 1:length(Symbols)) {
    return.class <- getSymbolLookup()[[Symbols[[i]]]]$return.class
    return.class <- ifelse(is.null(return.class),default.return.class,
                           return.class)
    dir <- getSymbolLookup()[[Symbols[[i]]]]$dir
    dir <- ifelse(is.null(dir),default.dir,
                           dir)
    extension <- getSymbolLookup()[[Symbols[[i]]]]$extension
    extension <- ifelse(is.null(extension),default.extension,
                           extension)
    if(verbose) cat("loading ",Symbols[[i]],".....")
    if(dir=="") {
      sym.file <- paste(Symbols[[i]],extension,sep=".")
    } else {
      sym.file <- file.path(dir,paste(Symbols[[i]],extension,sep="."))
    }
    if(!file.exists(sym.file)) {
      cat("\nfile ",paste(Symbols[[i]],"csv",sep='.')," does not exist ",
          "in ",dir,"....skipping\n")
      next
    }
    fr <- read.csv(sym.file)
    if(verbose)  
      cat("done.\n")
    fr <- xts(fr[,-1],as.Date(fr[,1],origin='1970-01-01'),src='csv',updated=Sys.time())
    colnames(fr) <- paste(toupper(gsub('\\^','',Symbols[[i]])),
                          c('Open','High','Low','Close','Volume','Adjusted'),
                             sep='.')
    fr <- convert.time.series(fr=fr,return.class=return.class)
    Symbols[[i]] <-toupper(gsub('\\^','',Symbols[[i]])) 
    if(auto.assign)
      assign(Symbols[[i]],fr,env)
    }
    if(auto.assign)
      return(Symbols)
    return(fr)
}
#}}}

# getSymbols.rds {{{
"getSymbols.rds" <-
function(Symbols,env,
         dir="",
         return.class="xts",
         extension="rds",
         col.names=c('Open','High','Low','Close','Volume','Adjusted'),
         ...) {
  importDefaults("getSymbols.rds")
  this.env <- environment()
  for(var in names(list(...))) {
    assign(var,list(...)[[var]], this.env)
  }

  default.return.class <- return.class
  default.dir <- dir
  default.extension <- extension

  if(missing(verbose)) verbose <- FALSE
  if(missing(auto.assign)) auto.assign <- TRUE

  for(i in 1:length(Symbols)) {
    return.class <- getSymbolLookup()[[Symbols[[i]]]]$return.class
    return.class <- ifelse(is.null(return.class),default.return.class,
                           return.class)
    dir <- getSymbolLookup()[[Symbols[[i]]]]$dir
    dir <- ifelse(is.null(dir),default.dir,
                           dir)
    extension <- getSymbolLookup()[[Symbols[[i]]]]$extension
    extension <- ifelse(is.null(extension),default.extension,
                           extension)
    if(verbose) cat("loading ",Symbols[[i]],".....")
    if(dir=="") {
      sym.file <- paste(Symbols[[i]],extension,sep=".")
    } else {
      sym.file <- file.path(dir,paste(Symbols[[i]],extension,sep="."))
    }
    if(!file.exists(sym.file)) {
      cat("\nfile ",paste(Symbols[[i]],extension,sep='.')," does not exist ",
          "in ",dir,"....skipping\n")
      next
    }
    #fr <- read.csv(sym.file)
    fr <- .readRDS(sym.file)
    if(verbose)  
      cat("done.\n")
    if(!is.xts(fr)) fr <- xts(fr[,-1],as.Date(fr[,1],origin='1970-01-01'),src='rda',updated=Sys.time())
    colnames(fr) <- paste(toupper(gsub('\\^','',Symbols[[i]])),col.names,sep='.')
    fr <- convert.time.series(fr=fr,return.class=return.class)
    Symbols[[i]] <-toupper(gsub('\\^','',Symbols[[i]])) 
    if(auto.assign)
      assign(Symbols[[i]],fr,env)
    }
    if(auto.assign)
      return(Symbols)
    return(fr)
}
#}}}

# getSymbols.rda {{{
"getSymbols.rda" <-
function(Symbols,env,
         dir="",
         return.class="xts",
         extension="rda",
         col.names=c('Open','High','Low','Close','Volume','Adjusted'),
         ...) {
  importDefaults("getSymbols.rda")
  this.env <- environment()
  for(var in names(list(...))) {
    assign(var,list(...)[[var]], this.env)
  }

  default.return.class <- return.class
  default.dir <- dir
  default.extension <- extension

  if(missing(verbose)) verbose <- FALSE
  if(missing(auto.assign)) auto.assign <- TRUE

  for(i in 1:length(Symbols)) {
    return.class <- getSymbolLookup()[[Symbols[[i]]]]$return.class
    return.class <- ifelse(is.null(return.class),default.return.class,
                           return.class)
    dir <- getSymbolLookup()[[Symbols[[i]]]]$dir
    dir <- ifelse(is.null(dir),default.dir,
                           dir)
    extension <- getSymbolLookup()[[Symbols[[i]]]]$extension
    extension <- ifelse(is.null(extension),default.extension,
                           extension)
    if(verbose) cat("loading ",Symbols[[i]],".....")
    if(dir=="") {
      sym.file <- paste(Symbols[[i]],extension,sep=".")
    } else {
      sym.file <- file.path(dir,paste(Symbols[[i]],extension,sep="."))
    }
    if(!file.exists(sym.file)) {
      cat("\nfile ",paste(Symbols[[i]],extension,sep='.')," does not exist ",
          "in ",dir,"....skipping\n")
      next
    }
    #fr <- read.csv(sym.file)
    local.name <- load(sym.file)
    assign('fr',get(local.name))
    if(verbose)  
      cat("done.\n")
    if(!is.xts(fr)) fr <- xts(fr[,-1],as.Date(fr[,1],origin='1970-01-01'),src='rda',updated=Sys.time())
    colnames(fr) <- paste(toupper(gsub('\\^','',Symbols[[i]])),col.names,sep='.')
    fr <- convert.time.series(fr=fr,return.class=return.class)
    Symbols[[i]] <-toupper(gsub('\\^','',Symbols[[i]])) 
    if(auto.assign)
      assign(Symbols[[i]],fr,env)
    }
    if(auto.assign)
      return(Symbols)
    return(fr)
}
#}}}

# getSymbols.RData {{{
`getSymbols.RData` <- getSymbols.rda
# }}}

# getSymbols.IBrokers {{{
"getSymbols.IBrokers" <- function(Symbols, env, return.class='xts',
endDateTime, barSize='1 day', duration='1 M',
useRTH = '1', whatToShow = 'TRADES', time.format = '1', ...)
{
  importDefaults('getSymbols.IBrokers')
  this.env <- environment()
  for(var in names(list(...))) {
    assign(var, list(...)[[var]], this.env)
  }
  if(missing(verbose))
    verbose <- FALSE
  if(missing(auto.assign))
    auto.assign <- TRUE
  if(is.method.available("twsConnect","IBrokers")) {
    tws <- do.call('twsConnect',list(clientId=1001))
    on.exit(do.call('twsDisconnect',list(tws)))
  
    if(missing(endDateTime)) endDateTime <- NULL
  
    for(i in 1:length(Symbols)) {
      Contract <- getSymbolLookup()[[Symbols[i]]]$Contract
      if(inherits(Contract,'twsContract')) {
        fr <- do.call('reqHistoricalData',list(tws, Contract, endDateTime=endDateTime,
                                barSize=barSize, duration=duration,
                                useRTH=useRTH, whatToShow=whatToShow,
                                time.format=time.format, verbose=verbose))
        fr <- convert.time.series(fr=fr, return.class=return.class)
        if(auto.assign)
          assign(Symbols[[i]], fr, env)
        if(i < length(Symbols)) {
          if(verbose) cat('waiting for TWS to accept next request')
          for(pacing in 1:6) {
            if(verbose) cat('.',sep='')
            Sys.sleep(1)
          }
          if(verbose) cat('done\n')
        }
      } else {
        warning(paste('unable to load',Symbols[i],': missing twsContract definition'))
      }
    }
    if(auto.assign)
      return(Symbols)
    return(fr) 
  }
}
# }}}

# getSymbols.RBloomberg {{{
"getSymbols.RBloomberg" <- function() {}
# }}}

# getSymbols.url {{{
"getSymbols.url" <- function() {}
# }}}

# getSymbols.freelunch {{{
"getSymbols.freelunch" <- function() {}
# }}}

# getSymbols.RODBC {{{
"getSymbols.RODBC" <- function() {}
# }}}

# getSymbols.RSQLite {{{
"getSymbols.RSQLite" <- function() {}
# }}}

# getSymbols.ROracle {{{
"getSymbols.ROracle" <- function() {}
# }}}

# getSymbols.oanda {{{
`getSymbols.oanda` <-
function(Symbols,env,return.class='xts',
         from=Sys.Date()-499,
         to=Sys.Date(),
         ...) {
     importDefaults("getSymbols.oanda")
     if( (as.Date(to)-as.Date(from)) > 500 )
       stop("oanda.com limits data to 500 days per request", call.=FALSE)
     this.env <- environment()
     for(var in names(list(...))) {
        # import all named elements that are NON formals
        assign(var, list(...)[[var]], this.env)
     }

     if(!auto.assign && length(Symbols) > 1)
       stop("must use auto.assign=TRUE for multiple Symbols requests")
     default.return.class <- return.class
     default.from <- from
     default.to <- to

     if(missing(verbose)) verbose <- FALSE
     if(missing(auto.assign)) auto.assign <- TRUE

     oanda.URL <- "http://www.oanda.com/convert/fxhistory?lang=en&"
     for(i in 1:length(Symbols)) {
       return.class <- getSymbolLookup()[[Symbols[[i]]]]$return.class
       return.class <- ifelse(is.null(return.class),default.return.class,
                              return.class)
       from <- getSymbolLookup()[[Symbols[[i]]]]$from
       from <- ifelse(is.null(from),default.from,from)
       to <- getSymbolLookup()[[Symbols[[i]]]]$to
       to <- ifelse(is.null(to),default.to,to)
   
       if(as.Date(to,origin='1970-01-01')-as.Date(from,origin='1970-01-01') > 499) stop("oanda limits data to 500 days")
       # automatically break larger requests into equal sized smaller request at some point
       # for now just let it remain

       from.date <- format(as.Date(from,origin='1970-01-01'),"date1=%m%%2F%d%%2F%y&")
       to.date <- format(as.Date(to,origin='1970-01-01'),"date=%m%%2F%d%%2F%y&date_fmt=us&")
       
       Symbols.name <- getSymbolLookup()[[Symbols[[i]]]]$name
       Symbols.name <- ifelse(is.null(Symbols.name),Symbols[[i]],Symbols.name)
       currency.pair <- strsplit(toupper(Symbols.name),"/")[[1]]
       if(length(currency.pair) != 2) {
         warning(paste("incorrectly specified currency pair",Symbols.name))
         next
       }

       if(verbose) cat("downloading ",Symbols.name,".....")
       tmp <- tempfile()
       download.file(paste(oanda.URL,from.date,to.date,"exch=",currency.pair[1],
                       "&expr2=",currency.pair[2],
                       "&margin_fixed=0&SUBMIT=Get+Table&format=CSV&redirected=1",
                       sep=""),destfile=tmp,quiet=!verbose)
       fr <- readLines(tmp)
       unlink(tmp)
       fr <- unlist(strsplit(
                    gsub("<PRE>|</PRE>","",fr[(grep("PRE",fr)[1]):(grep("PRE",fr)[2])]),","))

       if(verbose) cat("done.\n")
       fr <- xts(as.numeric(fr[1:length(fr)%%2!=1]),as.Date(fr[1:length(fr)%%2==1],"%m/%d/%Y",origin='1970-01-01'),
                 src='oanda',updated=Sys.time())
       dim(fr) <- c(length(fr),1)
       colnames(fr) <- gsub("/",".",Symbols[[i]])
       fr <- convert.time.series(fr=fr,return.class=return.class)
       Symbols[[i]] <-toupper(gsub('\\^|/','',Symbols[[i]])) 
       if(auto.assign)
         assign(Symbols[[i]],fr,env)
     }
     if(auto.assign)
       return(Symbols)
     return(fr)
}#}}}

# convert.time.series {{{
`convert.time.series` <- function(fr,return.class) {
       if('quantmod.OHLC' %in% return.class) {
         class(fr) <- c('quantmod.OHLC','zoo')
         return(fr)
       } else
       if('xts' %in% return.class) {
         return(fr)
       }
       if('zoo' %in% return.class) {
         return(as.zoo(fr))
       } else
       if('ts' %in% return.class) {
         fr <- as.ts(fr)
         return(fr)
       } else
       if('data.frame' %in% return.class) {
         fr <- as.data.frame(fr)
         return(fr)
       } else
       if('matrix' %in% return.class) {
         fr <- as.data.frame(fr)
         return(fr)
       } else
       if('its' %in% return.class) {
         if("package:its" %in% search() || suppressMessages(require("its", quietly=TRUE))) {
           fr.dates <- as.POSIXct(as.character(index(fr)))
           fr <- its::its(coredata(fr),fr.dates)
           return(fr)
         } else {
           warning(paste("'its' from package 'its' could not be loaded:",
                         " 'xts' class returned"))
         }
       } else 
       if('timeSeries' %in% return.class) {
         if("package:timeSeries" %in% search() || suppressMessages(require("timeSeries",quietly=TRUE))) {
           fr <- timeSeries(coredata(fr), charvec=as.character(index(fr)))
           return(fr)
         } else {
           warning(paste("'timeSeries' from package 'timeSeries' could not be loaded:",
                   " 'xts' class returned"))
         }
       }
}#}}}

# removeSymbols {{{
"removeSymbols" <- 
function(Symbols=NULL,env=.GlobalEnv) {
    if(exists('.getSymbols',env,inherits=FALSE)) {
    getSymbols <- get('.getSymbols',env,inherits=FALSE)
      if(is.null(Symbols)) {
        #Symbols <- paste(getSymbols)
        Symbols <- names(getSymbols)
      } else {
        #Symbols now has ONLY existing Symbols in it
        #Symbols <- Symbols[Symbols %in% unlist(getSymbols)]
        Symbols <- Symbols[Symbols %in% names(getSymbols)]
      }
      remove(list=as.character(Symbols),envir=env)
      Symbols.remaining <- getSymbols[!names(getSymbols) %in% Symbols]
      if(length(Symbols.remaining) == 0) {
        remove(list=c('.getSymbols'),envir=env)
      } else {
        assign('.getSymbols',Symbols.remaining,env)
      }
    }
}
# }}}

# showSymbols {{{
"showSymbols" <-
function(env=.GlobalEnv) {
    if(exists('.getSymbols',env,inherits=FALSE)) {
        return(unlist(get('.getSymbols',env)))
    } else { return(NULL) }
}
# }}}

# saveSymbols {{{
"saveSymbols"<-
function(Symbols=NULL,file.path=stop("must specify 'file.path'"),env=.GlobalEnv) {
  if(exists('.getSymbols',env,inherits=FALSE)) {
    getSymbols <- get('.getSymbols',env,inherits=FALSE)
      if(is.null(Symbols)) {
        Symbols <- names(getSymbols)
      } else {
        #Symbols now has ONLY existing Symbols in it
        Symbols <- Symbols[Symbols %in% names(getSymbols)]
      }
    for(each.symbol in Symbols) {
      save(list=each.symbol,
           file=paste(file.path,'/',each.symbol,".RData",sep=''),
           env=env)
    }    
  }
}
# }}}

# buildData {{{
"buildData" <- function(formula,na.rm=TRUE,return.class="zoo") {
  if(is.quantmod(formula)) {
    fr <- modelData(formula)
  } else {
    fr <- modelData(specifyModel(formula,na.rm=na.rm))
  }
  fr <- convert.time.series(fr=fr,return.class=return.class)
}
#}}}
