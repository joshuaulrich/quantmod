# getSymbols {{{
"getSymbols" <-
function(Symbols=NULL,
         env=.GlobalEnv,
         reload.Symbols = FALSE,
         verbose = FALSE,
         warnings = TRUE,
         src = c("yahoo","MySQL","google","FRED"),
         symbol.lookup = TRUE,
         ...)  {

      importDefaults("getSymbols")
      if(symbol.lookup && missing(src)) {
        symbols.src <- getOption('getSymbols.sources')
      } else {
        symbols.src <- NULL
      }
      src = src[1]
      if(is.character(Symbols)) {
      # at least one Symbol has been specified
        tmp.Symbols <- vector("list")
        for(each.symbol in Symbols) {
          if(each.symbol %in% names(symbols.src)) {
            tmp.Symbols[[each.symbol]] <- symbols.src[[each.symbol]]
          } else {
            tmp.Symbols[[each.symbol]] <- src          
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
                                           ...))
          for(each.symbol in symbols.returned) all.symbols[[each.symbol]]=symbol.source 
        }
        all.symbols <- c(all.symbols,old.Symbols)[unique(names(c(all.symbols,old.Symbols)))]
        assign('.getSymbols',all.symbols,env);
        invisible(return(env))
      } else {
        warning('no Symbols specified')
      }
}
#}}}

# getSymbols.yahoo {{{
"getSymbols.yahoo" <-
function(Symbols,env,return.class=c('quantmod.OHLC','zoo'),
         from='2007-01-01',
         to=Sys.Date(),
         ...)
{
     importDefaults("getSymbols.yahoo")
     this.env <- environment()
     for(var in names(list(...))) {
        # import all named elements that are NON formals
        assign(var, list(...)[[var]], this.env)
     }
     if(missing(verbose)) verbose <- FALSE
     yahoo.URL <- "http://chart.yahoo.com/table.csv?"
     from.y <- as.numeric(strsplit(as.character(from),'-',)[[1]][1])
     from.m <- as.numeric(strsplit(as.character(from),'-',)[[1]][2])-1
     from.d <- as.numeric(strsplit(as.character(from),'-',)[[1]][3])
     to.y <- as.numeric(strsplit(as.character(to),'-',)[[1]][1])
     to.m <- as.numeric(strsplit(as.character(to),'-',)[[1]][2])-1
     to.d <- as.numeric(strsplit(as.character(to),'-',)[[1]][3])
     for(i in 1:length(Symbols)) {
       if(verbose) cat("downloading ",Symbols[[i]],".....")
       fr <- read.csv(paste(yahoo.URL,
                           "s=",Symbols[[i]],
                           "&a=",from.m,
                           "&b=",sprintf('%.2d',from.d),
                           "&c=",from.y,
                           "&d=",to.m,
                           "&e=",sprintf('%.2d',to.d),
                           "&f=",to.y,
                           "&g=d&q=q&y=0",
                           "&z=",Symbols[[i]],"&x=.csv",
                           sep=''))
       if(verbose) cat("done.\n")
       fr <- zoo(fr[,-1],as.Date(fr[,1]))
       colnames(fr) <- paste(toupper(gsub('\\^','',Symbols[[i]])),
                             c('Open','High','Low','Close','Volume','Adjusted'),
                             sep='.')
       if('quantmod.OHLC' %in% return.class) {
         class(fr) <- c('quantmod.OHLC','zoo')
       } else
       if('zoo' %in% return.class) {
         fr
       }
       if('ts' %in% return.class) {
         fr <- as.ts(fr)
       } else
       if('data.frame' %in% return.class) {
         fr <- as.data.frame(fr)
       } else
       if('its' %in% return.class) {
         if("package:its" %in% search() || require("its", quietly=TRUE)) {
           index(fr) <- as.POSIXct(index(fr))
           fr <- its::as.its(fr)
         } else {
           warning(paste("'its' from package 'its' could not be loaded:",
                         " 'zoo' class returned"))
         }
       } else 
       if('timeSeries' %in% return.class) {
         if("package:fCalendar" %in% search() || require("fCalendar",quietly=TRUE)) {
           fr <- as.timeSeries(fr)
         } else {
           warning(paste("'timeSeries' from package 'fCalendar' could not be loaded:",
                   " 'zoo' class returned"))
         }
       }
       Symbols[[i]] <-toupper(gsub('\\^','',Symbols[[i]])) 
       assign(Symbols[[i]],fr,env)
     }
     return(Symbols)
}
# }}}

# getSymbols.google {{{
"getSymbols.google" <-
function(Symbols,env,return.class=c('quantmod.OHLC','zoo'),
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
     google.URL <- "http://finance.google.com/finance/historical?"
     from.y <- as.numeric(strsplit(as.character(from),'-',)[[1]][1])
     from.m <- as.numeric(strsplit(as.character(from),'-',)[[1]][2])
     from.d <- as.numeric(strsplit(as.character(from),'-',)[[1]][3])
     to.y <- as.numeric(strsplit(as.character(to),'-',)[[1]][1])
     to.m <- as.numeric(strsplit(as.character(to),'-',)[[1]][2])
     to.d <- as.numeric(strsplit(as.character(to),'-',)[[1]][3])
     for(i in 1:length(Symbols)) {
       if(verbose) cat("downloading ",Symbols[[i]],".....")
       fr <- read.csv(paste(google.URL,
                           "q=",Symbols[[i]],
                           "&startdate=",month.abb[from.m],
                           "+",sprintf('%.2d',from.d),
                           ",+",from.y,
                           "&enddate=",month.abb[to.m],
                           "+",sprintf('%.2d',to.d),
                           ",+",to.y,
                           "&output=csv",
                           sep=''))
       if(verbose) cat("done.\n")
       fr <- fr[nrow(fr):1,] #google data is backwards
       if(fix.google.bug) {
         bad.dates <- c('29-Dec-04','30-Dec-04','31-Dec-04')
         dup.dates <- which(fr[,1] %in% bad.dates)[(1:3)]
         fr <- fr[-dup.dates,]
         if(length(dup.dates) > 0) 
           warning("google duplicate bug - missing Dec 28,29,30 of 2003")
       }
       fr <- zoo(fr[,-1],as.Date(strptime(fr[,1],"%d-%B-%y")))
       colnames(fr) <- paste(toupper(gsub('\\^','',Symbols[[i]])),
                             c('Open','High','Low','Close','Volume'),
                             sep='.')
       if('quantmod.OHLC' %in% return.class) {
         class(fr) <- c('quantmod.OHLC','zoo')
       } else
       if('zoo' %in% return.class) {
         fr
       }
       if('ts' %in% return.class) {
         fr <- as.ts(fr)
       } else
       if('data.frame' %in% return.class) {
         fr <- as.data.frame(fr)
       } else
       if('its' %in% return.class) {
         if("package:its" %in% search() || require("its", quietly=TRUE)) {
           index(fr) <- as.POSIXct(index(fr))
           fr <- its::as.its(fr)
         } else {
           warning(paste("'its' from package 'its' could not be loaded:",
                         " 'zoo' class returned"))
         }
       } else 
       if('timeSeries' %in% return.class) {
         if("package:fCalendar" %in% search() || require("fCalendar",quietly=TRUE)) {
           fr <- as.timeSeries(fr)
         } else {
           warning(paste("'timeSeries' from package 'fCalendar' could not be loaded:",
                   " 'zoo' class returned"))
         }
       }
       Symbols[[i]] <-toupper(gsub('\\^','',Symbols[[i]])) 
       assign(Symbols[[i]],fr,env)
     }
     return(Symbols)
}
# }}}

# getSymbols.MySQL {{{
"getSymbols.MySQL" <- function(Symbols,env,return.class=c('quantmod.OHLC','zoo'),
                               db.fields=c('date','o','h','l','c','v','a'),
                               field.names = NULL,
                               user=NULL,password=NULL,dbname=NULL,...) {
     importDefaults("getSymbols.MySQL")
     this.env <- environment()
     for(var in names(list(...))) {
        # import all named elements that are NON formals
        assign(var, list(...)[[var]], this.env)
     }
     if(missing(verbose)) verbose <- FALSE
        if('package:DBI' %in% search() || require('DBI',quietly=TRUE)) {
          if('package:RMySQL' %in% search() || require('RMySQL',quietly=TRUE)) {
          } else { warning(paste("package:",dQuote("RMySQL"),"cannot be loaded" )) }
        } else {
          stop(paste("package:",dQuote('DBI'),"cannot be loaded."))
        }
        if(is.null(user) || is.null(password) || is.null(dbname)) {
          stop(paste(
              'At least one connection arguement (',sQuote('user'),
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
            fr <- zoo(fr[,-1],order.by=as.Date(fr[,1]))
            colnames(fr) <- paste(Symbols[[i]],
                                  c('Open','High','Low','Close','Volume','Adjusted'),
                                  sep='.')
            if('quantmod.OHLC' %in% return.class) {
              class(fr) <- c('quantmod.OHLC','zoo')
            } else
            if('zoo' %in% return.class) {
              fr
            } else
            if('ts' %in% return.class) {
              fr <- as.ts(fr)
            } else
            if('data.frame' %in% return.class) {
              fr <- as.data.frame(fr)
            } else
            if('its' %in% return.class) {
              if("package:its" %in% search() || require("its", quietly=TRUE)) {
                index(fr) <- as.POSIXct(index(fr))
                fr <- its::as.its(fr)
              } else {
                warning(paste("'its' from package 'its' could not be loaded:",
                        " 'zoo' class returned"))
              }
            } else 
            if('timeSeries' %in% return.class) {
              if("package:fCalendar" %in% search() || require("fCalendar",quietly=TRUE)) {
                fr <- as.timeSeries(fr)
              } else {
                warning(paste("'timeSeries' from package 'fCalendar' could not be loaded:",
                        " 'zoo' class returned"))
              }
            }
            assign(Symbols[[i]],fr,env)
            if(verbose) cat('done\n')
        }
        dbDisconnect(con)
        return(Symbols)

}
"getSymbols.mysql" <- getSymbols.MySQL
# }}}

# getSymbols.FRED {{{
`getSymbols.FRED` <- function(Symbols,env,
     return.class="zoo", ...) {
     importDefaults("getSymbols.FRED")
     this.env <- environment()
     for(var in names(list(...))) {
        # import all named elements that are NON formals
        assign(var, list(...)[[var]], this.env)
     }
     if(missing(verbose)) verbose <- FALSE
     FRED.URL <- "http://research.stlouisfed.org/fred2/series"
     for(i in 1:length(Symbols)) {
       if(verbose) cat("downloading ",Symbols[[i]],".....")
       fr <- read.csv(paste(FRED.URL,"/",
                            Symbols[[i]],"/",
                            "downloaddata/",
                            Symbols[[i]],".csv",sep=""))
       if(verbose) cat("done.\n")
       fr <- zoo(fr[,-1],as.Date(fr[,1]))
       dim(fr) <- c(NROW(fr),1)
       colnames(fr) <- as.character(toupper(Symbols[[i]]))
       if('zoo' %in% return.class) {
         fr
       }
       if('ts' %in% return.class) {
         fr <- as.ts(fr)
       } else
       if('data.frame' %in% return.class) {
         fr <- as.data.frame(fr)
       } else
       if('its' %in% return.class) {
         if("package:its" %in% search() || require("its", quietly=TRUE)) {
           index(fr) <- as.POSIXct(index(fr))
           fr <- its::as.its(fr)
         } else {
           warning(paste("'its' from package 'its' could not be loaded:",
                         " 'zoo' class returned"))
         }
       } else 
       if('timeSeries' %in% return.class) {
         if("package:fCalendar" %in% search() || require("fCalendar",quietly=TRUE)) {
           fr <- as.timeSeries(fr)
         } else {
           warning(paste("'timeSeries' from package 'fCalendar' could not be loaded:",
                   " 'zoo' class returned"))
         }
       }
       Symbols[[i]] <-toupper(gsub('\\^','',Symbols[[i]])) 
       assign(Symbols[[i]],fr,env)
     }
     return(Symbols)
} #}}}

"getSymbols.cache" <- function() {}
"getSymbols.file" <- function() {}
"getSymbols.url" <- function() {}
"getSymbols.freelunch" <- function() {}
"getSymbols.RODBC" <- function() {}
"getSymbols.oanda" <- function() {}

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
  fr <- modelData(specifyModel(formula,na.rm=na.rm))
  if('zoo' %in% return.class) {
    fr
  } else
  if('ts' %in% return.class) {
    fr <- as.ts(fr)
    return(fr)
  } else
  if('data.frame' %in% return.class) {
    fr <- as.data.frame(fr)
    return(fr)
  } else
  if('its' %in% return.class) {
    if("package:its" %in% search() || require("its", quietly=TRUE)) {
      index(fr) <- as.POSIXct(index(fr))
      fr <- its::as.its(fr)
      return(fr)
    } else {
      warning(paste("'its' from package 'its' could not be loaded:",
                    " 'zoo' class returned"))
    }
  } else 
  if('timeSeries' %in% return.class) {
    if("package:fCalendar" %in% search() || require("fCalendar",quietly=TRUE)) {
      fr <- as.timeSeries(fr)
      return(fr)
    } else {
      warning(paste("'timeSeries' from package 'fCalendar' could not be loaded:",
                    " 'zoo' class returned"))
    }
  } else {
    warning(paste("unable to return class",sQuote(return.class),":",
                  " 'zoo' class returned"))
  }
    
}
#}}}
