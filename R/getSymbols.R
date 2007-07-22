"getSymbols" <-
function(Symbols=NULL,
         env=.GlobalEnv,
         return.class = "zoo",
         reload.Symbols = FALSE,
         verbose = FALSE,
         warnings = TRUE,
         src = c("yahoo","MySQL","google","economagic"),
         symbol.lookup = TRUE,
         ...)  {

      importDefaults()
      if(symbol.lookup) {
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
                                           return.class=return.class,
                                           reload.Symbols=reload.Symbols,
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
"getSymbols.yahoo" <-
function(Symbols,env,return.class=c('quantmod.OHLC','zoo'),
         from='1990-01-01',
         to=Sys.Date(),
         verbose = FALSE,
         warnings = TRUE,
         ...)
{
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
       if('zoo' == return.class) {
         fr
       }
       if('ts' == return.class) {
         fr <- as.ts(fr)
       } else
       if('data.frame' == return.class) {
         fr <- as.data.frame(fr)
       } else
       if('its' == return.class) {
         if("package:its" %in% search() || require("its", quietly=TRUE)) {
           index(fr) <- as.POSIXct(index(fr))
           fr <- its::as.its(fr)
         } else {
           warning("package its could not be loaded: 'zoo' class returned")
         }
       }
       Symbols[[i]] <-toupper(gsub('\\^','',Symbols[[i]])) 
       assign(Symbols[[i]],fr,env)
     }
     return(Symbols)
}
"getSymbols.cache" <- function() {}
"getSymbols.file" <- function() {}
"getSymbols.url" <- function() {}
"getSymbols.freelunch" <- function() {}
"getSymbols.RODBC" <- function() {}
"getSymbols.oanda" <- function() {}

"getSymbols.MySQL" <- function(Symbols,env,return.class=c('quantmod.OHLC','zoo'),
                               db.fields=c('date','o','h','l','c','v','a'),
                               field.names = NULL,
                               verbose = FALSE,
                               warnings = TRUE,
                               user=NULL,password=NULL,dbname=NULL,...) {
        if('package:DBI' %in% search() || require('DBI',quietly=TRUE)) {
          if('package:RMySQL' %in% search() || require('RMySQL',quietly=TRUE)) {
          } else { warning("package RMySQL can't be loaded" ) }
        } else {
          stop(" package DBI can't be loaded.")
        }
        if(is.null(user) || is.null(password) || is.null(dbname)) {
            if(!is.null(getOption('tR.db'))) {
                user <- getOption('tR.db')$user
                password <- getOption('tR.db')$password
                dbname <- getOption('tR.db')$dbname
            } else {
                stop('db connection params must be specified in fetchRawData call or via options("tR.")');
            }
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
            if('zoo' == return.class) {
              fr
            } else
            if('ts' == return.class) {
              fr <- as.ts(fr)
            } else
            if('data.frame' == return.class) {
              fr <- as.data.frame(fr)
            } else
            if('its' == return.class) {
              if("package:its" %in% search() || require("its", quietly=TRUE)) {
                index(fr) <- as.POSIXct(index(fr))
                fr <- its::as.its(fr)
              } else {
                warning("package 'its' could not be loaded: 'quantmod.OHLC' class returned")
                class(fr) <- c('quantmod.OHLC','zoo')
              }
            }
            assign(Symbols[[i]],fr,env)
            if(verbose) cat('done\n')
        }
        dbDisconnect(con)
        return(Symbols)

}
"getSymbols.mysql" <- getSymbols.MySQL

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

"showSymbols" <-
function(env=.GlobalEnv) {
    if(exists('.getSymbols',env,inherits=FALSE)) {
        return(unlist(get('.getSymbols',env)))
    } else { return(NULL) }
}

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
