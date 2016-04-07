# getSymbols {{{
"getSymbols" <-
function(Symbols=NULL,
         env=parent.frame(),  ### 0.4-0
         #env=NULL,          ### 0.5-0
         reload.Symbols=FALSE,
         verbose=FALSE,
         warnings=TRUE,
         src="yahoo",
         symbol.lookup=TRUE,
         auto.assign=getOption('getSymbols.auto.assign',TRUE),
         ...)  {
      if(getOption("getSymbols.warning4.0",TRUE)) {
        # transition message for 0.4-0 to 0.5-0
        message(paste(
                '    As of 0.4-0,',sQuote('getSymbols'),'uses env=parent.frame() and\n',
                'auto.assign=TRUE by default.\n\n',

                'This  behavior  will be  phased out in 0.5-0  when the call  will\n',
                'default to use auto.assign=FALSE. getOption("getSymbols.env") and \n',
                'getOptions("getSymbols.auto.assign") are now checked for alternate defaults\n\n',
                'This message is shown once per session and may be disabled by setting \n',
                'options("getSymbols.warning4.0"=FALSE). See ?getSymbols for more details.'))
        options("getSymbols.warning4.0"=FALSE) 
      }
      importDefaults("getSymbols")
      #  to enable as-it-was behavior, set this:
      #  options(getSymbols=list(env=substitute(parent.frame(3))))

      #if(missing(env))
      #  env <- eval(getOption("getSymbols")$env)    ### 0.5-0

      if(missing(env) && !is.null(getOption("getSymbols.env")) )
          env <- getOption("getSymbols.env")         ### 0.4-0

      #env_ <- getSymbols_options_("env")
      #if(missing(env) && !is.null(env_))
      #  env <- env_
      if(is.null(env)) # default as of 0.5-0
        auto.assign <- FALSE
      if(!auto.assign && length(Symbols)>1)
        stop("must use auto.assign=TRUE for multiple Symbols requests")
      force(Symbols)  # need to check if symbol lookup defined _within_ call
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
      if(auto.assign && exists('.getSymbols',env,inherits=FALSE)) {
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
          return(req.symbols)
          #return(env)
        }
      } else {
        warning('no Symbols specified')
      }
}
#}}}

loadSymbols <- getSymbols
loadSymbols.formals <- c(formals(getSymbols)[-(8:9)], alist(auto.assign=getOption("loadSymbols.auto.assign",TRUE),...=))
formals(loadSymbols) <- loadSymbols.formals


# getSymbols.Bloomberg {{{
#"getSymbols.Bloomberg" <- function(Symbols,env,return.class='xts',
#                              from=as.POSIXlt(Sys.time()-60*60,"GMT"),
#                              to=as.POSIXlt(Sys.time(),"GMT"),
#                              bb.suffix="Equity",
#                              bb.interval="5",
#                              ...) {
#    importDefaults("getSymbols.Bloomberg")
#    this.env <- environment()
#    for(var in names(list(...))) {
#       # import all named elements that are NON formals
#       assign(var, list(...)[[var]], this.env)
#    }
#    if ((class(from)=="Date" && class(to)=="Date") ||
#           (class(from)=="character" && length(from)<=8 &&
#               class(to)=="character" && length(to)<=8 )) {
#       bb.intraday <- FALSE
#       bb.call <- bdh
#       bb.fields <- c("OPEN", "HIGH", "LOW", "PX_LAST", "VOLUME")
#    } else {
#       bb.intraday <- TRUE
#       bb.call <- bar
#       bb.fields <- "TRADE"
#    }
#    if(missing(verbose)) verbose <- FALSE
#    if(missing(auto.assign)) auto.assign <- TRUE
#       if('package:RBloomberg' %in% search() ||
#require('RBloomberg',quietly=TRUE)) {
#         {}
#       } else {
#         stop(paste("package:",dQuote('RBloomberg'),"cannot be loaded."))
#       }
#       bbconn <- blpConnect()
#       for(i in 1:length(Symbols)) {
#           bbsym <- paste(Symbols[[i]],bb.suffix)
#
#           if(verbose) {
#               cat(paste('Loading ',bbsym, ' from BB ', from,' to ',to,
#                   paste(rep('.',18-nchar(Symbols[[i]])),collapse=''),
#                   sep=''))
#
#           }
#           tryCatch (
#             {
#               if (bb.intraday) {
#                 fromStr <- paste(as.character(from),".000",sep="")
#                 toStr <- paste(as.character(to),".000",sep="")
#                 b <- bb.call(bbconn, bbsym, bb.fields,
#                             fromStr, toStr, bb.interval)
#                 b$datetime <- as.POSIXct(strptime(b$time,
#format="%Y-%m-%dT%H:%M:%S"))
#                 bxo <- as.xts(b$open, order.by=b$datetime)
#                 fr <- merge(bxo,  b$high, b$low, b$close, b$volume)
#               } else {
#                 if (class(from)=="character") {
#                   fromStr <- from
#                 } else {
#                   fromStr <- strftime(from,format="%Y%m%d")
#                 }
#                 if (class(to)=="character") {
#                   toStr <- to
#                 } else {
#                   toStr <- strftime(to,format="%Y%m%d")
#                 }
#                 b <- bb.call(bbconn, bbsym, bb.fields,
#                             fromStr, toStr)
#                 b$datetime <- as.POSIXct(strptime(b$date,
#format="%Y-%m-%d"))
#                 bxo <- as.xts(b$OPEN, order.by=b$datetime)
#                 fr <- merge(bxo,  b$HIGH, b$LOW, b$PX_LAST, b$VOLUME)
#               }
#
#
#
#               if(verbose) {
#                 cat(paste(length(fr),'points '))
#               }
#               colnames(fr) <- paste(Symbols[[i]],
#                                     c('Open','High','Low','Close','Volume'),
#                                     sep='.')
#               fr <- convert.time.series(fr=fr,return.class=return.class)
#               if(auto.assign)
#                 assign(Symbols[[i]],fr,env)
#             },
#             error=function(e) {print(e);fr <- data.frame()},
#             finally=function () {if(verbose) {cat('done\n')}}
#           )
#       }
#       blpDisconnect(bbconn)
#       if(auto.assign)
#         return(Symbols)
#       return(fr)
#}
#"getSymbols.Bloomberg" <- getSymbols.Bloomberg
# }}}

# getSymbols.yahoo {{{
"getSymbols.yahoo" <-
function(Symbols,env,return.class='xts',index.class="Date",
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
     if(!hasArg(adjust))
       adjust <- FALSE

     default.return.class <- return.class
     default.from <- from
     default.to <- to

     if(!hasArg(verbose)) verbose <- FALSE
     if(!hasArg(auto.assign)) auto.assign <- TRUE
     yahoo.URL <- "http://ichart.finance.yahoo.com/table.csv?"

     tmp <- tempfile()
     on.exit(unlink(tmp))
     for(i in 1:length(Symbols)) {
       return.class <- getSymbolLookup()[[Symbols[[i]]]]$return.class
       return.class <- ifelse(is.null(return.class),default.return.class,
                              return.class)
       from <- getSymbolLookup()[[Symbols[[i]]]]$from
       from <- if(is.null(from)) default.from else from
       to <- getSymbolLookup()[[Symbols[[i]]]]$to
       to <- if(is.null(to)) default.to else to
   
       from.y <- as.numeric(strsplit(as.character(as.Date(from,origin='1970-01-01')),'-',)[[1]][1])
       from.m <- as.numeric(strsplit(as.character(as.Date(from,origin='1970-01-01')),'-',)[[1]][2])-1
       from.d <- as.numeric(strsplit(as.character(as.Date(from,origin='1970-01-01')),'-',)[[1]][3])
       to.y <- as.numeric(strsplit(as.character(as.Date(to,origin='1970-01-01')),'-',)[[1]][1])
       to.m <- as.numeric(strsplit(as.character(as.Date(to,origin='1970-01-01')),'-',)[[1]][2])-1
       to.d <- as.numeric(strsplit(as.character(as.Date(to,origin='1970-01-01')),'-',)[[1]][3])
       
       Symbols.name <- getSymbolLookup()[[Symbols[[i]]]]$name
       Symbols.name <- ifelse(is.null(Symbols.name),Symbols[[i]],Symbols.name)
       if(verbose) cat("downloading ",Symbols.name,".....\n\n")
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
       if(verbose) cat("done.\n")
       fr <- xts(as.matrix(fr[,-1]),
                 as.Date(fr[,1]),
                 #as.POSIXct(fr[,1], tz=Sys.getenv("TZ")),
                 src='yahoo',updated=Sys.time())
       colnames(fr) <- paste(toupper(gsub('\\^','',Symbols.name)),
                             c('Open','High','Low','Close','Volume','Adjusted'),
                             sep='.')
       if(adjust) {
         # Adjustment algorithm by Joshua Ulrich
         fr <- adjustOHLC(fr, symbol.name=Symbols.name)
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

# getSymbols.yahooj {{{
"getSymbols.yahooj" <-
    function(Symbols, env=parent.frame(), return.class='xts', index.class="Date",
             from='2007-01-01',
             to=Sys.Date(),
             ...)
    {
        importDefaults("getSymbols.yahooj")
        this.env <- environment()
        for(var in names(list(...))) {
            # import all named elements that are NON formals
            assign(var, list(...)[[var]], this.env)
        }
        if(!hasArg(adjust))
            adjust <- FALSE
        
        default.return.class <- return.class
        default.from <- from
        default.to <- to
        
        if(!hasArg(verbose)) verbose <- FALSE
        if(!hasArg(auto.assign)) auto.assign <- TRUE

        if(!requireNamespace("XML", quietly=TRUE))
          stop("package:",dQuote("XML"),"cannot be loaded.")

        yahoo.URL <- "http://info.finance.yahoo.co.jp/history/"
        for(i in 1:length(Symbols)) {
            # The name of the symbol, which will actually be used as the
            # variable name. It needs to start with YJ, and it will be appended
            # if it does not.
            symname <- toupper(Symbols[[i]])
            
            # The symbol actually sent to Yahoo Japan. This is without the
            # starting YJ bit.
            symbol <- symname
            
            # If it starts with YJ, try looking up defaults
            if (grepl("^YJ", symname)) {
                return.class <- getSymbolLookup()[[symname]]$return.class
                return.class <- ifelse(is.null(return.class),default.return.class,
                                       return.class)
                from <- getSymbolLookup()[[symname]]$from
                from <- if(is.null(from)) default.from else from
                to <- getSymbolLookup()[[symname]]$to
                to <- if(is.null(to)) default.to else to
                
                # Extract the actual symbol to be sent to Yahoo Japan
                symbol <- substring(symname, 3)
            } else {
                return.class <- default.return.class
                from <- default.from
                to <- default.to
                
                # Prepend 'YJ' to the symbol and store it in symname
                symname <- paste('YJ', symbol, sep="")
            }

            from.y <- as.numeric(strsplit(as.character(as.Date(from,origin='1970-01-01')),'-',)[[1]][1])
            from.m <- as.numeric(strsplit(as.character(as.Date(from,origin='1970-01-01')),'-',)[[1]][2])
            from.d <- as.numeric(strsplit(as.character(as.Date(from,origin='1970-01-01')),'-',)[[1]][3])
            to.y <- as.numeric(strsplit(as.character(as.Date(to,origin='1970-01-01')),'-',)[[1]][1])
            to.m <- as.numeric(strsplit(as.character(as.Date(to,origin='1970-01-01')),'-',)[[1]][2])
            to.d <- as.numeric(strsplit(as.character(as.Date(to,origin='1970-01-01')),'-',)[[1]][3])
            
            Symbols.name <- getSymbolLookup()[[symname]]$name
            Symbols.name <- ifelse(is.null(Symbols.name),symbol,Symbols.name)
            if(verbose) cat("downloading ",Symbols.name,".....\n\n")
            
            page <- 1
            totalrows <- c()
            tmp <- tempfile()
            on.exit(unlink(tmp))
            while (TRUE) {
                download.file(paste(yahoo.URL,
                                    "?code=",Symbols.name,
                                    "&sm=",from.m,
                                    "&sd=",sprintf('%.2d',from.d),
                                    "&sy=",from.y,
                                    "&em=",to.m,
                                    "&ed=",sprintf('%.2d',to.d),
                                    "&ey=",to.y,
                                    "&tm=d",
                                    "&p=",page,
                                    sep=''),destfile=tmp,quiet=!verbose)
                
                fdoc <- XML::htmlParse(tmp)
                rows <- XML::xpathApply(fdoc, "//table[@class='boardFin yjSt marB6']//tr")
                if (length(rows) == 1) break
                
                totalrows <- c(totalrows, rows)
                page <- page + 1
            }
            if(verbose) cat("done.\n")
            
            # Available columns
            cols <- c('Open','High','Low','Close','Volume','Adjusted')
            
            firstrow <- totalrows[[1]]
            cells <- XML::getNodeSet(firstrow, "th")
            if (length(cells) == 5) cols <- cols[-(5:6)]

            # Process from the start, for easier stocksplit management
            totalrows <- rev(totalrows)
            mat <- matrix(0, ncol=length(cols) + 1, nrow=0, byrow=TRUE)
            for(row in totalrows) {
                cells <- XML::getNodeSet(row, "td")
                
                # 2 cells means it is a stocksplit row
                # So extract stocksplit data and recalculate the matrix we have so far
                if (length(cells) == 2 & length(cols) == 6 & nrow(mat) > 1) {
                    ss.data <- as.numeric(na.omit(as.numeric(unlist(strsplit(XML::xmlValue(cells[[2]]), "[^0-9]+")))))
                    factor <- ss.data[2] / ss.data[1]
                    
                    mat <- rbind(t(apply(mat[-nrow(mat),], 1, function(x) {
                        x * c(1, rep(1/factor, 4), factor, 1)
                    })), mat[nrow(mat),])
                }
                
                if (length(cells) != length(cols) + 1) next
                
                # Parse the Japanese date format using UTF characters
                # \u5e74 = "year"
                # \u6708 = "month"
                # \u65e5 = "day"
                date <- as.Date(XML::xmlValue(cells[[1]]), format="%Y\u5e74%m\u6708%d\u65e5")
                entry <- c(date)
                for(n in 2:length(cells)) {
                    entry <- cbind(entry, as.numeric(gsub(",", "", XML::xmlValue(cells[[n]]))))
                }
                
                mat <- rbind(mat, entry)
            }
            
            fr <- xts(mat[, -1], as.Date(mat[, 1]), src="yahooj", updated=Sys.time())
            
            colnames(fr) <- paste(symname, cols, sep='.')
            
            fr <- convert.time.series(fr=fr,return.class=return.class)
            if(is.xts(fr))
                indexClass(fr) <- index.class
            
            Symbols[[i]] <- symname
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
     if(!hasArg(verbose)) verbose <- FALSE
     if(!hasArg(auto.assign)) auto.assign <- TRUE
     google.URL <- "http://finance.google.com/finance/historical?"
     from.y <- as.numeric(strsplit(as.character(from),'-',)[[1]][1])
     from.m <- as.numeric(strsplit(as.character(from),'-',)[[1]][2])
     from.d <- as.numeric(strsplit(as.character(from),'-',)[[1]][3])
     to.y <- as.numeric(strsplit(as.character(to),'-',)[[1]][1])
     to.m <- as.numeric(strsplit(as.character(to),'-',)[[1]][2])
     to.d <- as.numeric(strsplit(as.character(to),'-',)[[1]][3])

     tmp <- tempfile()
     on.exit(unlink(tmp))
     for(i in 1:length(Symbols)) {
       Symbols.name <- getSymbolLookup()[[Symbols[[i]]]]$name
       Symbols.name <- ifelse(is.null(Symbols.name),Symbols[[i]],Symbols.name)
       if(verbose) cat("downloading ",Symbols.name,".....\n\n")
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
       # convert '-' to NAs
       suppressWarnings(storage.mode(fr) <- "numeric")
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
     if(!hasArg(verbose)) verbose <- FALSE
     if(!hasArg(auto.assign)) auto.assign <- TRUE

     if(!requireNamespace("DBI", quietly=TRUE))
       stop("package:",dQuote("DBI"),"cannot be loaded.")
     if(!requireNamespace("RSQLite", quietly=TRUE))
       stop("package:",dQuote("RSQLite"),"cannot be loaded.")

        drv <- DBI::dbDriver("SQLite")
        con <- DBI::dbConnect(drv,dbname=dbname)
        db.Symbols <- DBI::dbListTables(con)
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
            rs <- DBI::dbSendQuery(con, query)
            fr <- DBI::fetch(rs, n=-1)
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
        DBI::dbDisconnect(con)
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
                               user=NULL,password=NULL,dbname=NULL,host='localhost',port=3306,
                               ...) {
     importDefaults("getSymbols.MySQL")
     this.env <- environment()
     for(var in names(list(...))) {
        # import all named elements that are NON formals
        assign(var, list(...)[[var]], this.env)
     }
     if(!hasArg(verbose)) verbose <- FALSE
     if(!hasArg(auto.assign)) auto.assign <- TRUE

     if(!requireNamespace("DBI", quietly=TRUE))
       stop("package:",dQuote("DBI"),"cannot be loaded.")
     if(!requireNamespace("RMySQL", quietly=TRUE))
       stop("package:",dQuote("RMySQL"),"cannot be loaded.")

        if(is.null(user) || is.null(password) || is.null(dbname)) {
          stop(paste(
              'At least one connection argument (',sQuote('user'),
              sQuote('password'),sQuote('dbname'),
              ") is not set"))
        }
        con <- DBI::dbConnect(RMySQL::MySQL(),user=user,password=password,dbname=dbname,host=host,port=port)
        db.Symbols <- DBI::dbListTables(con)
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
            rs <- DBI::dbSendQuery(con, query)
            fr <- DBI::fetch(rs, n=-1)
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
        DBI::dbDisconnect(con)
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
     if(!hasArg(verbose)) verbose <- FALSE
     if(!hasArg(auto.assign)) auto.assign <- TRUE
     FRED.URL <- "https://research.stlouisfed.org/fred2/series"

     tmp <- tempfile()
     on.exit(unlink(tmp))
     for(i in 1:length(Symbols)) {
       if(verbose) cat("downloading ",Symbols[[i]],".....\n\n")
       URL <- paste(FRED.URL, "/", Symbols[[i]], "/downloaddata/", Symbols[[i]], ".csv", sep="")
       try.download.file(URL, destfile=tmp, quiet=!verbose, ...)
       fr <- read.csv(tmp,na.string=".")
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
function(Currencies,from=Sys.Date()-499,to=Sys.Date(),
         env=parent.frame(),
         verbose=FALSE,warning=TRUE,
         auto.assign=TRUE,...) {
  importDefaults("getFX")
  if(missing(env))
    env <- parent.frame(1)
  if(is.null(env))
    auto.assign <- FALSE
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
         base.currency="USD",env=parent.frame(),
         verbose=FALSE,warning=TRUE,
         auto.assign=TRUE,...) {
  importDefaults("getMetals")
  if(missing(env))
    env <- parent.frame(1)
  if(is.null(env))
    auto.assign <- FALSE
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
         col.names=c('Open','High','Low','Close','Volume','Adjusted'),
         ...) {
  importDefaults("getSymbols.csv")
  this.env <- environment()
  for(var in names(list(...))) {
    assign(var,list(...)[[var]], this.env)
  }

  default.return.class <- return.class
  default.dir <- dir
  default.extension <- extension

  if(!hasArg(verbose)) verbose <- FALSE
  if(!hasArg(auto.assign)) auto.assign <- TRUE

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

    # ensure date column is character before calling as.Date
    asDateArgs <- list(x=as.character(fr[,1]))
    # use format passed via '...', if specified
    if(hasArg("format"))
      asDateArgs$format <- format
    # allow format from setSymbolLookup to override
    if(!is.null(getSymbolLookup()[[Symbols[[i]]]]$format))
      asDateArgs$format <- getSymbolLookup()[[Symbols[[i]]]]$format

    fr <- xts(fr[,-1],do.call("as.Date", asDateArgs),src='csv',updated=Sys.time())
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

  if(!hasArg(verbose)) verbose <- FALSE
  if(!hasArg(auto.assign)) auto.assign <- TRUE

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
    fr <- readRDS(sym.file)
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

  if(!hasArg(verbose)) verbose <- FALSE
  if(!hasArg(auto.assign)) auto.assign <- TRUE

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
  if(!hasArg(verbose))
    verbose <- FALSE
  if(!hasArg(auto.assign))
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
                                timeFormat=time.format, verbose=verbose))
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

     if(!hasArg(verbose)) verbose <- FALSE
     if(!hasArg(auto.assign)) auto.assign <- TRUE

     # Request minimum data from server to fulfill user's request
     daySpans <- c(7, 30, 60, 90, 180, 364, 728, 1820)
     dateStr <- c("d7", "d30", "d60", "d90", "d180", "y1", "y2", "y5")

     tmp <- tempfile()
     on.exit(unlink(tmp))
     for(i in 1:length(Symbols)) {
       return.class <- getSymbolLookup()[[Symbols[[i]]]]$return.class
       return.class <- ifelse(is.null(return.class),default.return.class,
                              return.class)
       from <- getSymbolLookup()[[Symbols[[i]]]]$from
       from <- ifelse(is.null(from),default.from,from)
       from <- as.Date(from, origin='1970-01-01')
       to <- getSymbolLookup()[[Symbols[[i]]]]$to
       to <- ifelse(is.null(to),default.to,to)
       to <- as.Date(to, origin='1970-01-01')

       Symbols.name <- getSymbolLookup()[[Symbols[[i]]]]$name
       Symbols.name <- ifelse(is.null(Symbols.name),Symbols[[i]],Symbols.name)
       currency.pair <- strsplit(toupper(Symbols.name),"/")[[1]]
       if(length(currency.pair) != 2) {
         warning(paste("incorrectly specified currency pair",Symbols.name))
         next
       }

       if(verbose) cat("downloading ",Symbols.name,".....")
       # Request minimum data from server to fulfill user's request
       dateDiff <- difftime(to, from, units="days")
       dateLoc <- which(daySpans >= dateDiff)
       # throw warning, but return as much data as possible
       if(!length(dateLoc)) {
           warning("Oanda limits data to 5years. Symbol: ", Symbols[[i]])
           dateLoc <- length(dateStr)
       }
       data_range <- dateStr[dateLoc[1]]
       oanda.URL <- paste("https://www.oanda.com/currency/historical-rates/download?",
         "quote_currency=", currency.pair[1],
         "&end_date=", to,
         "&start_date=", from,
         "&period=daily&display=absolute&rate=0",
         "&data_range=", data_range,
         "&price=mid&view=table",
         "&base_currency_0=", currency.pair[2],
         "&base_currency_1=&base_currency_2=&base_currency_3=&base_currency_4=&download=csv",
         sep="")
       download.file(oanda.URL, destfile=tmp, quiet=!verbose)
       fr <- read.csv(tmp, skip=4, as.is=TRUE, header=TRUE)
       fr[,1L] <- as.Date(fr[,1L], origin="1970-01-01")
       fr <- na.omit(fr[,1:2])    # remove period mean/min/max from end of file
       if(is.character(fr[,2L]))  # remove thousands seperator and convert
         fr[,2L] <- as.numeric(gsub(",", "", fr[,2L], fixed=TRUE))

       if(verbose) cat("done.\n")
       fr <- xts(fr[,-1L], fr[,1L], src='oanda', updated=Sys.time())
       fr <- fr[paste(from, to, sep="/")]  # subset to requested timespan
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
         if(requireNamespace("its", quietly=TRUE)) {
           fr.dates <- as.POSIXct(as.character(index(fr)))
           fr <- its::its(coredata(fr),fr.dates)
           return(fr)
         } else {
           warning(paste("'its' from package 'its' could not be loaded:",
                         " 'xts' class returned"))
         }
       } else 
       if('timeSeries' %in% return.class) {
         if(requireNamespace("timeSeries", quietly=TRUE)) {
           fr <- timeSeries::timeSeries(coredata(fr), charvec=as.character(index(fr)))
           return(fr)
         } else {
           warning(paste("'timeSeries' from package 'timeSeries' could not be loaded:",
                   " 'xts' class returned"))
         }
       }
}#}}}

# removeSymbols {{{
"removeSymbols" <- 
function(Symbols=NULL,env=parent.frame()) {
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
function(env=parent.frame()) {
    if(exists('.getSymbols',env,inherits=FALSE)) {
        return(unlist(get('.getSymbols',env)))
    } else { return(NULL) }
}
# }}}

# saveSymbols {{{
"saveSymbols"<-
function(Symbols=NULL,file.path=stop("must specify 'file.path'"),env=parent.frame()) {
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
           envir=env)
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
