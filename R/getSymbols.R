"getSymbols" <-
function(Symbols=NULL,
         env=.GlobalEnv,
         db.fields = c('date','o','h','l','c','v','a'),
         return.type = c('quantmod.ohlc','data.frame'),
         field.names = NULL,
         reload.Symbols = TRUE,
         verbose = FALSE,
         warnings = TRUE,
         src = c("MySQL","yahoo","google","economagic"),
         user=NULL,password=NULL,dbname=NULL)  {

        if(reload.Symbols) {
            if(exists('.getSymbols',env,inherits=FALSE)) {
                old.Symbols <- get('.getSymbols',env)
                Symbols <- unique(c(old.Symbols, Symbols))
            }
        }
        removeSymbols(env=env)
#this needs to be changed to accomodate additional methods - no partials??!!
      available.src = c("MySQL","yahoo","google","economagic")
      src = available.src[pmatch(src,available.src)][1]

      if(!is.null(Symbols)) {
        Symbols <- as.list(Symbols)
        Symbols <- do.call(paste('getSymbols.',src,sep=''),list(Symbols=Symbols,env=env,db.fields=db.fields,
                                        field.names=field.names,reload.Symbols=reload.Symbols,
                                        verbose=verbose,warnings=warnings,
                                        user=user,password=password,dbname=dbname))

        assign('.getSymbols',Symbols,env);
        invisible(return(env))
    } else {warning('no Symbols specified')}
}
"getSymbols.MySQL" <- function(Symbols,env,
                               db.fields=c('date','o','h','l','c','v','a'),
                               return.type = c('quantmod.ohlc','data.frame'),
                               field.names = NULL,
                               reload.Symbols = TRUE,
                               verbose = FALSE,
                               warnings = TRUE,
                               user=NULL,password=NULL,dbname=NULL,...) {
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
            fr <- data.frame(fr[,-1],row.names=fr[,1])
            colnames(fr) <- paste(Symbols[[i]],
                                  c('Open','High','Low','Close','Volume','Adjusted'),
                                  sep='.')
            class(fr) <- c("quantmod.OHLC","data.frame")
            assign(Symbols[[i]],fr,env)
            if(verbose) cat('done\n')
        }
        dbDisconnect(con)
        return(Symbols)

}
"removeSymbols" <- 
function(Symbols=NULL,env=.GlobalEnv) {
    if(exists('.getSymbols',env,inherits=FALSE)) {
    getSymbols <- get('.getSymbols',env,inherits=FALSE)
      if(is.null(Symbols)) {
        Symbols <- paste(getSymbols)
      } else {
        #Symbols now has ONLY existing Symbols in it
        Symbols <- Symbols[Symbols %in% unlist(getSymbols)]
      }
      remove(list=as.character(Symbols),envir=env)
      Symbols.remaining <- getSymbols[!unlist(getSymbols) %in% Symbols]
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
