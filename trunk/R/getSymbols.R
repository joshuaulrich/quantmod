"getSymbols" <-
function(Symbols=NULL,
         env=.GlobalEnv,
         db.fields = c('date','o','h','l','c','v','a'),
         field.names = NULL,
         reload.Symbols = TRUE,
         verbose = FALSE,
         warnings = TRUE,
         na.rm = TRUE,
         user=NULL,password=NULL,dbname=NULL)  {

        if(reload.Symbols) {
            if(exists('.getSymbols',env,inherits=FALSE)) {
                old.Symbols <- get('.getSymbols',env)
                Symbols <- unique(c(old.Symbols, Symbols))
            }
        }
        removeSymbols(env)

      if(!is.null(Symbols)) {
        Symbols <- as.list(Symbols)
        if(is.null(user) || is.null(password) || is.null(dbname)) {
            if(!is.null(getOption('tR.db'))) {
                user <- getOption('tR.db')['user'];
                password <- getOption('tR.db')['password'];
                dbname <- getOption('tR.db')['dbname'];
            } else {
                stop('db connection params must be specified in fetchRawData call or via options("tR.")');
            }
        }
    
        con <- dbConnect(MySQL(),user=user,password=password,dbname=dbname)
        db.Symbols <- dbListTables(con)
        if(length(Symbols) != sum(Symbols %in% db.Symbols)) {
            missing.db.symbol = unlist(Symbols[which((Symbols %in% db.Symbols) == FALSE)])
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
            if(na.rm) {
                ##if(i > 1) fr <- subset(fr, rownames(fr) %in% dates)
                ##if(i==1) dates <- rownames(fr);
            }
            colnames(fr) <- paste(Symbols[[i]],
                                  c('Open','High','Low','Close','Volume','Adjusted'),
                                  sep='.')
            assign(Symbols[[i]],fr,env)
####            attach(get(Symbols[[i]],1),name=Symbols[[i]])
            if(verbose) cat('done\n')
        }
        dbDisconnect(con)
        assign('.getSymbols',Symbols,env);
        invisible(return(env))
    } else {warning('no Symbols specified')}
}


"removeSymbols" <- 
function(env=.GlobalEnv) {
    if(exists('.getSymbols',env,inherits=FALSE)) {
        Symbols <- paste(get('.getSymbols',env))
        remove(list=c('.getSymbols'),envir=env)
    #            search.pos = which(match(search(),symbols[i])==TRUE)
    #            if(length(search.pos) > 0) detach(pos=search.pos)            
        remove(list=as.character(Symbols),envir=env)
    }
}

"showSymbols" <-
function(env=.GlobalEnv) {
    if(exists('.getSymbols',env,inherits=FALSE)) {
        return(unlist(get('.getSymbols',env)))
    } else { return(NULL) }
}
