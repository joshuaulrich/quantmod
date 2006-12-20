"getSymbols" <-
function(symbols=NULL,
         field.names = c('date','o','h','l','c','v','a'),
         user=NULL,password=NULL,dbname=NULL)  {

      if(!is.null(symbols)) {
        symbols <- as.list(symbols)
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
        db.symbols <- dbListTables(con)
        if(length(symbols) != sum(symbols %in% db.symbols)) {
##            dbDisconnect(con);
            missing.db.symbol = unlist(symbols[which((symbols %in% db.symbols) == FALSE)])
            warning(paste('could not load symbol(s): ',paste(missing.db.symbol,collapse=', ')))
            symbols <- symbols[symbols %in% db.symbols]
        }
        for(i in 1:length(symbols)) {
            cat('Loading...',symbols[[i]])
            query <- paste("SELECT date,o,h,l,c,v,a FROM ",symbols[[i]]," ORDER BY date")
            rs <- dbSendQuery(con, query)
            fr <- fetch(rs, n=-1)
            fr <- data.frame(fr[,-1],row.names=fr[,1])
            colnames(fr) <- paste(symbols[[i]],
                                  c('Open','High','Low','Close','Volume','Adjusted'),
                                  sep='.')
            assign(symbols[[i]],fr,1)
        }
        dbDisconnect(con)
        invisible();
    }
}

