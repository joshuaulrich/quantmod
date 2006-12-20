"fetchRawData" <-
function(symbols=NULL,cols="ohlcva",user=NULL,password=NULL,dbname=NULL) {
  if(!is.null(symbols)) {

    output <- NULL
    from <- " FROM "
    where <- NULL
    symbols <- as.list(symbols)
    query <- paste("SELECT ",symbols[[1]],".date",sep="")
    orderby <- paste(" ORDER BY ",symbols[[1]],".date",sep="")

    ## build col selections
    if(length(cols)==1 || (length(cols) != length(symbols))) {
      cols <- rep(cols[1],length(symbols))     
    }

    for(i in 1:length(symbols)) {
      columns = unlist(strsplit(cols[i],NULL))
      query <- paste(query,", ",sep="")
      for(j in 1:length(columns)) {
        if( j<length(columns) ) {
          sp = ", "        		## used to seperate columns in SELECT
        } else {
          sp = ""
        }
        query <- paste(query,symbols[[i]],".",columns[j]," AS ",columns[j],symbols[[i]],sp,sep="")      
      }
      if( i<length(symbols) ) {
        sp = ", "			## used to seperate tables in FROM        
        nd = " AND "			## used to seperate date test in WHERE
      } else {
        sp = ""
        nd = ""
      }
      from <- paste(from,symbols[[i]],sp,sep="")
      where <- paste(where,symbols[[i]],".date=",symbols[[1]],".date ",nd,sep="")
    }
    
  }
  query <- paste(query,from," WHERE (",where,") ",orderby,sep="")

    # connection to mysql server.  passed params are checked first, then those stored
    # in options('tR.db').  if both fail command aborts with message.
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
    rs <- dbSendQuery(con, query)
    fr <- fetch(rs, n=-1)
    dbDisconnect(con)
    return(fr)
}

