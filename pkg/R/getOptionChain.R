`getOptionChain` <-
function(Symbols, Exp=NULL, src="yahoo", ...) {
  Call <- paste("getOptionChain",src,sep=".")
  if(missing(Exp)) {
    do.call(Call, list(Symbols=Symbols, ...))
  } else {
    do.call(Call, list(Symbols=Symbols, Exp=Exp, ...))
  }
}


getOptionChain.yahoo <- function(Symbols, Exp, ...)
{
  parse.expiry <- function(x) {
    if(is.null(x))
      return(NULL)
    if (nchar(x) == 5L) {
      x <- sprintf(substring(x, 4, 5), match(substring(x, 
          1, 3), month.abb), fmt = "20%s-%02i")
    }
    else if (nchar(x) == 6L) {
      x <- paste(substring(x, 1, 4), substring(x, 5, 6), 
          sep = "-")
    }
    return(x)
  }
  if(missing(Exp))
    opt <- readLines(paste("http://finance.yahoo.com/q/op?s",Symbols,sep="="))
  else
    opt <- readLines(paste("http://finance.yahoo.com/q/op?s=",Symbols,"&m=",parse.expiry(Exp),sep=""))
  opt <- opt[grep("Expire at",opt)]

  if(!missing(Exp) && is.null(Exp)) {
    allExp <- substr(strsplit(strsplit(opt,"<tr.*?>")[[1]][12],"m=")[[1]][-1],0,7)
    # fix for missing current month in links
    allExp <- c(format(as.yearmon(allExp[1]) - 1/12, "%Y-%m"), allExp)

    return(structure(lapply(allExp, getOptionChain.yahoo, Symbols=Symbols), .Names=format(as.yearmon(allExp))))
  }

  where <- cumsum(rle(sapply(gregexpr(paste("s",Symbols,sep="="),strsplit(opt, "<tr")[[1]]),
                             function(x) if(x[1] > 0) TRUE else FALSE))[[1]])[c(5:8)]
  CNAMES <- c("Strike","Last","Chg","Bid","Ask","Vol","OI")

  # calls
  down <- grep("cc0000",strsplit(opt,"<tr.*?>")[[1]][seq(where[1],where[2])])-1
  calls <- strsplit(gsub("\\s+"," ",gsub("<.*?>"," ",strsplit(opt,"<tr.*?>")[[1]][seq(where[1],where[2])]))," ")
  calls <- do.call(rbind,calls[-1])[,-1]
  callOSI <- calls[,2]
  calls <- data.frame(calls[,-2])
  calls <- apply(calls, 2, function(x) suppressWarnings(as.numeric(gsub(",","",x))))
  calls[down,3] <- calls[down,3] * -1
  colnames(calls) <- CNAMES
  rownames(calls) <- callOSI

  # puts
  down <- grep("cc0000",strsplit(opt,"<tr.*?>")[[1]][seq(where[3],where[4])])-1
  puts <- strsplit(gsub("\\s+"," ",gsub("<.*?>"," ",strsplit(opt,"<tr.*?>")[[1]][seq(where[3],where[4])]))," ")
  puts <- do.call(rbind,puts[-1])[,-1]
  putOSI <- puts[,2]
  puts <- data.frame(puts[,-2])
  puts <- apply(puts, 2, function(x) suppressWarnings(as.numeric(gsub(",","",x))))
  puts[down,3] <- puts[down,3] * -1
  colnames(puts) <- CNAMES
  rownames(puts) <- putOSI

  list(calls=calls,puts=puts,symbol=Symbols)
}

