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

    if(inherits(x, "Date") || inherits(x, "POSIXt"))
      return(format(x, "%Y-%m"))

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

  parseOptionTable_ <- function(x) {
    opt <- x
    os <- lapply(as.list(lapply(strsplit(opt,"<tr>"), function(.) gsub(",","",gsub("N/A","NA",gsub("(^ )|( $)","",gsub("[ ]+"," ",gsub("<.*?>"," ", .))))))[[1]]), function(.) strsplit(.," ")[[1]])
    which.opts <- sapply(os,function(.) length(.)==8)
    up <- grep("Up", strsplit(opt, "<tr>")[[1]][which.opts])
    dn <- grep("Down", strsplit(opt, "<tr>")[[1]][which.opts])
    allcontracts <- do.call(rbind,os[sapply(os,function(.) length(.) == 8)])
    rownames. <- allcontracts[,2]
    allcontracts <- allcontracts[,-2]
    suppressWarnings(storage.mode(allcontracts) <- "double")
    allcontracts[dn,3] <- allcontracts[dn,3]*-1
    allcontracts <- data.frame(allcontracts)
    rownames(allcontracts) <- rownames.
    colnames(allcontracts) <- c("Strike", "Last", "Chg", "Bid", "Ask", "Vol", "OI")
  
    call.rows <- which(substr(sprintf("%21s", rownames.),13,13) == "C")
    list(allcontracts[call.rows,], allcontracts[-call.rows,])
  }

  if(missing(Exp))
    opt <- readLines(paste(paste("http://finance.yahoo.com/q/op?s",Symbols,sep="="),"Options",sep="+"), warn=FALSE)
  else
    opt <- readLines(paste(paste("http://finance.yahoo.com/q/op?s=",Symbols,"&m=",parse.expiry(Exp),sep=""),"Options",sep="+"), warn=FALSE)
  opt <- opt[grep("Expire at",opt)]
  opt <- gsub("%5E","",opt)

  if(!missing(Exp) && is.null(Exp)) {
    ViewByExp <- grep("View By Expiration",strsplit(opt, "<tr.*?>")[[1]])
    allExp <- substr(strsplit(strsplit(opt,"<tr.*?>")[[1]][ViewByExp],"m=")[[1]][-1],0,7)
    # fix for missing current month in links
    # allExp <- c(format(as.yearmon(allExp[1]) - 1/12, "%Y-%m"), allExp)

    return(structure(lapply(allExp, getOptionChain.yahoo, Symbols=Symbols), .Names=format(as.yearmon(allExp))))
  }
  calls_puts <- parseOptionTable_(opt)
  list(calls=calls_puts[[1]],puts=calls_puts[[2]],symbol=Symbols)
}

