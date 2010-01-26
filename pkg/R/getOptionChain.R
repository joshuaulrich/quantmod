`getOptionChain` <-
function(Symbols, Exp=NULL, src="yahoo", ...) {
  Call <- paste("getOptionChain",src,sep=".")
  do.call(Call, list(Symbols=Symbols, Exp=Exp, ...))
}

`getOptionChain.yahoo` <-
function(Symbols, Exp="2009-05", ...) {
  parse.expiry <- function(x) {
    if(nchar(x)==5L) {
      #MonYR
      x <- sprintf(substring(x,4,5),match(substring(x, 1, 3),month.abb),fmt="20%s-%02i")
    } else
    if(nchar(x)==6L) {
      #CCYY-MM or CCYYMM
      x <- paste(substring(x,1,4),substring(x,5,6),sep="-")
    }
    return(x)
  } 
  if(is.null(Exp)) {
    opts <- try(readLines(paste("http://finance.yahoo.com/q/op?s=",
                           Symbols,"&m=",Exp,sep="")), silent=TRUE)
    if(inherits(opts, "try-error"))
      return(NULL)
    Exp <- unlist(strsplit(gsub("(CALL OPTIONS)| ","",gsub("<.*?>","",opts[grep("View By Expiration",opts)+1],perl=TRUE)),"\\|"))
  }
  if(length(Exp) > 1) {
    opt.list <- list()
    for(i in 1:length(Exp)) {
      opt.list <- c(opt.list, list(getOptionChain(Symbols, parse.expiry(Exp[i]))))
    }
    names(opt.list) <- Exp
    return(opt.list)
  }
  opt <- try(readLines(paste("http://finance.yahoo.com/q/op?s=",
                         Symbols,"&m=",parse.expiry(Exp),sep="")), silent=TRUE)
  if(inherits(opt, "try-error"))
    return(NULL)
  extag <- grep("Expire at", opt) + 1
  neg.call.chg <- which(sapply(strsplit(opt[extag[1]],"color:#"),substring,1,6)[-1] == "cc0000")
  calls <- strsplit(gsub("<.*?>","---",opt[extag[1]],perl=TRUE),"---")[[1]] 

  neg.put.chg <- which(sapply(strsplit(opt[extag[2]],"color:#"),substring,1,6)[-1] == "cc0000")
  puts  <- strsplit(gsub("<.*?>","---",opt[extag[2]],perl=TRUE),"---")[[1]] 

  call.len <- sum(nchar(calls) > 10) #length(grep("X$", calls, perl=TRUE))
  if(call.len > 0) {
    calls <- calls[-which(calls=="")][-(1:9)][seq(1,9*call.len)]
    call.mat <- matrix(calls,byrow=TRUE,nc=9)[,-4]
    call.symbols <- call.mat[,2]
    call.symbols <- call.symbols[-(length(call.symbols)+c(-1,0))]
    call.mat <- call.mat[1:length(call.symbols),-2]
    call.mat <- gsub(",","",call.mat)
    suppressWarnings(storage.mode(call.mat) <- "numeric")
    call.mat[neg.call.chg, 3] <- call.mat[neg.call.chg,3]*-1
    calls <- data.frame(call.mat, row.names=call.symbols)
    colnames(calls) <- c("Strike","Last","Chg","Bid","Ask","Vol","OI")
  } else calls <- NULL
  
  put.len <- sum(nchar(puts) > 10) #length(grep("X$", puts, perl=TRUE))
  if(put.len > 0) {
    puts <- puts[-which(puts=="")][-(1:9)][seq(1,9*put.len)]
    put.mat <- matrix(puts,byrow=TRUE,nc=9)[,-4]
    put.symbols <- put.mat[,2]
    # symbols now contains some additional lines 2 for call, 3 for puts
    # remove here
    put.symbols <- put.symbols[-(length(put.symbols)+c(-2,-1,0))] 
    put.mat <- put.mat[1:length(put.symbols),-2]
    put.mat <- gsub(",","",put.mat)
    suppressWarnings(storage.mode(put.mat) <- "numeric")
    put.mat[neg.put.chg, 3] <- put.mat[neg.put.chg,3]*-1
    puts <- data.frame(put.mat, row.names=put.symbols)
    colnames(puts) <- c("Strike","Last","Chg","Bid","Ask","Vol","OI")
  } else puts <- NULL
  list(calls=calls,puts=puts,symbol=Symbols)
}

