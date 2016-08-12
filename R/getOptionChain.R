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
  library("XML")
  library("rjson")
  
  dateToMillis <- function(x)
  {
    as.numeric(x) * 86400000	/ 1000
  }
  
  parse.expiry <- function(x) {
    if (is.null(x))
      return(NULL)
    if (is.character(x))
    {
      x <- as.Date(x)
    }
    if (inherits(x, "Date") || inherits(x, "POSIXt"))
      return(dateToMillis(x))
    return(NULL)
  }
  
  getOptionChainYahoo <- function(sym, Exp)
  {
    url <-
      paste("https://query2.finance.yahoo.com/v7/finance/options/",
            Symbols,
            sep = "")
    if (!missing(Exp)) {
      url <-
        paste(
          "https://query2.finance.yahoo.com/v7/finance/options/",
          Symbols,
          "?date=",
          parse.expiry(Exp),
          sep = ""
        )
    }
    
    opt <- readLines(url)
    opt <- paste(opt, collapse = '')
    json <- fromJSON(opt)
    calls <- json$optionChain$result[[1]]$options[[1]]$calls
    puts <- json$optionChain$result[[1]]$options[[1]]$puts
    price <- json$optionChain$result$quote$regularMarketPrice
    
    return (list(
      calls = chainToDf(calls),
      puts = chainToDf(puts),
      price = price,
      sym = sym
    ))
  }
  
  chainToDf <- function(theRawList)
  {
    theList <- list()
    for (i in 1:length(theRawList))
    {
      xx <-
        list(
          contractSymbol = theRawList[[i]]$contractSymbol,
          strike = theRawList[[i]]$strike,
          bid = theRawList[[i]]$bid,
          ask = theRawList[[i]]$ask,
          lastPrice = theRawList[[i]]$lastPrice,
          volume = theRawList[[i]]$volume,
          openInterest = theRawList[[i]]$openInterest
        )
      theList[[i]] <- xx
    }
    x <- do.call(rbind.data.frame, theList)
    rownames(x) <- x$contractSymbol
    y <-
      x[, c("strike",
            "bid",
            "ask",
            "lastPrice",
            "volume",
            "openInterest")]
    theNames <- c("Strike", "Bid", "Ask", "Last", "Vol", "OI")
    names(y) <- theNames
    for (i in theNames)
    {
      y[, i] <- as.numeric(as.character(y[, i]))
    }
    return(y)
  }
  
  getOptionChainYahoo(Symbols, Exp)
}

