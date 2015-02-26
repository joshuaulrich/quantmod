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
  if(!require(XML, quietly=TRUE))
    stop("package:",dQuote("XML"),"cannot be found.")

  thParse <- function(x) {
    if (length(XML::xmlChildren(x)) > 1) {
      XML::xmlValue(x[["div"]][["div"]])
    } else {
      XML::xmlValue(x)
    }
  }
  NewToOld <- function(x) {
    d <- with(x, data.frame(Strike, Last, Chg=Change, Bid, Ask, Vol=Volume,
      OI=`Open Interest`, row.names=`Contract Name`, stringsAsFactors=FALSE))
    d[] <- lapply(d, type.convert, as.is=TRUE)
    d
  }
  tbl <- XML::htmlParse(paste0("http://finance.yahoo.com/q/op?s=", Symbols[1]), isURL=TRUE)

  xpaths <- list()
  xpaths$tables <- "//table[contains(@class, 'quote-table')]"
  xpaths$table.names <- paste0(xpaths$tables, "/caption/text()")
  xpaths$headers <- paste0(xpaths$tables, "/thead/tr[not(contains(@class, 'filterRangeRow'))]")

  table.names <- XML::xpathSApply(tbl, xpaths$table.names, XML::xmlValue)
  table.names <- tolower(gsub("[[:space:]]", "", table.names))
  table.headers <- XML::xpathApply(tbl, xpaths$headers, fun=function(x) sapply(x['th'], thParse))

  dftables <- XML::xmlApply(XML::getNodeSet(tbl, xpaths$tables), XML::readHTMLTable, stringsAsFactors=FALSE)
  names(dftables) <- table.names

  XML::free(tbl)

  dftables <- mapply(setNames, dftables, table.headers, SIMPLIFY=FALSE)
  dftables <- lapply(dftables, NewToOld)
  dftables
}

