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
  if(!requireNamespace("XML", quietly=TRUE))
    stop("package:",dQuote("XML"),"cannot be loaded.")

  thParse <- function(x) {
    if (length(XML::xmlChildren(x)) > 1) {
      XML::xmlValue(x[["div"]][["div"]])
    } else {
      XML::xmlValue(x)
    }
  }
  NewToOld <- function(x) {
    # clean up colnames, in case there's weirdness in the HTML
    x <- setNames(x, make.names(names(x)))
    # set cleaned up colnames to current output colnames
    d <- with(x, data.frame(Strike=strike, Last=last, Chg=change,
      Bid=bid, Ask=ask, Vol=volume, OI=openinterest,
      row.names=`contractname`, stringsAsFactors=FALSE))
    d[] <- lapply(d, type.convert, as.is=TRUE)
    d
  }
  cleanNames <- function(x) {
    tolower(gsub("[[:space:]]", "", x))
  }

  # Don't check the expiry date if we're looping over dates we just scraped
  checkExp <- !hasArg(".expiry.known") || !match.call(expand.dots=TRUE)$.expiry.known
  # Construct URL
  urlExp <- paste0("http://finance.yahoo.com/q/op?s=", Symbols[1])
  # Add expiry date to URL
  if(!checkExp)
    urlExp <- paste0(urlExp, "&date=", Exp)

  # Fetch data; ensure object is free'd on function exit
  tbl <- XML::htmlParse(urlExp, isURL=TRUE)
  on.exit(XML::free(tbl))

  # xpaths to the data we're interested in
  xpaths <- list()
  xpaths$tables <- "//table[contains(@class, 'quote-table')]"
  xpaths$table.names <- paste0(xpaths$tables, "/caption/text()")
  xpaths$headers <- paste0(xpaths$tables, "/thead/tr[not(contains(@class, 'filterRangeRow'))]")
  xpaths$expiries <- "//div[contains(@class, 'options_menu')]/form/select//option"

  # Extract table names and headers
  table.names <- XML::xpathSApply(tbl, xpaths$table.names, XML::xmlValue)
  table.names <- cleanNames(table.names)
  table.headers <- XML::xpathApply(tbl, xpaths$headers, fun=function(x) sapply(x['th'], thParse))
  table.headers <- lapply(table.headers, cleanNames)

  # Only return nearest expiry (default served by Yahoo Finance), unless the user specified Exp
  if(!missing(Exp) && checkExp) {
    all.expiries <- XML::xpathSApply(tbl, xpaths$expiries, XML::xmlGetAttr, name="value")
    all.expiries.posix <- .POSIXct(as.numeric(all.expiries), tz="UTC")

    if(is.null(Exp)) {
      # Return all expiries if Exp = NULL
      out <- lapply(all.expiries, getOptionChain.yahoo, Symbols=Symbols, .expiry.known=TRUE)
      # Expiry format was "%b %Y", but that's not unique with weeklies. Change
      # format to "%b.%d.%Y" ("%Y-%m-%d wouldn't be good, since names should
      # start with a letter or dot--naming things is hard).
      return(setNames(out, format(all.expiries.posix, "%b.%d.%Y")))
    } else {
      # Ensure data exist for user-provided expiry date(s)
      if(inherits(Exp, "Date"))
        valid.expiries <- as.Date(all.expiries.posix) %in% Exp
      else if(inherits(Exp, "POSIXt"))
        valid.expiries <- all.expiries.posix %in% Exp
      else if(is.character(Exp)) {
        expiry.range <- range(unlist(lapply(Exp, .parseISO8601, tz="UTC")))
        valid.expiries <- all.expiries.posix >= expiry.range[1] &
                          all.expiries.posix <= expiry.range[2]
      }
      if(all(!valid.expiries))
        stop("Provided expiry date(s) not found. Available dates are: ",
             paste(as.Date(all.expiries.posix), collapse=", "))

      expiry.subset <- all.expiries[valid.expiries]
      if(length(expiry.subset) == 1)
        return(getOptionChain.yahoo(Symbols, expiry.subset, .expiry.known=TRUE))
      else {
        out <- lapply(expiry.subset, getOptionChain.yahoo, Symbols=Symbols, .expiry.known=TRUE)
        # See comment above regarding the output names
        return(setNames(out, format(all.expiries.posix[valid.expiries], "%b.%d.%Y")))
      }
    }
  }

  dftables <- XML::xmlApply(XML::getNodeSet(tbl, xpaths$tables), XML::readHTMLTable, stringsAsFactors=FALSE)
  names(dftables) <- table.names

  dftables <- mapply(setNames, dftables, table.headers, SIMPLIFY=FALSE)
  dftables <- lapply(dftables, NewToOld)
  dftables
}

