setSymbolLookup <- function(...)
{
  new.symbols <- list(...)
  if(length(new.symbols)==1 && is.null(names(new.symbols)) && is.list(new.symbols[[1]])) new.symbols<-new.symbols[[1]]
  all.symbols <- getOption("getSymbols.sources")
  for(each.symbol in names(new.symbols)) {
    if(length(new.symbols[[each.symbol]])==1 &
       !is.list(new.symbols[[each.symbol]])) {
      # if a single value is passed then it
      # is interpreted as 'src', unless
      # it is part of a list, then it is the
      # appropriately named element.

      all.symbols[[each.symbol]] <- list(src=new.symbols[[each.symbol]])
    } else {
      all.symbols[[each.symbol]] <- new.symbols[[each.symbol]]
    }
  }
  options(getSymbols.sources=all.symbols)
}

"setSymbolLookup.bak" <-
function(...)
{
  new.symbols <- list(...)
  all.symbols <- getOption("getSymbols.sources")
  for(each.symbol in names(new.symbols)) {
    all.symbols[[each.symbol]] <- new.symbols[[each.symbol]]
  }
  options(getSymbols.sources=all.symbols)
}

"loadSymbolLookup" <-
function(file,dir="")
{
  if(missing(file)) file <- ".quantmod.SymbolLookup.rda"
  if(dir!="") {
    file <- file.path(dir,file)
  }
  if(file.exists(file)) {
    load(file)
    options(getSymbols.sources=get('lookup.list'))
  } else {
    stop("no SymbolLookup file exists in this directory")
  }
}

"saveSymbolLookup" <-
function(file,dir="")
{
  if(missing(file)) file <- ".quantmod.SymbolLookup.rda"
  if(dir!="") {
    file <- file.path(dir,file)
  }
  lookup.list <- getSymbolLookup()
  save(lookup.list,file=file)
}

"getSymbolLookup" <-
function(Symbols=NULL)
{
  all.symbols <- getOption("getSymbols.sources")
  if(is.null(Symbols)) Symbols <- names(all.symbols)
  all.symbols[Symbols]
}

"unsetSymbolLookup" <-
function(Symbols,confirm=TRUE)
{
}
