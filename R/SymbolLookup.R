"setSymbolLookup" <-
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
function(filename=".quantmod.SymbolSource.RData")
{

}

"saveSymbolLookup" <-
function(filename=".quantmod.SymbolSource.RData")
{

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
