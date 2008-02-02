"modelSignal" <-
function(x)
{
  if(!is.quantmodResults(x)) stop(paste(dQuote("x"),"must be of class",
                                  dQuote("quantmodResults")))
  x@signal
}
