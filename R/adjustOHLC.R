adjustOHLC <-
function(x, 
         adjust=c("split","dividend"), 
         use.Adjusted=FALSE, 
         ratio=NULL) 
{
  symbol.name <- deparse(substitute(x))

  if(is.null(ratio)) {
    if(use.Adjusted) {
      # infer from Yahoo! Ajusted column
      if(!has.Ad(x))
        stop("no Adjusted column in 'x'")
      ratio <- Ad(x)/Cl(x)
    } else {
      # use actual split and/or dividend data
      div    <- getDividends(symbol.name)
      splits <- getSplits(symbol.name)
      ratios <- adjRatios(splits, div, Cl(x))
      if(length(adjust)==1 && adjust == "split") {
        ratio <- ratios[,1]
      } else if(length(adjust)==1 && adjust == "dividend") {
        ratio <- ratios[,2]
      } else ratio <- ratios[,1] * ratios[,2]
    }
  }
  Adjusted <- Cl(x) * ratio
  structure(
    cbind((ratio * (Op(x)-Cl(x)) + Adjusted),
          (ratio * (Hi(x)-Cl(x)) + Adjusted),
          (ratio * (Lo(x)-Cl(x)) + Adjusted),
          Adjusted,
          if(has.Vo(x)) Vo(x) else NULL,
          if(has.Ad(x)) Ad(x) else NULL
         ),
       .Dimnames=list(NULL, colnames(x)))
}

