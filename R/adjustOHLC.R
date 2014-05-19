adjustOHLC <-
function(x, 
         adjust=c("split","dividend"), 
         use.Adjusted=FALSE, 
         ratio=NULL, symbol.name=deparse(substitute(x))) 
{
  if(is.null(ratio)) {
    if(use.Adjusted) {
      # infer from Yahoo! Adjusted column
      if(!has.Ad(x))
        stop("no Adjusted column in 'x'")
      ratio <- Ad(x)/Cl(x)
    } else {
      # use actual split and/or dividend data
      div <- getDividends(symbol.name, from="1900-01-01")
      splits <- getSplits(symbol.name, from="1900-01-01")
      # un-adjust dividends for splits (Yahoo already adjusts div for splits)
      if(is.xts(splits) && is.xts(div) && nrow(splits) > 0 && nrow(div) > 0)
        div <- div * 1/adjRatios(splits=merge(splits, index(div)))[,1]
      # calculate adjustment ratios using unadjusted dividends
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

