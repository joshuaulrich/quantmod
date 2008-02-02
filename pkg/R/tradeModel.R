"tradeModel" <- function(x,
                         signal.threshold=c(0,0),
                         leverage=1,
                         return.model=TRUE,
                         plot.model=FALSE,
                         trade.dates=NULL,
                         exclude.training=TRUE,
                         ret.type=c('weeks','months','quarters','years'),...)
{
  trade.offset = 0;
  quantmod <- getModelData(x);
  if(class(quantmod) != "quantmod") stop("model must be of class quantmod");
  if(!is.null(trade.dates) & length(trade.dates) < 2) stop("trade.dates must be of length 2");
  model.data <- modelData(quantmod,trade.dates,exclude.training=exclude.training);
  fitted.zoo <- predictModel(quantmod@fitted.model,model.data,...)
  if(class(fitted.zoo) != "zoo") {
    fitted.zoo <- zoo(as.vector(fitted.zoo),index(model.data));
  }
  # trade Rule section

  #on open
  signal.zoo <- ifelse(fitted.zoo < signal.threshold[1] |
                       fitted.zoo > signal.threshold[2],
                       ifelse(fitted.zoo > 0,1,-1), 0); 
  tmp.index <- index(signal.zoo)[-(1+trade.offset)];
  market.zoo <- model.data[-(nrow(model.data)+trade.offset),1]
  signal.zoo <- signal.zoo[-c(length(index(signal.zoo))-trade.offset,length(index(signal.zoo)))];
  signal.zoo = merge(market.zoo,signal.zoo)
  index(signal.zoo) <- tmp.index;

  quantmodResults <- new("quantmodResults", model=quantmod, signal=signal.zoo);

  model.returns <- modelReturn(quantmodResults,trade.dates=trade.dates,leverage=leverage,ret.type=ret.type);
  quantmodResults@return <- model.returns;

  # strip data to minimize memory consumption
  quantmodResults@model <- stripModelData(quantmodResults@model);
 return(quantmodResults);
}
