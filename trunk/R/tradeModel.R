"tradeModel" <- function(tR.model,
                         signal.threshold=c(0,0),
                         leverage=1,
                         return.model=TRUE,
                         plot.model=FALSE,
                         trade.dates=NULL,
                         exclude.training=TRUE,
                         ret.type=c('weeks','months','quarters','years'),...)
{
  trade.offset = 0;
  tR.model <- getModelData(tR.model);
  if(class(tR.model) != "tR.model") stop("model must be of class tR.model");
  if(!is.null(trade.dates) & length(trade.dates) < 2) stop("trade.dates must be of length 2");
  model.data <- modelData(tR.model,trade.dates,exclude.training=exclude.training);
  fitted.zoo <- predictModel(tR.model@fitted.model,model.data,...)
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

  tR.results <- new("tR.results", model=tR.model, signal=signal.zoo);

  model.returns <- modelReturn(tR.results,trade.dates=trade.dates,leverage=leverage,ret.type=ret.type);
  tR.results@return <- model.returns;

  # strip data to minimize memory consumption
  tR.results@model <- stripModelData(tR.results@model);
 return(tR.results);
}
