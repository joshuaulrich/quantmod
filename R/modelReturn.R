"modelReturn" <- function(tR.results,trade.dates=NULL,ret.type='months', leverage=1,exclude.training=TRUE)
{
  tR.return <- new("tR.return");

  trade.signal <- tR.results@signal;
  trade.start <- start(trade.signal);
  trade.end <- end(trade.signal);
  holding.period <- trade.end - trade.start;
  days.tradeable <- length(trade.signal);
  days.traded <- sum(abs(trade.signal),na.rm=TRUE);
  trade.percentage <- days.traded / days.tradeable;
  
#  model.data <- modelData(tR.results@model,trade.dates,exclude.training=exclude.training)
  model.index <- index(trade.signal);
  model.results <- trade.signal[,1] * leverage * trade.signal[,2];
  model.results[which(is.na(model.results))] <- 0;
  model.cumret <- cumprod(1+model.results);
    signal.accuracy <- length(model.results[as.numeric(model.results) > 0])/length(model.results);
    pos.days <- trade.signal[trade.signal[,1] > 0]
    neg.days <- trade.signal[trade.signal[,1] < 0]
    
    if(NROW(pos.days) == 0 || NROW(neg.days) == 0) {
        warning("Model results are all one direction.")
    } else {
        pos.days.accuracy <- sum(ifelse(pos.days[,1]*pos.days[,2] > 0, 1, 0))/NROW(pos.days)
        neg.days.accuracy <- sum(ifelse(neg.days[,1]*neg.days[,2] > 0, 1, 0))/NROW(neg.days)

        pos.days.results <- pos.days[,1]*pos.days[,2]
        neg.days.results <- neg.days[,1]*neg.days[,2]

        raw.signal.bias <- mean(trade.signal[,2])
    }

  CAGR <- as.numeric((model.cumret[trade.end])^(1/(as.numeric(holding.period)/252))-1);
  HPR <- as.numeric(model.cumret[length(model.cumret)])-1;
  accuracy <- zoo(NULL,model.index);
  

    periods <- match.arg(ret.type,c("weeks","months","quarters","years"),several.ok=TRUE)
#  period.options <- c("weeks","months","quarters","years");
#  periods <- period.options[pmatch(ret.type,period.options)];

  returnsBy <- merge(model.cumret,model.results);
  for(i in 1:length(periods))
  {
    this.period <- periods[i];
    this.bp <- breakpoints(trade.signal,by=this.period,TRUE);
    accuracy <- merge(accuracy,zoo(papply(x=model.results,INDEX=this.bp,function(x) {
                        length(x[as.numeric(x) > 0])/length(x)
                        }),model.index[this.bp]));
    returnsBy <- merge(returnsBy,returnBy(x=model.results,dat=trade.signal,by=this.period)[,2]);
  }
  colnames(returnsBy) <- c("cum.return","days",periods);
  if(NCOL(accuracy)>1) colnames(accuracy) <- periods;
  tR.return@returnsBy <- returnsBy;  
  
  tR.return@dist.of.returns <- lapply(as.data.frame(returnsBy), function(x) as.numeric(summary(x))[1:6])
  tR.return@results <- model.results;
  tR.return@returns <- model.cumret;
#  tR.return@CAGR <- sprintf("%.4f%%", CAGR*100);
#  tR.return@HPR <- sprintf("%.2f%%",HPR*100);
  tR.return@CAGR <- CAGR
  tR.return@HPR <- HPR
  tR.return@accuracy <- accuracy
#  tR.return@accuracy <- accuracy;
  return(tR.return);

}

