"returnBy" <-
function(x,dat,by,...)
{
  bp <- endpoints(dat,by);
  end.value <- period.apply(cumprod(1+x),bp, function(x) x[length(x)]);
  per.change <- diff(c(1,end.value))/c(1,end.value[-length(end.value)]);
  zret <- zoo(cbind(end.value,per.change),index(dat)[bp]);
  change.name <- paste("change.by.",as.character(by),sep='');
  colnames(zret) <- c("cumulative.ret",change.name);
  return(zret);
}

