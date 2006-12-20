"modelData" <- function(tR.model,data.window=NULL,exclude.training=FALSE)
{
  model.data <- tR.model@model.data;
  if(!is.null(data.window))
  {
    if(length(data.window) > 2) {
      model.data <- model.data[index(model.data) %in% data.window]; 
    } else {
      start.date.index <- index(model.data[which(index(model.data) >= as.Date(data.window[1]))])
      end.date.index <- index(model.data[which(index(model.data) <= as.Date(data.window[2]))])
      model.data <- model.data[intersect(start.date.index,end.date.index)]
    }
  }
  if(exclude.training == TRUE)
  {
    model.data <- model.data[!index(model.data) %in% tR.model@training.data];
  } 
  return(model.data);
} 
