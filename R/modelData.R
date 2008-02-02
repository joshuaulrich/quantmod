"modelData" <- function(x,data.window=NULL,exclude.training=FALSE)
{
  model.data <- x@model.data;
  if(!is.null(data.window))
  {
    if(length(data.window) > 2) {
      model.data <- model.data[index(model.data) %in% data.window]; 
    } else {
      start.date.index <- index(model.data[which(index(model.data) >= as.Date(data.window[1],origin='1970-01-01'))])
      end.date.index <- index(model.data[which(index(model.data) <= as.Date(data.window[2],origin='1970-01-01'))])
      date.range <- as.Date(intersect(start.date.index,end.date.index),origin='1970-01-01')
      model.data <- model.data[date.range]
    }
  }
  if(exclude.training == TRUE)
  {
    model.data <- model.data[!index(model.data) %in% x@training.data];
  } 
  return(model.data);
} 
