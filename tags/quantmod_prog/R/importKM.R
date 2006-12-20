"importKM" <- 
function(tR.model) {
  nf <- source(as.character(paste(tR.model@model.id,".R",sep='')))$value;
  tR.model <- updateModel(nf,tR.model);
  return(tR.model);
}



