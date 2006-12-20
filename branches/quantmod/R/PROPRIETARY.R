#####################################################################
##
##
## This file is property of Jeff Ryan - jeff.a.ryan@gmail.com
##
## All rights reserved.  Any copying is strictly prohibited.
##
##
##
#####################################################################

"tR.kmgmdh" <-
function(tR.model,training.data, ...) {
  model.id <- paste("km-",as.numeric(Sys.time()),sep='')
  model.id <- paste(as.character(model.id),".dat",sep='');
  core <- cbind(as.Date(index(training.data)),coredata(training.data))
  colnames(core) <- c("date",colnames(core)[-1]);
  write.table(core,sep="\t",file=model.id,row.names=FALSE);
}
  
setClass("kmgmdh",representation(
              model.inputs="character",
              model.formula="character"
              )
        );

"predict.kmgmdh" <-
function(object,model.data,...)
{
	model.fun <- eval(parse(text=object@model.formula));
	nf <- model.fun(model.data);
}
