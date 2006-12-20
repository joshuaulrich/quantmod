"updateModel" <-
function(fitted.model,tR.model) {
   tR.model@model.inputs <- fitted.model@model.inputs;
   tR.model@fitted.model <- fitted.model;
   return(tR.model);
}

