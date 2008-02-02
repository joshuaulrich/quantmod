"updateModel" <-
function(fitted.model,quantmod) {
   quantmod@model.inputs <- fitted.model@model.inputs;
   quantmod@fitted.model <- fitted.model;
   return(quantmod);
}

