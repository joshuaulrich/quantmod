"specifyModel" <-
function(formula,na.rm=TRUE) {
  new.tR.model <- new("tR.model");
  formula <- as.formula(formula);
  dot.vars <- all.vars(formula);
    convert.vars <- function(vars) {
        v <- unlist(strsplit(vars,'[.]'));
        v <- paste(v[1],'(',v[2], if(length(v)>2) paste(',',v[3],sep=''),')',sep='');
        return(v);
    }
#    model.vars <- unlist(lapply(dot.vars,convert.vars));
#    model.formula <- paste(model.vars[1],paste(model.vars[-1],collapse=' + '),sep=' ~ ');

  new.tR.model@model.spec <- formula
  new.tR.model@model.formula <- as.formula(gsub("[) ]","",gsub("[(,=:'\"]",".",deparse(formula))));
  new.tR.model@model.target <- as.character(new.tR.model@model.formula[[2]])
  new.tR.model@build.inputs <- as.character(attr(terms(new.tR.model@model.formula),"term.labels"));
  vars <- all.vars(formula);
  new.tR.model@symbols <- vars;
  new.tR.model@product <- vars[1]; 
  new.tR.model <- getModelData(new.tR.model,na.rm=na.rm);
  return(new.tR.model);
}
"specifyModel.original" <-
function(formula,na.rm=TRUE) {
  new.tR.model <- new("tR.model");
  formula <- as.formula(formula);
  new.tR.model@model.spec <- formula
  new.tR.model@model.formula <- as.formula(gsub("\\)","",gsub("\\(",".",deparse(formula))));
  new.tR.model@model.target <- deparse(formula[[2]]);
  new.tR.model@build.inputs <- as.character(attr(terms(formula),"term.labels"));
  vars <- all.vars(formula);
  new.tR.model@symbols <- vars;
  new.tR.model@product <- vars[1]; 
  new.tR.model <- getModelData(new.tR.model,na.rm=na.rm);
  return(new.tR.model);
}

