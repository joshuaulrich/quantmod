"specifyModel" <-
function(formula,na.rm=TRUE) {
  new.quantmod <- new("quantmod");
  formula <- as.formula(formula);
  dot.vars <- all.vars(formula);
    convert.vars <- function(vars) {
        v <- unlist(strsplit(vars,'[.]'));
        v <- paste(v[1],'(',v[2], if(length(v)>2) paste(',',v[3],sep=''),')',sep='');
        return(v);
    }
#    model.vars <- unlist(lapply(dot.vars,convert.vars));
#    model.formula <- paste(model.vars[1],paste(model.vars[-1],collapse=' + '),sep=' ~ ');

  new.quantmod@model.spec <- formula
  new.quantmod@model.formula <- as.formula(gsub("[) ]","",gsub("[(,=:^'\"]",".",deparse(formula))));
  new.quantmod@model.target <- as.character(new.quantmod@model.formula[[2]])
  new.quantmod@build.inputs <- as.character(attr(terms(new.quantmod@model.formula),"term.labels"));
  vars <- all.vars(formula);
  new.quantmod@symbols <- vars;
  new.quantmod@product <- vars[1]; 
  new.quantmod <- getModelData(new.quantmod,na.rm=na.rm);
  return(new.quantmod);
}
"specifyModel.original" <-
function(formula,na.rm=TRUE) {
  new.quantmod <- new("quantmod");
  formula <- as.formula(formula);
  new.quantmod@model.spec <- formula
  new.quantmod@model.formula <- as.formula(gsub("\\)","",gsub("\\(",".",deparse(formula))));
  new.quantmod@model.target <- deparse(formula[[2]]);
  new.quantmod@build.inputs <- as.character(attr(terms(formula),"term.labels"));
  vars <- all.vars(formula);
  new.quantmod@symbols <- vars;
  new.quantmod@product <- vars[1]; 
  new.quantmod <- getModelData(new.quantmod,na.rm=na.rm);
  return(new.quantmod);
}

# used potentially for chartSeries parsing - where a model.frame is unecessary
`unique.formula.names` <-
function(x) {
  tm <- attr(terms(x),'variables')
  name.list <- lapply(tm, function(y) paste(make.names(y),collapse=''))[-1]
  unlist(lapply(name.list, function(x) gsub('\\.+','.',x)))
}

`make.short.names` <-
function(x) {
  gsub('\\.+','.',make.names(colnames(model.frame(x))))
}
