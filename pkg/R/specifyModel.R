`specifyModel2` <-
function(formula, na.rm = TRUE) {
  nq <- new('quantmod')
  formula <- as.formula(formula)
  
  if(length(formula) < 3) stop('formula must have a lhs')
  
  nq@model.spec <- formula #original specification on the cli

  nq@model.formula <- model.formula(formula)     #parsed formula to remove illegal chars
  
  nq@model.target  <- as.character(nq@model.formula[[2]])
  nq@build.inputs  <- as.character(attr(terms(nq@model.formula), 'term.labels'))
  nq@symbols <- all.vars(formula)
  nq@product <- all.vars(formula)[1]
  nq@model.data <- as.zoo(model.data(formula))
  return(nq)
}


`model.data` <- function(x) {
  # create a data.frame for use in statistical function calls
  # this creates the data.frame that will be passable
  # as a 'data' argument to most function calls
  dat <- sapply(attr(terms(x), 'variables')[-1], eval)
  colnames(dat) <- make.names(attr(terms(x), 'variables'))[-1]
  rownames(dat) <- rownames(get(all.vars(x)[1]))
  as.data.frame(dat)
}

`model.formula` <- function(x) {
  Terms <- rownames(attr(terms(x), 'factors'))
  escape <- function(ff) {
      ff <- gsub('(\\()','\\\\(',ff)
      ff <- gsub('(\\))','\\\\)',ff)
      ff <- gsub('(\\[)','\\\\[',ff)
      gsub('(\\])','\\\\]',ff)
  }
  for(i in 1:length(Terms)) {
    x <- eval(parse(text=gsub(escape(Terms[i]), make.names(Terms[i]), deparse(x))))
  }
  x

}

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
