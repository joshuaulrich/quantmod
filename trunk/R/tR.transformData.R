"tR.transformData" <-
function(transform,data,...) {
  expr <- parse(text=transform);
  expr[[1]][[1]] <- parse(text=eval.parent(substitute(paste("tR.",expr[[1]][[1]],sep=''))))[[1]];
  expr[[1]][[2]] <- parse(text=eval.parent(substitute(paste("symbol='",deparse(expr[[1]][[2]]),"'",sep=''))))[[1]];
  expr[[1]][[length(expr[[1]])+1]] <- parse(text=eval.parent(substitute(paste("data=data",sep=''))))[[1]];
  expr <- eval(expr);
  return(expr);
}

