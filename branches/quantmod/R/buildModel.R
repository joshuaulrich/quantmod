"buildModel" <-
function(tR.model,training.per,method,...) {
    model.id=deparse(substitute(tR.model))
    if(length(training.per) != 2) stop("training.per must be of length 2");
    model.data <- tR.model@model.data;
    start.date.index <- index(model.data[which(index(model.data) >= as.Date(training.per[1]))])
    end.date.index <- index(model.data[which(index(model.data) <= as.Date(training.per[2]))])
    method <- as.character(paste("tR.",method,sep=''));
    training.data <- model.data[intersect(start.date.index,end.date.index)];
    formula <- tR.model@model.formula
    mcall <- do.call(method,list(tR.model=tR.model,training.data=training.data, ...));
    tR.model@fitted.model <- mcall$fitted;
    tR.model@model.inputs <- as.character(mcall$inputs);
    tR.model@build.date = as.Date(Sys.time());
    tR.model@model.id <- paste(class(mcall$fitted)[length(class(mcall$fitted))],
                               as.numeric(Sys.time()),sep='');
    tR.model@training.data <- intersect(start.date.index,end.date.index);
    invisible(tR.model);
}
