`bM` <-
function(x,FUN,subset,...) {
    if(!is.quantmod(x)) stop('x must be a quantmod object')

    if(is.function(FUN)) {
      FUN <- deparse(substitute(FUN))
    }

    FUN <- as.character(paste("buildModel.",FUN,sep=''))

    training.data <- x@model.data[subset]

    #formula <- x@model.formula

    mcall <- do.call(FUN,list(quantmod=x,training.data=training.data, ...))

    x@fitted.model <- mcall$fitted
    x@model.inputs <- as.character(mcall$inputs)
    x@build.date = as.character(Sys.time())
    x@model.id <- paste(class(mcall$fitted)[length(class(mcall$fitted))],
                               as.numeric(Sys.time()),sep='')
    x@training.data <- subset
    invisible(x)
}

"buildModel" <-
function(x,method,training.per,...) {

    as.POSIXorDate <- function(x) {
      class.of.index <- class(index(model.data))
      if("POSIXt" %in% class.of.index) {
        if("POSIXlt" %in% class.of.index) {
          x <- as.POSIXlt(x)
        } else {
          x <- as.POSIXct(x)
        }
      } else {
        x <- as.Date(x)
      }
      x
    }

    model.id=deparse(substitute(x))
    if(length(training.per) != 2) stop("training.per must be of length 2");
    model.data <- x@model.data;
    start.date.index <- index(model.data[which(index(model.data) >= as.POSIXorDate(training.per[1]))])
    end.date.index <- index(model.data[which(index(model.data) <= as.POSIXorDate(training.per[2]))])
    training.dates <- as.POSIXorDate(intersect(as.character(start.date.index),
                                               as.character(end.date.index)));
    method <- as.character(paste("buildModel.",method,sep=''));
    training.data <- model.data[training.dates];
    formula <- x@model.formula
    mcall <- do.call(method,list(quantmod=x,training.data=training.data, ...));
    x@fitted.model <- mcall$fitted;
    x@model.inputs <- as.character(mcall$inputs);
    x@build.date = as.character(Sys.time());
    x@model.id <- paste(class(mcall$fitted)[length(class(mcall$fitted))],
                               as.numeric(Sys.time()),sep='');
    x@training.data <- (training.dates);
    invisible(x);
}
