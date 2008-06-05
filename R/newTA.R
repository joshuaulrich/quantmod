`newTA` <- function(FUN, tFUN, on=NA,
                    legend.name, fdots=TRUE, cdots=TRUE, data.at=1, ...) {
  if(is.character(FUN)) {
    if(exists(FUN) && is.function(get(FUN))) {
      FUN.name <- FUN
      FUN <- get(FUN)
    }
  } else
  if(is.function(FUN)) {
    FUN.name <- deparse(substitute(FUN))
  } else stop('FUN required to be a function object')

  # create a text string of the function for inclusion in .body
  funToFun <- function (x, fun.name, drop.arg = 1, dots=TRUE) 
  {
      drop.arg <- if (any(drop.arg < 1)) {
          1:length(formals(x))
      }
      else -drop.arg
      fnames <- names(formals(x))
      if( !dots && ('...' %in% fnames) )
        fnames <- fnames[-which('...' == fnames)]

      fun.args <- paste(fnames, "=", 
          c('x',fnames[drop.arg]), sep = "")
      fun.args <- paste(gsub("=\\.\\.\\.", "", fun.args), collapse = ",")
      paste(fun.name, "(", fun.args, ")", 
          collapse = "", sep = "")
  }

  .formals <- formals(FUN)[-data.at]
  .body <- deparse(body(skeleton.TA))
  gpars <- list(...)

  # add ability to customize legend.name, still retaining legend color/last value
  if(!missing(legend.name) && is.character(legend.name)) {
    .body[21] <- paste('legend.name <-',deparse(legend.name))
  }  

  # cdots: should the newTA object have a ... arg?
  # if the function uses, the call must too
  if(missing(fdots) && !('...' %in% .formals))
    fdots <- FALSE
  if(fdots) cdots <- TRUE
  if(!cdots) {
    .formals <- .formals[-which('...' == names(.formals))]
    .body[21] <- paste("gpars <-",list(gpars))
  } else {
    if(!'...' %in% names(.formals)) {
      .formals <- c(.formals,alist(...=))
    }
    .body[21] <- paste('gpars <- c(list(...),', list(gpars),
                       ')[unique(names(c(',list(gpars),',list(...))))]')
  }

  .formals <- eval(parse(text=paste('c(.formals,alist(on=',on,', legend="auto"))')))

  if(!missing(tFUN)) {
    if(is.character(tFUN)) {
      if(exists(tFUN) && is.function(get(tFUN))) {
        tFUN <- tFUN
      }
    } else
    if(is.function(tFUN)) {
      tFUN <- deparse(substitute(tFUN))
    } else stop('tFun required to be a function object')
    # add tranform Function to .body
    .body[4] <- paste("x <-",tFUN,"(x)",sep="")
  # if missing, assume no transform need to be done
  } else .body[4] <- 'FP <- ""'

  # fdots: should the underlying function call use ...
  .body[5] <- paste("x <-",funToFun(FUN,FUN.name,data.at, dots=fdots))
  as.function(c(.formals,as.call(parse(text=.body))[[1]]),.GlobalEnv) 
}

`skeleton.TA` <- function(on)
{
    lchob <- quantmod:::get.current.chob()
    x <- as.matrix(lchob@xdata)
    TDP <- "TRANSFORM DATA PLACEHOLDER"
    FP  <- "FUNCTION PLACEHOLDER"
    chobTA <- new("chobTA")
    if(NCOL(x) == 1) {
    chobTA@TA.values <- x[lchob@xsubset]
    } else chobTA@TA.values <- x[lchob@xsubset,]
    chobTA@name <- "chartTA"
    if(is.na(on)) {
    chobTA@new <- TRUE
    }
    else {
      chobTA@new <- FALSE
      chobTA@on  <- on
    }
    chobTA@call <- match.call()
    legend.name <- gsub('^add','',deparse(match.call()))
    gpars <- list()
    chobTA@params <- list(xrange = lchob@xrange, colors = lchob@colors, 
        color.vol = lchob@color.vol, multi.col = lchob@multi.col, 
        spacing = lchob@spacing, width = lchob@width, bp = lchob@bp, 
        x.labels = lchob@x.labels, time.scale = lchob@time.scale,
        legend = legend, legend.name = legend.name, pars = list(gpars)) 
    if (is.null(sys.call(-1))) {
        TA <- lchob@passed.args$TA
        lchob@passed.args$TA <- c(TA, chobTA)
        lchob@windows <- lchob@windows + ifelse(chobTA@new, 1, 
            0)
        chartSeries.chob <- quantmod:::chartSeries.chob
        do.call('chartSeries.chob',list(lchob))
        invisible(chobTA)
    }
    else {
        return(chobTA)
    }
}

