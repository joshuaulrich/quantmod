"is.quantmod" <-
function(x)
{
  (class(x) == 'quantmod')
}
"is.quantmodResults" <-
function(x)
{
  (class(x) == 'quantmodResults')
}

`as.zoo.data.frame`<-
function(x,row.date=TRUE,...)
{
    # Registered S3 method overwritten by 'quantmod':
    #   method            from
    #   as.zoo.data.frame zoo
    send.message <- getOption("quantmod.deprecate.as.zoo.data.frame", NULL)
    if(is.null(send.message)) {
        # Only message once if user hasn't set an option value
        send.message <- TRUE
        options(quantmod.deprecate.as.zoo.data.frame = FALSE)
    }
    if(isTRUE(send.message)) {
        message(
            "\nNOTE: quantmod::as.zoo.data.frame() is deprecated",
            "\n  Use as.zoo(x, order.by = as.Date(rownames(x))) instead.",
            "\n  This note is printed once. To see it for every call, set",
            "\n  options(quantmod.deprecate.as.zoo.data.frame = TRUE)\n")
    }

    # ignore row.date if order.by is specified
    if(hasArg("order.by")) {
        zoo(x,...)
    }
    #really need to test order - ???how?
    else if(row.date) {
        zoo(x,as.Date(rownames(x),origin='1970-01-01'),...)
    }
    else {
        zoo(x,rownames(x),...)
    }
}

`as.zoo.quantmod.OHLC` <-
function(x,...)
{
  class(x) <- 'zoo'
  x
}

`as.quantmod.OHLC`<-
function(x,
         col.names=c('Open','High','Low','Close','Volume','Adjusted'),
         name=NULL,
         ...)
{
    if(ncol(x) != length(col.names))
      stop("'col.names' must match number of columns of 'x'")
    UseMethod("as.quantmod.OHLC")
}

`as.quantmod.OHLC.data.frame`<-
function(x,
         col.names=c('Open','High','Low','Close','Volume','Adjusted'),
         name=NULL,
         ...)
{
  if(is.null(name)) name <- deparse(substitute(x))
  x <- as.zoo(x)
  colnames(x) <- paste(name,'.',col.names,sep='')
  class(x) <- c('quantmod.OHLC','zoo')
  x
}

`as.quantmod.OHLC.quantmod.OHLC` <-
function(x,
         col.names=c('Open','High','Low','Close','Volume','Adjusted'),
         name=NULL,
         ...)
{
  if(is.null(name)) name <- deparse(substitute(x))
  x <- as.zoo(x)
  colnames(x) <- paste(name,'.',col.names,sep='')
  class(x) <- c('quantmod.OHLC','zoo')
  x
}

`as.quantmod.OHLC.zoo` <-
function(x,
         col.names=c('Open','High','Low','Close','Volume','Adjusted'),
         name=NULL,
         ...)
{
  if(is.null(name)) name <- deparse(substitute(x))
  x <- as.zoo(x)
  colnames(x) <- paste(name,'.',col.names,sep='')
  class(x) <- c('quantmod.OHLC','zoo')
  x
}

`[.quantmod.OHLC`<-
function(x,i,j,drop=TRUE,...)
{
  original.cols <- ncol(x)
  original.names <- colnames(x)
  class(x) <- "zoo"
  if(missing(i)) i <- 1:nrow(x)
  if(missing(j)) {
    x <- x[i=i,drop=drop,...]
    class(x) <- c("quantmod.OHLC","zoo")
    j <- 1:original.cols
  } else {
    x <- x[i=i,j=j,drop=drop,...]
    if(is.null(dim(x)))
      dim(x) <- c(NROW(x),NCOL(x))
    if(ncol(x)==original.cols)
      class(x) <- c("quantmod.OHLC","zoo")
  }
  if(!is.null(dim(x)))
    colnames(x) <- original.names[j]
  x
}
