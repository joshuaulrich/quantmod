`as.zoo.data.frame`<-
function(x,row.date=TRUE,...)
{
    if(row.date) {
        zoo(x,as.Date(rownames(x)),...)
    }
    else {
        zoo(x,rownames(x),...)
    }
}

`as.quantmod.OHLC`<-
function(x,
         #col.names=c('Open','High','Low','Close','Volume','Adjusted'),
         #name=NULL,
         ...)
{
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
  class(x) <- "zoo"
  x <- x[i,j,drop=TRUE,...]
  class(x) <- c("quantmod.OHLC","zoo")
  x
}
