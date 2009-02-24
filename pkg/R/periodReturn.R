cumReturn <- function(x, ...) {
  first.value <- as.numeric(x[1])
  (x - first.value)/first.value
}

`periodReturn` <-
function(x,period='monthly',subset=NULL,type='arithmetic',leading=TRUE,...) {
  xx <- try.xts(x)

  # currently there is a bug in ts conversions, just use 'xts'
  if(inherits(x,'ts')) {
    x <- na.omit(try.xts(x))
    xtsAttributes(x) <- CLASS(x) <- NULL
    xx <- x 
    TS <- TRUE
  } else TS <- FALSE

  if(has.Op(xx) & has.Cl(xx)) {
    getFirst <- function(X) Op(X)
    getLast  <- function(X) Cl(X)
  } else getFirst <- getLast <- function(X) X[,1]

  FUN = eval(parse(text=paste('xts::to',period,sep='.'))) 
  on.opts <- list(daily='days',
                  weekly='weeks',
                  monthly='months',
                  quarterly='quarters',
                  yearly='years',
                  annually='years')
  ep <- endpoints(xx, on=on.opts[[period]])
  ret <- Delt(Cl(FUN(x,...)),type=type)

  if(leading) {
    firstval <- as.numeric(Delt(getFirst(xx[1]),getLast(xx[ep[2]]),type=type))
    ret[1,1] <- firstval
  }

  colnames(ret) <- paste(period,'returns',sep='.')
  if(TS) xx <- 1  # make sure reclass doesn't do anything!
  tmp.ret <- reclass(ret,xx[ep[-1]])
  if(is.null(subset)) subset <- '/'
  reclass(as.xts(tmp.ret)[subset])
}

`periodReturn0` <-
function(x,period='monthly',subset=NULL,type='arithmetic',...) {
  xx <- x
  if(is.null(subset)) subset <- '::'

  FUN = eval(parse(text=paste('xts::to',period,sep='.'))) 

  x <- FUN(x, ...)

  # get key attributes for later rebuilding
  x <- as.xts(x)
  .originalCLASS <- CLASS(x)
  .originalAttr <- xtsAttributes(x)
  .originalIndexClass <- indexClass(x)

  x <- Delt(Cl(x),type=type)

  colnames(x) <- paste(period,'returns',sep='.')
  x <- as.xts(x)[subset]

  # replace attributes lost to Delt fun and reclass
  CLASS(x) <- .originalCLASS
  xtsAttributes(x) <- .originalAttr
  indexClass(x) <- .originalIndexClass
  reclass(x)
}

`dailyReturn` <-
function(x,subset=NULL,type='arithmetic',leading=TRUE,...) {
  periodReturn(x,'daily',subset,type,leading,...)
}

`monthlyReturn` <-
function(x,subset=NULL,type='arithmetic',leading=TRUE,...) {
  periodReturn(x,'monthly',subset,type,leading,...)
}

`weeklyReturn` <-
function(x,subset=NULL,type='arithmetic',leading=TRUE,...) {
  periodReturn(x,'weekly',subset,type,leading,...)
}

`quarterlyReturn` <-
function(x,subset=NULL,type='arithmetic',leading=TRUE,...) {
  periodReturn(x,'quarterly',subset,type,leading,...)
}

`yearlyReturn` <-
function(x,subset=NULL,type='arithmetic',leading=TRUE,...) {
  periodReturn(x,'yearly',subset,type,leading,...)
}

`annualReturn` <- yearlyReturn

`allReturns` <-
function(x,subset=NULL,type='arithmetic',leading=TRUE) {
  all.ret <- cbind(
    periodReturn(x,'daily',type=type,leading=FALSE),
    periodReturn(x,'weekly',type=type),
    periodReturn(x,'monthly',type=type,indexAt='endof'),
    periodReturn(x,'quarterly',type=type,indexAt='endof'),
    periodReturn(x,'yearly',type=type)
  )
  colnames(all.ret) <- c('daily','weekly','monthly','quarterly','yearly')
  all.ret
}
