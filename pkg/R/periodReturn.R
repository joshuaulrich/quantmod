`periodReturn` <-
function(x,period='monthly',subset=NULL,type='arithmetic',...) {
  xx <- try.xts(x)
  FUN = eval(parse(text=paste('xts::to',period,sep='.'))) 
  firstval <- as.numeric(Delt(Cl(xx[c(1,endpoints(xx)[2])]),type=type)[2,1])
  ret <- Delt(Cl(FUN(x,...)),type=type)
  ret[1,1] <- firstval
  colnames(ret) <- paste(period,'returns',sep='.')
  tret <- reclass(ret,xx[endpoints(xx)[-1]])
  if(is.null(subset)) subset <- '/'
  reclass(as.xts(tret)[subset])
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
function(x,subset=NULL,type='arithmetic',...) {
  periodReturn(x,'daily',subset,type,...)
}

`monthlyReturn` <-
function(x,subset=NULL,type='arithmetic',...) {
  periodReturn(x,'monthly',subset,type,...)
}

`weeklyReturn` <-
function(x,subset=NULL,type='arithmetic',...) {
  periodReturn(x,'weekly',subset,type,...)
}

`quarterlyReturn` <-
function(x,subset=NULL,type='arithmetic',...) {
  periodReturn(x,'quarterly',subset,type,...)
}

`yearlyReturn` <-
function(x,subset=NULL,type='arithmetic',...) {
  periodReturn(x,'yearly',subset,type,...)
}

`annualReturn` <- yearlyReturn

`allReturns` <-
function(x,subset=NULL,type='arithmetic') {
  all.ret <- cbind.zoo(
    periodReturn(x,'daily',type=type),
    periodReturn(x,'weekly',type=type),
    periodReturn(x,'monthly',type=type,indexAt='endof'),
    periodReturn(x,'quarterly',type=type,indexAt='endof'),
    periodReturn(x,'yearly',type=type)
  )
  colnames(all.ret) <- c('daily','weekly','monthly','quarterly','yearly')
  all.ret
}
