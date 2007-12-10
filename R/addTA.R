# chartVo {{{
`chartVo` <-
function(x) {
  # if volume is to be plotted, do so here
    # scale volume - vol.divisor
    if(class(x) != "chobTA") stop("chartVo requires a suitable chobTA object")
    Opens <- x@TA.values[,1]
    Closes <- x@TA.values[,2]
    Volumes <- x@TA.values[,3]

    spacing <- x@params$spacing
    width <- x@params$width

    x.range <- x@params$xrange
    x.range <- seq(x.range[1],x.range[2]*spacing)

    multi.col <- x@params$multi.col
    color.vol <- x@params$color.vol

    vol.scale <- x@params$vol.scale
    #par(mar=c(2,4,0,3))
    plot(x.range,seq(min(Volumes),max(Volumes),
         length.out=length(x.range)),
         type='n',axes=FALSE,ann=FALSE)
    grid(NA,NULL,col="#333333")
    bar.col <- x@params$colors$fg.col
    for(i in 1:length(Volumes)) {
      Vols <- c(0,Volumes[i])
      x.pos <- 1+spacing*(i-1)
      if(x@params$color.vol) {
        dn.up.col <- x@params$colors$dn.up.col
        up.up.col <- x@params$colors$up.up.col
        dn.dn.col <- x@params$colors$dn.dn.col
        up.dn.col <- x@params$colors$up.dn.col
        up.col <- x@params$colors$up.col
        dn.col <- x@params$colors$dn.col

        if(i > 1 & multi.col & color.vol) {
          if(Opens[i] < Closes[i] & Opens[i] < Closes[i-1]) bar.col <- dn.up.col
          if(Opens[i] < Closes[i] & Opens[i] > Closes[i-1]) bar.col <- up.up.col
          if(Opens[i] > Closes[i] & Opens[i] < Closes[i-1]) bar.col <- dn.dn.col
          if(Opens[i] > Closes[i] & Opens[i] > Closes[i-1]) bar.col <- up.dn.col
        } else {
          bar.col <- ifelse(Opens[i] > Closes[i],dn.col,up.col)
        }
      }
      #if(color.vol) 
      #  bar.col <- ifelse(Opens[i] > Closes[i],dn.col,up.col)
      #lines(c(x.pos,x.pos),Vols,lwd=width,col=bar.col)
      border.col <- ifelse(x@params$multi.col,"#000000",bar.col)
      rect(x.pos-spacing/4,0,x.pos+spacing/4,Vols[2],col=bar.col,border=border.col)
    }
    title(ylab=paste("volume (",vol.scale[[2]],")"))
    #title(ylab=paste("volume (",vol.scale[[2]],")"),
    #      xlab=x@params$colors$time.scale,col.lab=x@params$colors$fg.col)
    #axis(1,at=1:length(Volumes)*spacing+1,labels=FALSE,col="#444444")
    #axis(1,at=x@params$bp*spacing+1,labels=x@params$x.labels,las=1)
    axis(2)
    box(col=x@params$colors$fg.col)
} # }}}
# addVo {{{
`addVo` <- function() {
  if(exists('chob',env=sys.frames()[[1]])) {
    if(identical(sys.frames()[[1]],.GlobalEnv)) stop('only chob in GlobalEnv!')
    lchob <- get('chob',env=sys.frames()[[1]])
  } else {
    gchob <- get.chob()
    #protect against NULL device or windows not drawn to yet
    if(dev.cur()==1 || length(gchob) < dev.cur()) {
      # create a default chob to work with: must match requirements of call
      return(invisible(NULL))
    } else {
    #which is the current device; is it in the chob list?
    #current.chob <- which(unlist(sapply(gchob,
    #                             function(x) {
    #                               if(!is.null(x)) x@device==as.numeric(dev.cur())
    #                             })))+1
    current.chob <- which(sapply(gchob,
                                 function(x) {
                                   ifelse(class(x)=="chob" &&
                                   x@device==as.numeric(dev.cur()),TRUE,FALSE)
                                 }))
    if(identical(current.chob,integer(0))) stop("no current plot")
    lchob <- gchob[[current.chob]]
    }
  }
  x <- as.matrix(eval(lchob@passed.args$x))
  Volumes <- x[,5]
  max.vol <- max(Volumes)
  vol.scale <- list(100, "100s")
  if (max.vol > 10000) 
    vol.scale <- list(1000, "1000s")
  if (max.vol > 1e+05) 
    vol.scale <- list(10000, "10,000s")
  if (max.vol > 1e+06) 
    vol.scale <- list(1e+05, "100,000s")
  if (max.vol > 1e+07) 
    vol.scale <- list(1e+06, "millions")

  chobTA <- new("chobTA")
  chobTA@new <- TRUE

  chobTA@TA.values <- cbind(x[,c(1,4)],Volumes/vol.scale[[1]])
  chobTA@name <- "chartVo"
  chobTA@call <- match.call()
  chobTA@params <- list(xrange=lchob@xrange,
                        colors=lchob@colors,
                        color.vol=lchob@color.vol,
                        multi.col=lchob@multi.col,
                        spacing=lchob@spacing,
                        width=lchob@width,
                        bp=lchob@bp,
                        vol.scale=vol.scale,
                        x.labels=lchob@x.labels,
                        time.scale=lchob@time.scale)
  return(chobTA)
} # }}}

# addSMI {{{
`addSMI` <- function(param=c(5,3,3,3),ma.type=c('EMA','EMA','EMA')) {
  if(exists('chob',env=sys.frames()[[1]])) {
    if(identical(sys.frames()[[1]],.GlobalEnv)) stop()
    lchob <- get('chob',env=sys.frames()[[1]])
  } else {
    gchob <- get.chob()
    #protect against NULL device or windows not drawn to yet
    if(dev.cur()==1 || length(gchob) < dev.cur()) stop()
    current.chob <- which(sapply(gchob,
                                 function(x) {
                                   ifelse(class(x)=="chob" &&
                                   x@device==as.numeric(dev.cur()),TRUE,FALSE)
                                 }))
    if(identical(current.chob,integer(0))) stop("no current plot")
    lchob <- gchob[[current.chob]]
  }
  x <- as.matrix(eval(lchob@passed.args$x))
  chobTA <- new("chobTA")
  chobTA@new <- TRUE

  smi <- SMI(cbind(Hi(x),Lo(x),Cl(x)),n=param[1],ma.slow=list(ma.type[1],n=param[2]),
             ma.fast=list(ma.type[2],n=param[3]),ma.sig=list(ma.type[3],n=param[4]))
  chobTA@TA.values <- smi
  chobTA@name <- "chartSMI"
  chobTA@call <- match.call()
  chobTA@params <- list(xrange=lchob@xrange,
                        colors=lchob@colors,
                        color.vol=lchob@color.vol,
                        multi.col=lchob@multi.col,
                        spacing=lchob@spacing,
                        width=lchob@width,
                        bp=lchob@bp,
                        x.labels=lchob@x.labels,
                        time.scale=lchob@time.scale,
                        param=param,ma.type=ma.type)
  return(chobTA)
} #}}}
# chartSMI {{{
`chartSMI` <-
function(x) {
    spacing <- x@params$spacing
    width <- x@params$width

    x.range <- x@params$xrange
    x.range <- seq(x.range[1],x.range[2]*spacing)

    multi.col <- x@params$multi.col
    color.vol <- x@params$color.vol

    param <- x@params$param; ma.type <- x@params$ma.type
    smi <- x@TA.values
    plot(x.range,seq(min(smi[,1]*.975,na.rm=TRUE),max(smi[,1]*1.05,na.rm=TRUE),length.out=length(x.range)),
         type='n',axes=FALSE,ann=FALSE)
    grid(NA,NULL,col="#333333")
    lines(seq(1,length(x.range),by=spacing),smi[,1],col='#0033CC',lwd=2,type='l')
    lines(seq(1,length(x.range),by=spacing),smi[,2],col='#BFCFFF',lwd=1,lty='dotted',type='l')
    title(ylab=paste('SMI(',paste(param,collapse=','),')',sep=''))
    axis(2)
    box(col=x@params$colors$fg.col)
} # }}}

# addBBands {{{
`addBBands` <- function(n=20,ma='SMA',sd=2,on=1) {
  if(exists('chob',env=sys.frames()[[1]])) {
    if(identical(sys.frames()[[1]],.GlobalEnv)) stop()
    lchob <- get('chob',env=sys.frames()[[1]])
  } else {
    gchob <- get.chob()
    #protect against NULL device or windows not drawn to yet
    if(dev.cur()==1 || length(gchob) < dev.cur()) stop()
    current.chob <- which(sapply(gchob,
                                 function(x) {
                                   ifelse(class(x)=="chob" &&
                                   x@device==as.numeric(dev.cur()),TRUE,FALSE)
                                 }))
    if(identical(current.chob,integer(0))) stop("no current plot")
    lchob <- gchob[[current.chob]]
  }
  x <- as.matrix(eval(lchob@passed.args$x))
  chobTA <- new("chobTA")
  chobTA@new <- FALSE


  x <- as.matrix(eval(lchob@passed.args$x))

  bb <- bollingerBands(cbind(Hi(x),Lo(x),Cl(x)),ma=list(ma,n=n),sd=list(FUN='sd',n=sd))
  chobTA@TA.values <- bb
  chobTA@name <- "chartBBands"
  chobTA@call <- match.call()
  chobTA@on <- on
  chobTA@params <- list(xrange=lchob@xrange,
                        colors=lchob@colors,
                        color.vol=lchob@color.vol,
                        multi.col=lchob@multi.col,
                        spacing=lchob@spacing,
                        width=lchob@width,
                        bp=lchob@bp,
                        x.labels=lchob@x.labels,
                        time.scale=lchob@time.scale,
                        n=n,ma=ma,sd=sd)
  return(chobTA)
zo} #}}}
# chartBBands {{{
`chartBBands` <-
function(x) {
    spacing <- x@params$spacing
    width <- x@params$width

    x.range <- x@params$xrange
    x.range <- seq(x.range[1],x.range[2]*spacing)

    multi.col <- x@params$multi.col
    color.vol <- x@params$color.vol

    param <- x@params$param; ma.type <- x@params$ma.type
    bb <- x@TA.values
    lines(seq(1,length(x.range),by=spacing),bb[,1],col='red',lwd=1,lty='dashed')
    lines(seq(1,length(x.range),by=spacing),bb[,3],col='red',lwd=1,lty='dashed')
    lines(seq(1,length(x.range),by=spacing),bb[,2],col='grey',lwd=1,lty='dotted')
} # }}}

# addMA {{{
`addMA` <- function(n=10,wilder=FALSE,on=1,with.col=Cl,overlay=TRUE,col='blue') {
  if(exists('chob',env=sys.frames()[[1]])) {
    if(identical(sys.frames()[[1]],.GlobalEnv)) stop()
    lchob <- get('chob',env=sys.frames()[[1]])
  } else {
    gchob <- get.chob()
    #protect against NULL device or windows not drawn to yet
    if(dev.cur()==1 || length(gchob) < dev.cur()) {
      return(invisible(NULL))
    } else {
    current.chob <- which(sapply(gchob,
                                 function(x) {
                                   ifelse(class(x)=="chob" &&
                                   x@device==as.numeric(dev.cur()),TRUE,FALSE)
                                 }))
    if(identical(current.chob,integer(0))) stop("no current plot")
    lchob <- gchob[[current.chob]]
    }
  }
  chobTA <- new("chobTA")
  chobTA@new <- !overlay

  # get the appropriate data - from the approp. src
  if(on==1) {
    x <- as.matrix(eval(lchob@passed.args$x))
    if(is.function(with.col)) {
      x.tmp <- do.call(with.col,list(x))
    } else x.tmp <- x[,with.col]
  } else {
    # get values from TA...
    which.TA <- which(sapply(lchob@passed.args$TA,function(x) x@new))
    target.TA <- eval(lchob@passed.args$TA[which.TA][on-1])[[1]]
    x <- as.matrix(target.TA@TA.values)
    if(missing(with.col)) {
      warning('missing "with.col" argument')
      invisible(return())
    }
    if(is.function(with.col)) {
      x.tmp <- do.call(with.col,list(x))
    } else x.tmp <- x[,with.col]
  }

  chobTA@TA.values <- x.tmp # single numeric vector
  chobTA@name <- "chartMA"
  chobTA@call <- match.call()
  chobTA@on <- on # used for deciding when to draw...
  chobTA@params <- list(xrange=lchob@xrange,
                        colors=lchob@colors,
                        color.vol=lchob@color.vol,
                        multi.col=lchob@multi.col,
                        spacing=lchob@spacing,
                        width=lchob@width,
                        bp=lchob@bp,
                        x.labels=lchob@x.labels,
                        time.scale=lchob@time.scale,
                        ma.col=col,n=n,wilder=wilder)
  return(chobTA)
} # }}}
# chartMA {{{
`chartMA` <-
function(x) {
  # if volume is to be plotted, do so here
    # scale volume - vol.divisor

    spacing <- x@params$spacing
    width <- x@params$width

    x.range <- x@params$xrange
    x.range <- seq(x.range[1],x.range[2]*spacing)

    multi.col <- x@params$multi.col
    color.vol <- x@params$color.vol

    if(length(x@params$n) < length(x@params$ma.col)) {
      colors <- 3:10
    } else colors <- x@params$ma.col
 
    for(li in 1:length(x@params$n)) {
      ma <- EMA(x@TA.values,n=x@params$n[li],wilder=x@params$wilder)
      if(x@new) {
        par(new=TRUE)
        plot(x.range,seq(min(ma*.975),max(ma*1.05),length.out=length(x.range)),
             type='n',axes=FALSE,ann=FALSE)
        title(ylab=paste('EMA(',paste(x@params$n[li],collapse=','),')',sep=''))
        axis(2)
        box(col=x@params$colors$fg.col)
      }
      lines(seq(1,length(x.range),by=spacing),ma,col=colors[li],lwd=1,type='l')
    }
} # }}}

# addExpiry {{{
`addExpiry` <- function(type='options',lty='dotted',col='blue') {
  if(exists('chob',env=sys.frames()[[1]])) {
    if(identical(sys.frames()[[1]],.GlobalEnv)) stop()
    lchob <- get('chob',env=sys.frames()[[1]])
  } else {
    gchob <- get.chob()
    #protect against NULL device or windows not drawn to yet
    if(dev.cur()==1 || length(gchob) < dev.cur()) {
      return(invisible(NULL))
    } else {
    #which is the current device; is it in the chob list?
    current.chob <- which(sapply(gchob,
                                 function(x) {
                                   ifelse(class(x)=="chob" &&
                                   x@device==as.numeric(dev.cur()),TRUE,FALSE)
                                 }))
    if(identical(current.chob,integer(0))) stop("no current plot")
    lchob <- gchob[[current.chob]]
    }
  }
  chobTA <- new("chobTA")
  chobTA@new <- FALSE

  # get the appropriate data - from the approp. src
  #if(from.fig==1) {
  x <- eval(lchob@passed.args$x)
  if(type=='options') {
    index.of.exp <- options.expiry(x)
  } else index.of.exp <- futures.expiry(x)

  chobTA@TA.values <- index.of.exp # single numeric vector
  chobTA@name <- "chartExpiry"
  chobTA@call <- match.call()
  chobTA@on <- 1
  chobTA@params <- list(xrange=lchob@xrange,
                        colors=lchob@colors,
                        color.vol=lchob@color.vol,
                        multi.col=lchob@multi.col,
                        spacing=lchob@spacing,
                        width=lchob@width,
                        bp=lchob@bp,
                        x.labels=lchob@x.labels,
                        time.scale=lchob@time.scale,
                        col=col,lty=lty)
  return(chobTA)
} # }}}
# chartExpiry {{{
`chartExpiry` <-
function(x) {
    spacing <- x@params$spacing
    width <- x@params$width

    x.range <- x@params$xrange
    x.range <- seq(x.range[1],x.range[2]*spacing)

    multi.col <- x@params$multi.col
    color.vol <- x@params$color.vol

    for(ex in 1:length(x@TA.values)) {
      abline(v=x@TA.values[ex]*spacing,lty=x@params$lty,col=x@params$col)
    }
} # }}}


`EMA` <-
function (x, n = 10, wilder = FALSE) 
{
    x <- as.vector(x)
    NAs <- ifelse(any(is.na(x)),length(which(is.na(x))),0)
    x <- na.omit(x)
    ema <- rep(NA, NROW(x))
    if (wilder) 
        ratio <- 1/n
    else ratio <- 2/(n + 1)
    ema[n] <- mean(x[1:n])
    for (i in (n + 1):NROW(x)) {
        ema[i] <- x[i] * ratio + ema[i - 1] * (1 - ratio)
    }
    return(c(rep(NA,NAs),ema))
}

