#
#  At present all TA functionality is in this file
#  
#  TA implemented and charting optimized:
#
#    BBands,CCI,CMF,CMO,DPO,EMA,Envelope,MACD,Momentum,
#    RSI,SMA,SMI,Vo,WPR

#  TA implemented, charting not completed/optimized:
#
#    ADX,ATR,DEMA,EVWMA,Expiry,Lines,ROC,SAR,TRIX,WMA,ZLEMA

#  TA not yet implemented (and some may not be)
#
#    CLV,CMD,OBV,KST,TDI,WHF,Aroon,ChAD,ChVol,WilliamsAD,
#    Points, Stoch, SD, ...??? 
# addMomentum {{{
`addMomentum` <- function(n=1) {


  lchob <- get.current.chob()

  x <- as.matrix(lchob@xdata)

  chobTA <- new("chobTA")
  chobTA@new <- TRUE

  #  needs to accept any arguments for x, not just close

  xx <- if(is.OHLC(x)) {
    Cl(x)
  } else x 

  mom <- momentum(xx,n=n)

  chobTA@TA.values <- mom[lchob@xsubset]
  chobTA@name <- "chartMomentum"
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
                        n=n)
  if(is.null(sys.call(-1))) {
    TA <- lchob@passed.args$TA
    lchob@passed.args$TA <- c(TA,chobTA)
    lchob@windows <- lchob@windows + ifelse(chobTA@new,1,0)
    do.call('chartSeries.chob',list(lchob))
    invisible(chobTA)
  } else {
   return(chobTA)
  } 
} #}}}
# chartMomentum {{{
`chartMomentum` <-
function(x) {
    spacing <- x@params$spacing
    width <- x@params$width

    x.range <- x@params$xrange
    x.range <- seq(x.range[1],x.range[2]*spacing)

    multi.col <- x@params$multi.col
    color.vol <- x@params$color.vol

    n <- x@params$n
    mom <- x@TA.values

    y.range <- seq(-max(abs(mom),na.rm=TRUE),max(abs(mom),na.rm=TRUE),
                   length.out=length(x.range)) * 1.05
    plot(x.range,y.range,
         type='n',axes=FALSE,ann=FALSE)
    coords <- par('usr')
    rect(coords[1],coords[3],coords[2],coords[4],col=x@params$colors$area)
    grid(NA,NULL,col=x@params$colors$grid.col)

    COLOR <- "#0033CC"

    abline(h=0,col="#666666",lwd=1,lty='dotted')

    lines(seq(1,length(x.range),by=spacing),mom,col=COLOR,lwd=2,type='l')

    text(0, last(y.range)*.9, 
         paste("Momentum (", x@params$n, "):"),pos=4)

    text(0, last(y.range)*.9,
         paste("\n\n\n",sprintf("%.2f",last(mom)),sep=''),
         col = COLOR, pos = 4)

    axis(2)
    box(col=x@params$colors$fg.col)
} # }}}

# addCCI {{{
`addCCI` <- function(n=20, maType="SMA", c=0.015) {


  lchob <- get.current.chob()

  x <- as.matrix(lchob@xdata)

  chobTA <- new("chobTA")
  chobTA@new <- TRUE

  xx <- if(is.OHLC(x)) {
    cbind(Hi(x),Lo(x),Cl(x))
  } else x 

  cci <- CCI(xx,n=n,maType=maType,c=c)

  chobTA@TA.values <- cci[lchob@xsubset]
  chobTA@name <- "chartCCI"
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
                        n=n,maType=maType,c=c)
  if(is.null(sys.call(-1))) {
    TA <- lchob@passed.args$TA
    lchob@passed.args$TA <- c(TA,chobTA)
    lchob@windows <- lchob@windows + ifelse(chobTA@new,1,0)
    do.call('chartSeries.chob',list(lchob))
    invisible(chobTA)
  } else {
   return(chobTA)
  } 
} #}}}
# chartCCI {{{
`chartCCI` <-
function(x) {
    spacing <- x@params$spacing
    width <- x@params$width

    x.range <- x@params$xrange
    x.range <- seq(x.range[1],x.range[2]*spacing)

    multi.col <- x@params$multi.col
    color.vol <- x@params$color.vol

    n <- x@params$n
    cci <- x@TA.values

    y.range <- seq(-max(abs(cci),na.rm=TRUE),
                   max(abs(cci),na.rm=TRUE),
                   length.out=length(x.range))*1.05
    plot(x.range,y.range,
         type='n',axes=FALSE,ann=FALSE)
    coords <- par('usr')
    rect(coords[1],coords[3],coords[2],coords[4],col=x@params$colors$area)
    grid(NA,NULL,col=x@params$colors$grid.col)

    usr <- par('usr')

    # draw shading in -100:100 y-range 
    rect(usr[1],-100,usr[2],100,col=x@params$colors$BBands$fill)

    # fill upper and lower areas
    xx <- seq(1,length(x.range),by=spacing)
    cci.above <- ifelse(cci >=  100,cci, 100)
    cci.below <- ifelse(cci <= -100,cci,-100)
    
    polygon(c(xx,rev(xx)),c(cci.above,rep(100,length(xx))),col="red")
    polygon(c(xx,rev(xx)),c(cci.below,rep(-100,length(xx))),col="red")

    # draw CCI
    lines(seq(1,length(x.range),by=spacing),cci,col='red',lwd=1,type='l')

    # draw dotted guide line at 0
    abline(h=0,col='#666666',lwd=1,lty='dotted')

    # add indicator name and last value
    text(0, last(y.range)*.9,
         paste("Commodity Channel Index (", x@params$n, ",",
         x@params$c,"):",sep=''),pos=4)
    text(0, last(y.range)*.9,
         paste("\n\n\n",sprintf("%.2f",last(cci)),sep=''), col = 'red', 
         pos = 4)

    axis(2)
    box(col=x@params$colors$fg.col)
} # }}}

# addADX {{{
`addADX` <- function(n=14, maType="EMA", wilder=TRUE) {


  lchob <- get.current.chob()

  x <- as.matrix(lchob@xdata)

  chobTA <- new("chobTA")
  chobTA@new <- TRUE

  if(!is.OHLC(x)) stop("only applicable to HLC series")

  adx <- ADX(cbind(Hi(x),Lo(x),Cl(x)),n=n,maType=maType,wilder=wilder)

  chobTA@TA.values <- adx[lchob@xsubset,]
  chobTA@name <- "chartADX"
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
                        n=n,maType=maType,wilder=wilder)
  if(is.null(sys.call(-1))) {
    TA <- lchob@passed.args$TA
    lchob@passed.args$TA <- c(TA,chobTA)
    lchob@windows <- lchob@windows + ifelse(chobTA@new,1,0)
    do.call('chartSeries.chob',list(lchob))
    invisible(chobTA)
  } else {
   return(chobTA)
  } 
} #}}}
# chartADX {{{
`chartADX` <-
function(x) {
    spacing <- x@params$spacing
    width <- x@params$width

    x.range <- x@params$xrange
    x.range <- seq(x.range[1],x.range[2]*spacing)

    multi.col <- x@params$multi.col
    color.vol <- x@params$color.vol

    n <- x@params$n
    adx <- x@TA.values
    plot(x.range,seq(min(adx[,4]*.975,na.rm=TRUE),
         max(adx[,4]*1.05,na.rm=TRUE),length.out=length(x.range)),
         type='n',axes=FALSE,ann=FALSE)
    coords <- par('usr')
    rect(coords[1],coords[3],coords[2],coords[4],col=x@params$colors$area)
    grid(NA,NULL,col=x@params$colors$grid.col)
    # draw DIp
    lines(seq(1,length(x.range),by=spacing),adx[,1],col='green',lwd=1,type='l')
    # draw DIn
    lines(seq(1,length(x.range),by=spacing),adx[,2],col='red',lwd=1,type='l')
    # draw ADX
    lines(seq(1,length(x.range),by=spacing),adx[,4],col='blue',lwd=2,type='l')

    # draw upper and lower guidelines
    abline(h=20,col='#666666',lwd=1,lty='dotted')
    abline(h=40,col='#666666',lwd=1,lty='dotted')
    #title(ylab=paste('SMI(',paste(param,collapse=','),')',sep=''))
    axis(2)
    box(col=x@params$colors$fg.col)
} # }}}

# addATR {{{
`addATR` <- function(n=14, maType="EMA", ...) {


  lchob <- get.current.chob()

  x <- as.matrix(lchob@xdata)

  chobTA <- new("chobTA")
  chobTA@new <- TRUE

  if(!is.OHLC(x)) stop("only applicable to HLC series")

  atr <- ATR(cbind(Hi(x),Lo(x),Cl(x)),n=n,maType=maType,...)

  chobTA@TA.values <- atr[lchob@xsubset,]
  chobTA@name <- "chartATR"
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
                        n=n,maType=maType)
  if(is.null(sys.call(-1))) {
    TA <- lchob@passed.args$TA
    lchob@passed.args$TA <- c(TA,chobTA)
    lchob@windows <- lchob@windows + ifelse(chobTA@new,1,0)
    do.call('chartSeries.chob',list(lchob))
    invisible(chobTA)
  } else {
   return(chobTA)
  } 
} #}}}
# chartATR {{{
`chartATR` <-
function(x) {
    spacing <- x@params$spacing
    width <- x@params$width

    x.range <- x@params$xrange
    x.range <- seq(x.range[1],x.range[2]*spacing)

    multi.col <- x@params$multi.col
    color.vol <- x@params$color.vol

    n <- x@params$n
    atr <- x@TA.values
    plot(x.range,seq(min(atr[,2]*.975,na.rm=TRUE),
         max(atr[,2]*1.05,na.rm=TRUE),length.out=length(x.range)),
         type='n',axes=FALSE,ann=FALSE)
    coords <- par('usr')
    rect(coords[1],coords[3],coords[2],coords[4],col=x@params$colors$area)
    grid(NA,NULL,col=x@params$colors$grid.col)

    # draw ADX
    lines(seq(1,length(x.range),by=spacing),atr[,2],col='blue',lwd=2,type='l')

    axis(2)
    box(col=x@params$colors$fg.col)
} # }}}

# addTRIX {{{
`addTRIX` <- function(n=20, signal=9, maType="EMA", percent=TRUE) {


  lchob <- get.current.chob()

  x <- as.matrix(lchob@xdata)

  chobTA <- new("chobTA")
  chobTA@new <- TRUE

  xx <- if(is.OHLC(x)) {
    Cl(x)
  } else x 

  trix <- TRIX(xx,n=n,nSig=signal,maType=maType,percent=percent)

  chobTA@TA.values <- trix[lchob@xsubset]

  chobTA@name <- "chartTRIX"
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
                        n=n,signal=signal,maType=maType,percent=percent)
  if(is.null(sys.call(-1))) {
    TA <- lchob@passed.args$TA
    lchob@passed.args$TA <- c(TA,chobTA)
    lchob@windows <- lchob@windows + ifelse(chobTA@new,1,0)
    do.call('chartSeries.chob',list(lchob))
    invisible(chobTA)
  } else {
   return(chobTA)
  } 
} #}}}
# chartTRIX {{{
`chartTRIX` <-
function(x) {
    spacing <- x@params$spacing
    width <- x@params$width

    x.range <- x@params$xrange
    x.range <- seq(x.range[1],x.range[2]*spacing)

    multi.col <- x@params$multi.col
    color.vol <- x@params$color.vol

    n <- x@params$n

    trix <- x@TA.values

    plot(x.range,seq(min(trix[,1]*.975,na.rm=TRUE),
         max(trix[,1]*1.05,na.rm=TRUE),length.out=length(x.range)),
         type='n',axes=FALSE,ann=FALSE)

    coords <- par('usr')
    rect(coords[1],coords[3],coords[2],coords[4],col=x@params$colors$area)
    grid(NA,NULL,col=x@params$colors$grid.col)

    # draw TRIX
    lines(seq(1,length(x.range),by=spacing),trix[,1],col='green',lwd=1,type='l')
    # draw Signal
    lines(seq(1,length(x.range),by=spacing),trix[,2],col='#999999',lwd=1,type='l')

    axis(2)
    box(col=x@params$colors$fg.col)
} # }}}

# addDPO {{{
`addDPO` <- function(n=10, maType="EMA", shift=n/2+1, percent=FALSE) {


  lchob <- get.current.chob()

  x <- as.matrix(lchob@xdata)

  chobTA <- new("chobTA")
  chobTA@new <- TRUE
 
  # should really allow for _any_ series to be used, like MA (FIXME)

  xx <- if(is.OHLC(x)) {
    Cl(x)
  } else x 

  dpo <- DPO(xx,n=n,maType=maType,shift=shift,percent=percent)

  chobTA@TA.values <- dpo[lchob@xsubset]

  chobTA@name <- "chartDPO"
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
                        n=n,maType=maType,shift=shift,percent=percent)
  if(is.null(sys.call(-1))) {
    TA <- lchob@passed.args$TA
    lchob@passed.args$TA <- c(TA,chobTA)
    lchob@windows <- lchob@windows + ifelse(chobTA@new,1,0)
    do.call('chartSeries.chob',list(lchob))
    invisible(chobTA)
  } else {
   return(chobTA)
  } 
} #}}}
# chartDPO {{{
`chartDPO` <-
function(x) {
    spacing <- x@params$spacing
    width <- x@params$width

    x.range <- x@params$xrange
    x.range <- seq(x.range[1],x.range[2]*spacing)

    multi.col <- x@params$multi.col
    color.vol <- x@params$color.vol

    n <- x@params$n
    dpo <- x@TA.values

    y.range <- seq(-max(abs(dpo), na.rm = TRUE), max(abs(dpo), 
                   na.rm = TRUE), length.out = length(x.range)) * 1.05

    if(x@new) {
      plot(x.range,y.range,
           type='n',axes=FALSE,ann=FALSE)
      coords <- par('usr')
      rect(coords[1],coords[3],coords[2],coords[4],col=x@params$colors$area)
      grid(NA,NULL,col=x@params$colors$grid.col)
    }
    xx <- seq(1,length(x.range),by=spacing)

    dpo.tmp <- dpo
    dpo.tmp[is.na(dpo)] <- 0
    dpo.positive <- ifelse(dpo.tmp >= 0,dpo.tmp,0)
    dpo.negative <- ifelse(dpo.tmp <  0,dpo.tmp,0)

    polygon(c(xx,rev(xx)),c(dpo.positive,rep(0,length(dpo))),col=x@params$colors$up.col)
    polygon(c(xx,rev(xx)),c(dpo.negative,rep(0,length(dpo))),col=x@params$colors$dn.col)

    abline(h=0,col="#999999")

    text(0, last(y.range)*.9,
         paste("De-trended Price Oscillator (", x@params$n,"):", sep = ""), 
        pos = 4)

    text(0, last(y.range)*.9,
        paste("\n\n\n",sprintf("%.3f",last(na.omit(dpo))), sep = ""), 
        col = ifelse(last(dpo) > 0,x@params$colors$up.col,x@params$colors$dn.col), 
        pos = 4)

    axis(2)
    box(col=x@params$colors$fg.col)

#    y.range <- seq(-max(abs(dpo), na.rm = TRUE), max(abs(dpo), 
#        na.rm = TRUE), length.out = length(x.range)) * 1.05
#    plot(x.range, y.range, type = "n", axes = FALSE, ann = FALSE)
#
#    grid(NA,NULL,col=x@params$colors$grid.col)
#
#    # draw DPO
#    lines(seq(1,length(x.range),by=spacing),dpo,col='green',lwd=1,type='l')
#
#    #title(ylab=paste('SMI(',paste(param,collapse=','),')',sep=''))
#    axis(2)
#    box(col=x@params$colors$fg.col)
} # }}}

# addRSI {{{
`addRSI` <- function(n=14,maType='EMA',wilder=TRUE) {


  lchob <- get.current.chob()

  x <- as.matrix(lchob@xdata)

  chobTA <- new("chobTA")
  chobTA@new <- TRUE


  xx <- if(is.OHLC(x)) {
    Cl(x)
  } else x 

  rsi <- RSI(xx,n=n,maType=maType,wilder=wilder)
  chobTA@TA.values <- rsi[lchob@xsubset]
  chobTA@name <- "chartRSI"
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
                        n=n, wilder=wilder,maType=maType)
  if(is.null(sys.call(-1))) {
    TA <- lchob@passed.args$TA
    lchob@passed.args$TA <- c(TA,chobTA)
    lchob@windows <- lchob@windows + ifelse(chobTA@new,1,0)
    do.call('chartSeries.chob',list(lchob))
    invisible(chobTA)
  } else {
   return(chobTA)
  } 
} #}}}
# chartRSI {{{
`chartRSI` <-
function(x) {
    spacing <- x@params$spacing
    width <- x@params$width

    x.range <- x@params$xrange
    x.range <- seq(x.range[1],x.range[2]*spacing)

    multi.col <- x@params$multi.col
    color.vol <- x@params$color.vol

    param <- x@params$param; ma.type <- x@params$ma.type
    rsi <- x@TA.values

    y.range <- seq(min(rsi,na.rm=TRUE)*.975,max(rsi,na.rm=TRUE)*1.05,
                   length.out=length(x.range))

    if(x@new) {
      plot(x.range,y.range,type='n',axes=FALSE,ann=FALSE)
  
      coords <- par('usr')
      rect(coords[1],coords[3],coords[2],coords[4],col=x@params$colors$area)
      grid(NA,NULL,col=x@params$colors$grid.col)
    }
    lines(seq(1,length(x.range),by=spacing),rsi,col='#0033CC',lwd=2,type='l')
    lines(seq(1,length(x.range),by=spacing),rsi,col='#BFCFFF',lwd=1,lty='dotted',type='l')

    text(0, last(y.range)*.9,
         paste("Relative Strength Index (", x@params$n,"):", sep = ""), 
         pos = 4)

    text(0, last(y.range)*.9,
         paste("\n\n\n",sprintf("%.3f",last(rsi)), sep = ""), col = '#0033CC', 
         pos = 4)

    axis(2)
    box(col=x@params$colors$fg.col)
} # }}}

# addROC {{{
`addROC` <- function(n=1,type=c('discrete','continuous'),col='red') {


  lchob <- get.current.chob()

  x <- as.matrix(lchob@xdata)

  chobTA <- new("chobTA")
  chobTA@new <- TRUE

  xx <- if(is.OHLC(x)) {
    Cl(x)
  } else x 

  type <- match.arg(type)

  roc <- ROC(xx,n=n,type=type,na.pad=TRUE)

  chobTA@TA.values <- roc[lchob@xsubset]
  chobTA@name <- "chartROC"
  chobTA@call <- match.call()
  chobTA@params <- list(xrange=lchob@xrange,
                        colors=lchob@colors,
                        multi.col=lchob@multi.col,
                        spacing=lchob@spacing,
                        width=lchob@width,
                        bp=lchob@bp,
                        x.labels=lchob@x.labels,
                        time.scale=lchob@time.scale,
                        n=n,type=type,col=col)
  if(is.null(sys.call(-1))) {
    TA <- lchob@passed.args$TA
    lchob@passed.args$TA <- c(TA,chobTA)
    lchob@windows <- lchob@windows + ifelse(chobTA@new,1,0)
    do.call('chartSeries.chob',list(lchob))
    invisible(chobTA)
  } else {
   return(chobTA)
  } 
} #}}}
# chartROC {{{
`chartROC` <-
function(x) {
    spacing <- x@params$spacing
    width <- x@params$width

    x.range <- x@params$xrange
    x.range <- seq(x.range[1],x.range[2]*spacing)

    multi.col <- x@params$multi.col
    color.vol <- x@params$color.vol

    #param <- x@params$param; ma.type <- x@params$ma.type
    roc <- x@TA.values
    if(x@new) {
      plot(x.range,seq(min(roc*.975,na.rm=TRUE),max(roc*1.05,na.rm=TRUE),length.out=length(x.range)),
         type='n',axes=FALSE,ann=FALSE)
      grid(NA,NULL,col=x@params$colors$grid.col)
    }
    lines(seq(1,length(x.range),by=spacing),roc,col=x@params$col,lwd=2,type='l')
    #title(ylab=paste('RSI(',paste(c(n.up,collapse=','),')',sep=''))
    axis(2)
    box(col=x@params$colors$fg.col)
} # }}}

# addBBands {{{
`addBBands` <- function(n=20,sd=2,maType='SMA',draw='bands',on=-1) {


  draw.options <- c('bands','percent','width')
  draw <- draw.options[pmatch(draw,draw.options)]

  lchob <- get.current.chob()

  x <- as.matrix(lchob@xdata)

  chobTA <- new("chobTA")
  if(draw=='bands') {
    chobTA@new <- FALSE
    } else {
      chobTA@new <- TRUE
      on <- NULL
  }


  xx <- if(is.OHLC(x)) {
    cbind(Hi(x),Lo(x),Cl(x))
  } else x 

  bb <- BBands(xx,n=n,maType=maType,sd=sd)
  
  chobTA@TA.values <- bb[lchob@xsubset,]
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
                        n=n,ma=maType,sd=sd,
                        draw=draw)
  return(chobTA)
} #}}}
# chartBBands {{{
`chartBBands` <-
function(x) {
    spacing <- x@params$spacing
    width <- x@params$width

    x.range <- x@params$xrange
    x.range <- seq(x.range[1],x.range[2]*spacing)

    multi.col <- x@params$multi.col
    color.vol <- x@params$color.vol

    bband.col <- ifelse(!is.null(x@params$colors$BBands$col),
                        x@params$colors$BBands$col,'red') 
    bband.fill <- ifelse(!is.null(x@params$colors$BBands$fill),
                        x@params$colors$BBands$fill,x@params$colors$bg.col) 

    # bband col vector
    # lower.band, middle.band, upper.band, %b, bb.width
    if(length(bband.col) == 1) # no user specified
      bband.col <- c(bband.col,'grey',rep(bband.col,3))
    
    param <- x@params$param; ma.type <- x@params$ma.type
   
    bb <- x@TA.values

    if(x@params$draw == 'bands') {
      # draw Bollinger Bands on price chart
      if(x@on[1] > 0) {
        lines(seq(1,length(x.range),by=spacing),
              bb[,1],col=bband.col[1],lwd=1,lty='dashed')
        lines(seq(1,length(x.range),by=spacing),
              bb[,3],col=bband.col[3],lwd=1,lty='dashed')
        lines(seq(1,length(x.range),by=spacing),
              bb[,2],col=bband.col[2],lwd=1,lty='dotted')
      } else {
        xx <- seq(1,length(x.range),by=spacing)
        polygon(c(xx,rev(xx)),
                c(bb[,1],rev(bb[,3])),col=bband.fill,border=NA)
        lines(seq(1,length(x.range),by=spacing),
              bb[,1],col=bband.col[1],lwd=1,lty='dashed')
        lines(seq(1,length(x.range),by=spacing),
              bb[,3],col=bband.col[3],lwd=1,lty='dashed')
        lines(seq(1,length(x.range),by=spacing),
              bb[,2],col=bband.col[2],lwd=1,lty='dotted')
      }
     
      # return the text to be pasted
      legend.text <- list()
      legend.text[[1]] <- list(legend=paste("Bollinger Bands (",
                     paste(x@params$n,x@params$sd,sep=","),") [Upper/Lower]: ",
                     sprintf("%.3f",last(bb[,3])),"/",
                     sprintf("%.3f",last(bb[,1])), sep = ""), 
                     text.col = bband.col[3]) 
      invisible(legend.text)
    } else 
      if(x@params$draw == 'percent') {
        # draw %B in new frame
        y.range <- seq(min(bb[,4], na.rm = TRUE) * .9,
                        max(abs(bb[,4]), na.rm = TRUE) * 1.05,
                        length.out = length(x.range))
        plot(x.range, y.range, type = "n", axes = FALSE, ann = FALSE)
        grid(NA,NULL,col=x@params$colors$grid.col)

        lines(seq(1,length(x.range),by=spacing), bb[,4],
              col=bband.col[4],lwd=1)
        
        text(0,last(y.range) * .9, paste("Bollinger %b (",
             paste(x@params$n,x@params$sd,sep=","), "): ",
             sep=""), pos=4)
        text(0,last(y.range) * .9, paste("\n\n\n",
             sprintf("%.3f",last(bb[,4])), sep = ""),
             pos=4, col=bband.col[4])

        axis(2)
        box(col = x@params$colors$fg.col)
        
      } else {
        # draw width in new frame
        # (high band - low band) / middle band
        bbw <- (bb[,3] - bb[,1]) / bb[,2]
        
        y.range <- seq(min(bbw, na.rm = TRUE) * .9,
                        max(abs(bbw), na.rm = TRUE) * 1.05,
                        length.out = length(x.range))
        plot(x.range, y.range, type = "n", axes = FALSE, ann = FALSE)
        grid(NA,NULL,col=x@params$colors$grid.col)

        lines(seq(1,length(x.range),by=spacing), bbw,
              col=bband.col[5],lwd=1)
        
        text(0,last(y.range) * .9, paste("Bollinger Band Width (",
             paste(x@params$n,x@params$sd,sep=","), "): ",
             sep=""), pos=4)
        text(0,last(y.range) * .9, paste("\n\n\n",
             sprintf("%.3f",last(bbw)), sep = ""),
             pos=4, col=bband.col[5])

        axis(2)
        box(col = x@params$colors$fg.col)
      }
} # }}}

# addEnvelope {{{
`addEnvelope` <- function(n=20,p=2.5,maType='SMA',...,on=1) {


  lchob <- get.current.chob()

  x <- as.matrix(lchob@xdata)

  chobTA <- new("chobTA")
  chobTA@new <- FALSE

  xx <- if(is.OHLC(x)) {
    Cl(x)
  } else x 

  ma <- do.call(maType,list(xx,n=n,...))
  mae <- cbind(ma*(1-p/100),ma,ma*(1+p/100))
  
  chobTA@TA.values <- mae[lchob@xsubset,]

  chobTA@name <- "chartEnvelope"
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
                        n=n,p=p,maType=maType)
  if(is.null(sys.call(-1))) {
    TA <- lchob@passed.args$TA
    lchob@passed.args$TA <- c(TA,chobTA)
    lchob@windows <- lchob@windows + ifelse(chobTA@new,1,0)
    do.call('chartSeries.chob',list(lchob))
    invisible(chobTA)
  } else {
   return(chobTA)
  } 
} #}}}
# chartEnvelope {{{
`chartEnvelope` <-
function(x) {
    spacing <- x@params$spacing
    width <- x@params$width

    x.range <- x@params$xrange
    x.range <- seq(x.range[1],x.range[2]*spacing)

    multi.col <- x@params$multi.col
    color.vol <- x@params$color.vol

    mae <- x@TA.values
    if(x@on[1] > 0) {
      lines(seq(1,length(x.range),by=spacing),mae[,1],col='blue',lwd=1,lty='dotted')
      lines(seq(1,length(x.range),by=spacing),mae[,3],col='blue',lwd=1,lty='dotted')
      #lines(seq(1,length(x.range),by=spacing),mae[,2],col='grey',lwd=1,lty='dotted')
    } else {
      xx <- seq(1,length(x.range),by=spacing)
      polygon(c(xx,rev(xx)), c(mae[,1],rev(mae[,3])),col='#282828',border=NA)
      lines(seq(1,length(x.range),by=spacing),mae[,1],col='blue',lwd=1,lty='dotted')
      lines(seq(1,length(x.range),by=spacing),mae[,3],col='blue',lwd=1,lty='dotted')
      #lines(seq(1,length(x.range),by=spacing),mae[,2],col='grey',lwd=1,lty='dotted')
    }
   
    # return the text to be pasted
    txt <- list()
    txt[[1]] <- list(text=paste("Moving Ave. Envelope (",
                   paste(x@params$n,x@params$p,sep=","),") [Upper/Lower]: ",
                   sprintf("%.3f",last(mae[,3])),"/",
                   sprintf("%.3f",last(mae[,1])), sep = ""), col = 'blue') 
    invisible(txt)

} # }}}

# addSAR {{{
`addSAR` <- function(accel=c(0.02,0.2),col='blue') {


  lchob <- get.current.chob()

  x <- as.matrix(lchob@xdata)

  chobTA <- new("chobTA")
  chobTA@new <- FALSE

  if(!is.OHLC(x)) stop("SAR requires HL series") 

  sar <- SAR(cbind(Hi(x),Lo(x)),accel=accel)

  chobTA@TA.values <- sar[lchob@xsubset]

  chobTA@name <- "chartSAR"
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
                        accel=accel,col=col)
  if(is.null(sys.call(-1))) {
    TA <- lchob@passed.args$TA
    lchob@passed.args$TA <- c(TA,chobTA)
    lchob@windows <- lchob@windows + ifelse(chobTA@new,1,0)
    do.call('chartSeries.chob',list(lchob))
    invisible(chobTA)
  } else {
   return(chobTA)
  } 
} #}}}
# chartSAR {{{
`chartSAR` <-
function(x) {
    spacing <- x@params$spacing
    width <- x@params$width

    x.range <- x@params$xrange
    x.range <- seq(x.range[1],x.range[2]*spacing)

    multi.col <- x@params$multi.col
    color.vol <- x@params$color.vol

    sar <- x@TA.values
    points(seq(1,length(x.range),by=spacing),sar,col=x@params$col,cex=0.5)
} # }}}

# addMACD {{{
`addMACD` <- function(fast=12,slow=26,signal=9,type='EMA',histogram=TRUE,col) {


  lchob <- get.current.chob()

  x <- as.matrix(lchob@xdata)

  chobTA <- new("chobTA")
  chobTA@new <- TRUE

  col <- if(missing(col)) col <- c('#999999','#777777',
                              '#BBBBBB','#FF0000')

  xx <- if(is.OHLC(x)) {
    Cl(x)
  } else x 

  macd <- MACD(xx,nFast=fast,nSlow=slow,nSig=signal,maType=type)
  
  chobTA@TA.values <- macd[lchob@xsubset,]

  chobTA@name <- "chartMACD"
  chobTA@call <- match.call()
  chobTA@params <- list(xrange=lchob@xrange,
                        colors=lchob@colors,
                        spacing=lchob@spacing,
                        width=lchob@width,
                        bp=lchob@bp,
                        x.labels=lchob@x.labels,
                        time.scale=lchob@time.scale,
                        fast=fast,slow=slow,signal=signal,
                        col=col,histo=histogram
                        )
  return(chobTA)
} #}}}
# chartMACD {{{
`chartMACD` <-
function(x) {
    spacing <- x@params$spacing
    width <- x@params$width

    x.range <- x@params$xrange
    x.range <- seq(x.range[1],x.range[2]*spacing)

    col <- x@params$col
    macd <- x@TA.values

    y.range <- seq(-max(abs(macd),na.rm=TRUE),max(abs(macd),na.rm=TRUE),
                   length.out=length(x.range)) * 1.05

    if(x@new) {
      plot(x.range,y.range,type='n',axes=FALSE,ann=FALSE)

      coords <- par('usr')
      rect(coords[1],coords[3],coords[2],coords[4],col=x@params$colors$area)
      grid(NA,NULL,col=x@params$colors$grid.col)
    }

    if(x@params$histo) {
      x.pos <- 1 + spacing * (1:NROW(macd) -1)
      cols <- ifelse((macd[,1]-macd[,2]) > 0, col[1],col[2])
      rect(x.pos - spacing/5,0,x.pos + spacing/5, macd[,1]-macd[,2],
           col=cols,border=cols)
    } 

    lines(seq(1,length(x.range),by=spacing),macd[,1],col=col[3],lwd=1)
    lines(seq(1,length(x.range),by=spacing),macd[,2],col=col[4],lwd=1,lty='dotted')

    legend("topleft",
           legend=c(paste("Moving Average Convergence Divergence (",
                    paste(x@params$fast,x@params$slow,x@params$signal,sep=','),"):", sep = ""),
                    paste("MACD:",sprintf("%.3f",last(macd[,1]))),
                    paste("Signal:",sprintf("%.3f",last(macd[,2])))),
           text.col=c(x@params$colors$fg.col, col[3], col[4]), bty='n', y.intersp=0.95) 
#   text(0, last(y.range)*.9,
#        paste("Moving Average Convergence Divergence (",
#        paste(x@params$fast,x@params$slow,x@params$signal,sep=','),"):", sep = ""), 
#        pos = 4)

#   text(0, last(y.range)*.9,
#       paste("\n\n\nMACD: ",sprintf("%.3f",last(macd[,1])), sep = ""),
#       col = col[3],pos = 4)

#   text(0, last(y.range)*.9,
#       paste("\n\n\n\n\n\nSignal: ",sprintf("%.3f",last(macd[,2])), sep = ""),
#       col = col[4],pos = 4)

    axis(2)
    box(col=x@params$colors$fg.col)
} # }}}

# addShading {{{
`addShading` <- function(when,on=-1,overlay=TRUE,col='blue') {

  lchob <- get.current.chob()
  chobTA <- new("chobTA")
  chobTA@new <- !overlay

    x <- lchob@xdata
    i <- when
    indexClass(x) <- "POSIXct"
    POSIXindex <- index(x)
    if (missing(i)) 
        i <- 1:NROW(x)
    if (timeBased(i)) 
        i <- as.character(as.POSIXct(i))
    if (is.character(i)) {
        i <- strsplit(i, ';')[[1]]
        i.tmp <- NULL
        for (ii in i) {
            if (!identical(grep("::", ii), integer(0))) {
                dates <- strsplit(ii, "::")[[1]]
                first.time <- ifelse(dates[1] == "", POSIXindex[1], 
                  do.call("firstof", as.list(as.numeric(strsplit(dates[1], 
                    ":|-|/| ")[[1]]))))
                last.time <- ifelse(length(dates) == 1, POSIXindex[length(POSIXindex)], 
                  do.call("lastof", as.list(as.numeric(strsplit(dates[2], 
                    ":|-|/| ")[[1]]))))
            }
            else {
                dates <- ii
                first.time <- do.call("firstof", as.list(as.numeric(strsplit(dates, 
                  ":|-|/| ")[[1]])))
                last.time <- do.call("lastof", as.list(as.numeric(strsplit(dates, 
                  ":|-|/| ")[[1]])))
            }
            i.tmp <- c(i.tmp, which(POSIXindex <= last.time & 
                POSIXindex >= first.time))
        }
        i <- i.tmp
    }

  xstart <- unique(c(i[1],i[which(diff(i) != 1)+1]))
  xend   <- unique(c(i[which(diff(i) != 1)-1], rev(i)[1]))

  chobTA@TA.values <- x
  chobTA@name <- "chartShading"
  chobTA@call <- match.call()
  chobTA@on <- on # used for deciding when to draw...
  chobTA@params <- list(xrange=lchob@xrange,
                        yrange=lchob@yrange,
                        colors=lchob@colors,
                        spacing=lchob@spacing,
                        width=lchob@width,
                        xsubset=lchob@xsubset,
                        time.scale=lchob@time.scale,
                        xstart=xstart,xend=xend
                        )
  if(is.null(sys.call(-1))) {
    TA <- lchob@passed.args$TA
    lchob@passed.args$TA <- c(TA,chobTA)
    lchob@windows <- lchob@windows + ifelse(chobTA@new,1,0)
    do.call('chartSeries.chob',list(lchob))
    invisible(chobTA)
  } else {
   return(chobTA)
  } 
} # }}}
# chartShading {{{
`chartShading` <-
function(x) {
    spacing <- x@params$spacing
    width <- x@params$width

    x.range <- x@params$xrange
    x.range <- seq(x.range[1],x.range[2]*spacing)
    y.range <- x@params$yrange
    xstart <- x@params$xstart
    xend <- x@params$xend
 
    rect(((xstart-1)*spacing+1)-width/2, rep(y.range[1]*.95,length(xstart)),
         ((xend-1)*spacing+1)+width/2, rep(y.range[2]*1.05,length(xend)),
         col=c(x@params$colors$BBands$fill),border=NA)
      #abline(v=(x@params$v-1)*spacing+1,col=x@params$col)
} # }}}

# addLines {{{
`addLines` <- function(x,h,v,on=1,overlay=TRUE,col='blue') {
 
  if(missing(x)) x <- NULL
  if(missing(h)) h <- NULL
  if(missing(v)) v <- NULL

  lchob <- get.current.chob()
  chobTA <- new("chobTA")
  chobTA@new <- !overlay

  chobTA@TA.values <- NULL # single numeric vector
  chobTA@name <- "chartLines"
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
                        col=col,h=h,x=x,v=v)
  if(is.null(sys.call(-1))) {
    TA <- lchob@passed.args$TA
    lchob@passed.args$TA <- c(TA,chobTA)
    lchob@windows <- lchob@windows + ifelse(chobTA@new,1,0)
    do.call('chartSeries.chob',list(lchob))
    invisible(chobTA)
  } else {
   return(chobTA)
  } 
} # }}}
# chartLines {{{
`chartLines` <-
function(x) {
    spacing <- x@params$spacing
    width <- x@params$width

    x.range <- x@params$xrange
    x.range <- seq(x.range[1],x.range[2]*spacing)

    multi.col <- x@params$multi.col
    color.vol <- x@params$color.vol
 
    if(!is.null(x@params$x)) {
      # draw lines given positions specified in x
      lines(x=(x@params$x-1)*spacing+1,col=x@params$col)  
    }
    if(!is.null(x@params$h)) {
      # draw horizontal lines given positions specified in h
      abline(h=x@params$h,col=x@params$col)
    }
    if(!is.null(x@params$v)) {
      # draw vertical lines given positions specified in v
      abline(v=(x@params$v-1)*spacing+1,col=x@params$col)
    }

} # }}}

# addPoints {{{
`addPoints` <- function(x,y=NULL,type='p',pch=20,
                        offset=1,col=2,bg=2,cex=1,
                        on=1,overlay=TRUE) {
 
  lchob <- get.current.chob()
  xdata <- as.matrix(lchob@xdata)

  chobTA <- new("chobTA")
  chobTA@new <- !overlay

  
  chobTA@TA.values <- xdata[lchob@xsubset,]
  chobTA@name <- "chartPoints"
  chobTA@call <- match.call()
  chobTA@on <- on # used for deciding when to draw...

  if(missing(bg)) bg <- col

    xsubset <- x %in% lchob@xsubset
    if(NROW(x) != NROW(y)) stop('x and y must be of equal lengths')
    x <- x[xsubset]
    if(!is.null(y))
      y <- y[xsubset]


  chobTA@params <- list(xrange=lchob@xrange,
                        colors=lchob@colors,
                        color.vol=lchob@color.vol,
                        multi.col=lchob@multi.col,
                        spacing=lchob@spacing,
                        width=lchob@width,
                        subset=lchob@xsubset,
                        x.labels=lchob@x.labels,
                        time.scale=lchob@time.scale,
                        x=x,y=y,type=type,offset=offset,
                        pch=pch,col=col,bg=bg,cex=cex)
  if(is.null(sys.call(-1))) {
    TA <- lchob@passed.args$TA
    lchob@passed.args$TA <- c(TA,chobTA)
    lchob@windows <- lchob@windows + ifelse(chobTA@new,1,0)
    do.call('chartSeries.chob',list(lchob))
    invisible(chobTA)
  } else {
   return(chobTA)
  } 
} # }}}
# chartPoints {{{
`chartPoints` <-
function(x) {
    spacing <- x@params$spacing
    width <- x@params$width

    x.range <- x@params$xrange
    x.range <- seq(x.range[1],x.range[2]*spacing)

    multi.col <- x@params$multi.col
    color.vol <- x@params$color.vol
 
    xdata <- x@TA.values
    x.points <- which(x@params$subset %in% x@params$x)
    y.points <- x@params$y
    type <- x@params$type
    offset <- x@params$offset
    pch <- x@params$pch
    col <- x@params$col
    bg <- x@params$bg
    cex <- x@params$cex

    # if OHLC and above - get Hi, else Lo
    # if univariate - get value
    y.data <- if(is.OHLC(xdata)) {
      if(offset > 1) {
        Hi(xdata)
      } else Lo(xdata)
    } else xdata

    if(is.null(y.points)) y.points <- y.data[x.points] * offset

    points(x=(x.points-1) * spacing + 1, y=y.points,
           type=type,pch=pch,col=col,bg=bg,cex=cex)
} # }}}

# addEMA {{{
`addEMA` <- function(n=10,wilder=FALSE,ratio=NULL,on=1,with.col=Cl,overlay=TRUE,col='blue') {


  lchob <- get.current.chob()
  chobTA <- new("chobTA")
  chobTA@new <- !overlay


  # get the appropriate data - from the approp. src
  if(on==1) {
    x <- as.matrix(lchob@xdata)

    if(!is.OHLC(x) && missing(with.col)) with.col <- 1

    if(is.function(with.col)) {
      x.tmp <- do.call(with.col,list(x))
    } else x.tmp <- x[,with.col]
  } else {
    # get values from TA...
    which.TA <- which(sapply(lchob@passed.args$TA,function(x) x@new))
    target.TA <- eval(lchob@passed.args$TA[which.TA][on-1])[[1]]

    x <- as.matrix(target.TA@TA.values)

    if(missing(with.col)) with.col <- 1
      
    if(is.function(with.col)) {
      x.tmp <- do.call(with.col,list(x))
    } else x.tmp <- x[,with.col]
  }

  ma.tmp <- NULL

  for(i in 1:length(n)) {
    ma <- EMA(x.tmp,n=n[i],wilder=wilder[1],
              ratio=ratio[1])
    ma.tmp <- cbind(ma.tmp,ma)
  }

  chobTA@TA.values <- matrix(ma.tmp[lchob@xsubset,],ncol=NCOL(ma.tmp))

  chobTA@name <- "chartEMA"
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
                        col=col,n=n,wilder=wilder,ratio=ratio)
  if(is.null(sys.call(-1))) {
    TA <- lchob@passed.args$TA
    lchob@passed.args$TA <- c(TA,chobTA)
    lchob@windows <- lchob@windows + ifelse(chobTA@new,1,0)
    do.call('chartSeries.chob',list(lchob))
    invisible(chobTA)
  } else {
   return(chobTA)
  } 
} # }}}
# chartEMA {{{
`chartEMA` <-
function(x) {
    spacing <- x@params$spacing
    width <- x@params$width

    x.range <- x@params$xrange
    x.range <- seq(x.range[1],x.range[2]*spacing)

    multi.col <- x@params$multi.col
    color.vol <- x@params$color.vol

    if(length(x@params$n) != length(x@params$col)) {
      colors <- 3:10
    } else colors <- x@params$col 
 
    chart.key <- list()

    for(li in 1:length(x@params$n)) {
      ma <- x@TA.values[,li]

      if(x@new) {
        par(new=TRUE)
        plot(x.range,seq(min(ma*.975),max(ma*1.05),length.out=length(x.range)),
             type='n',axes=FALSE,ann=FALSE)
        coords <- par('usr')
        rect(coords[1],coords[3],coords[2],coords[4],col=x@params$colors$area)
       # title(ylab=paste('EMA(',paste(x@params$n[li],collapse=','),')',sep=''))
        axis(2)
        box(col=x@params$colors$fg.col)
      }
      lines(seq(1,length(x.range),by=spacing),ma,col=colors[li],lwd=1,type='l')
      chart.key[[li]] <- list(text=paste("EMA (",
                   paste(x@params$n[li],sep=","),"): ",
                   sprintf("%.3f",last(ma)),
                   sep = ""), col = colors[li]) 

    }
    invisible(chart.key)

} # }}}

# addSMA {{{
`addSMA` <- function(n=10,on=1,with.col=Cl,overlay=TRUE,col='brown') {


  lchob <- get.current.chob()
  chobTA <- new("chobTA")
  chobTA@new <- !overlay

  # get the appropriate data - from the approp. src
  if(on==1) {
    x <- as.matrix(lchob@xdata)

    if(!is.OHLC(x) && missing(with.col)) with.col <- 1

    if(is.function(with.col)) {
      x.tmp <- do.call(with.col,list(x))
    } else x.tmp <- x[,with.col]
  } else {
    # get values from TA...
    which.TA <- which(sapply(lchob@passed.args$TA,function(x) x@new))
    target.TA <- eval(lchob@passed.args$TA[which.TA][on-1])[[1]]

    x <- as.matrix(target.TA@TA.values)

    if(missing(with.col)) with.col <- 1

    if(is.function(with.col)) {
      x.tmp <- do.call(with.col,list(x))
    } else x.tmp <- x[,with.col]
  }
  ma.tmp <- NULL
  for(i in 1:length(n)) {
    ma <- SMA(x.tmp,n=n[i])
    ma.tmp <- cbind(ma.tmp,ma)
  }

  chobTA@TA.values <- matrix(ma.tmp[lchob@xsubset,],ncol=NCOL(ma.tmp)) # single numeric vector
  chobTA@name <- "chartSMA"
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
                        col=col,n=n)
  if(is.null(sys.call(-1))) {
    TA <- lchob@passed.args$TA
    lchob@passed.args$TA <- c(TA,chobTA)
    lchob@windows <- lchob@windows + ifelse(chobTA@new,1,0)
    do.call('chartSeries.chob',list(lchob))
    invisible(chobTA)
  } else {
   return(chobTA)
  } 
} # }}}
# chartSMA {{{
`chartSMA` <-
function(x) {
    spacing <- x@params$spacing
    width <- x@params$width

    x.range <- x@params$xrange
    x.range <- seq(x.range[1],x.range[2]*spacing)

    multi.col <- x@params$multi.col
    color.vol <- x@params$color.vol

    if(length(x@params$n) != length(x@params$col)) {
      colors <- c(4:10,3)
    } else colors <- x@params$col

    chart.key <- list() 

    for(li in 1:length(x@params$n)) {
      ma <- x@TA.values[,li]
      if(x@new) {
        par(new=TRUE)
        plot(x.range,seq(min(ma*.975),max(ma*1.05),length.out=length(x.range)),
             type='n',axes=FALSE,ann=FALSE)
        coords <- par('usr')
        rect(coords[1],coords[3],coords[2],coords[4],col=x@params$colors$area)
        #title(ylab=paste('EMA(',paste(x@params$n[li],collapse=','),')',sep=''))
        axis(2)
        box(col=x@params$colors$fg.col)
      }
      lines(seq(1,length(x.range),by=spacing),ma,col=colors[li],lwd=1,type='l')
      chart.key[[li]] <- list(text = paste("SMA (", paste(x@params$n[li], 
            sep = ","), "): ", sprintf("%.3f", last(ma)), sep = ""), 
            col = colors[li])
    }
    invisible(chart.key)
} # }}}

# addWMA {{{
`addWMA` <- function(n=10,wts=1:n,on=1,with.col=Cl,overlay=TRUE,col='green') {


  lchob <- get.current.chob()
  chobTA <- new("chobTA")
  chobTA@new <- !overlay

  # get the appropriate data - from the approp. src
  if(on==1) {
    x <- as.matrix(lchob@xdata)

    if(!is.OHLC(x) && missing(with.col)) with.col <- 1

    if(is.function(with.col)) {
      x.tmp <- do.call(with.col,list(x))
    } else x.tmp <- x[,with.col]
  } else {
    # get values from TA...
    which.TA <- which(sapply(lchob@passed.args$TA,function(x) x@new))
    target.TA <- eval(lchob@passed.args$TA[which.TA][on-1])[[1]]
    x <- as.matrix(target.TA@TA.values)

    if(missing(with.col)) with.col <- 1

    if(is.function(with.col)) {
      x.tmp <- do.call(with.col,list(x))
    } else x.tmp <- x[,with.col]
  }

  chobTA@TA.values <- x.tmp[lchob@xsubset]
  chobTA@name <- "chartWMA"
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
                        col=col,n=n,wts=wts)
  if(is.null(sys.call(-1))) {
    TA <- lchob@passed.args$TA
    lchob@passed.args$TA <- c(TA,chobTA)
    lchob@windows <- lchob@windows + ifelse(chobTA@new,1,0)
    do.call('chartSeries.chob',list(lchob))
    invisible(chobTA)
  } else {
   return(chobTA)
  } 
} # }}}
# chartWMA {{{
`chartWMA` <-
function(x) {
    spacing <- x@params$spacing
    width <- x@params$width

    x.range <- x@params$xrange
    x.range <- seq(x.range[1],x.range[2]*spacing)

    multi.col <- x@params$multi.col
    color.vol <- x@params$color.vol

    if(length(x@params$n) < length(x@params$col)) {
      colors <- 3:10
    } else colors <- x@params$col
 
    for(li in 1:length(x@params$n)) {
      ma <- WMA(x@TA.values,n=x@params$n[li],wts=x@params$wts)
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

# addDEMA {{{
`addDEMA` <- function(n=10,on=1,with.col=Cl,overlay=TRUE,col='pink') {


  lchob <- get.current.chob()
  chobTA <- new("chobTA")
  chobTA@new <- !overlay

  # get the appropriate data - from the approp. src
  if(on==1) {
    x <- as.matrix(lchob@xdata)

    if(!is.OHLC(x) && missing(with.col)) with.col <- 1

    if(is.function(with.col)) {
      x.tmp <- do.call(with.col,list(x))
    } else x.tmp <- x[,with.col]
  } else {
    # get values from TA...
    which.TA <- which(sapply(lchob@passed.args$TA,function(x) x@new))
    target.TA <- eval(lchob@passed.args$TA[which.TA][on-1])[[1]]
    x <- as.matrix(target.TA@TA.values)

    if(missing(with.col)) with.col <- 1

    if(is.function(with.col)) {
      x.tmp <- do.call(with.col,list(x))
    } else x.tmp <- x[,with.col]
  }

  chobTA@TA.values <- x.tmp[lchob@xsubset]
  chobTA@name <- "chartDEMA"
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
                        col=col,n=n)
  if(is.null(sys.call(-1))) {
    TA <- lchob@passed.args$TA
    lchob@passed.args$TA <- c(TA,chobTA)
    lchob@windows <- lchob@windows + ifelse(chobTA@new,1,0)
    do.call('chartSeries.chob',list(lchob))
    invisible(chobTA)
  } else {
   return(chobTA)
  } 
} # }}}
# chartDEMA {{{
`chartDEMA` <-
function(x) {
    spacing <- x@params$spacing
    width <- x@params$width

    x.range <- x@params$xrange
    x.range <- seq(x.range[1],x.range[2]*spacing)

    multi.col <- x@params$multi.col
    color.vol <- x@params$color.vol

    if(length(x@params$n) < length(x@params$col)) {
      colors <- 3:10
    } else colors <- x@params$col
 
    for(li in 1:length(x@params$n)) {
      ma <- DEMA(x@TA.values,n=x@params$n[li])
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

# addEVWMA {{{
`addEVWMA` <- function(n=10,on=1,with.col=Cl,overlay=TRUE,col='yellow') {


  lchob <- get.current.chob()
  chobTA <- new("chobTA")
  chobTA@new <- !overlay

  # get the appropriate data - from the approp. src
  if(on==1) {
    x <- as.matrix(lchob@xdata)

    if(!is.OHLC(x) && missing(with.col)) with.col <- 1

    if(is.function(with.col)) {
      x.tmp <- cbind(do.call(with.col,list(x)),Vo(x))
    } else x.tmp <- x[,with.col]
  } else {
    # get values from TA...
    which.TA <- which(sapply(lchob@passed.args$TA,function(x) x@new))
    target.TA <- eval(lchob@passed.args$TA[which.TA][on-1])[[1]]
    x <- as.matrix(target.TA@TA.values)

    if(missing(with.col)) with.col <- 1

    if(is.function(with.col)) {
      x.tmp <- do.call(with.col,list(x))
    } else x.tmp <- x[,with.col]
  }
  if(!has.Vo(x)) return()

  chobTA@TA.values <- cbind(x.tmp,Vo(x))[lchob@xsubset,] # Price + Volume
  chobTA@name <- "chartEVWMA"
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
                        col=col,n=n)
  if(is.null(sys.call(-1))) {
    TA <- lchob@passed.args$TA
    lchob@passed.args$TA <- c(TA,chobTA)
    lchob@windows <- lchob@windows + ifelse(chobTA@new,1,0)
    do.call('chartSeries.chob',list(lchob))
    invisible(chobTA)
  } else {
   return(chobTA)
  } 
} # }}}
# chartEVWMA {{{
`chartEVWMA` <-
function(x) {
    spacing <- x@params$spacing
    width <- x@params$width

    x.range <- x@params$xrange
    x.range <- seq(x.range[1],x.range[2]*spacing)

    multi.col <- x@params$multi.col
    color.vol <- x@params$color.vol

    if(length(x@params$n) < length(x@params$col)) {
      colors <- 3:10
    } else colors <- x@params$col
 
    for(li in 1:length(x@params$n)) {
      ma <- EVWMA(x@TA.values[,1],x@TA.values[,2],n=x@params$n[li])
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

# addZLEMA {{{
`addZLEMA` <- function(n=10,ratio=NULL,on=1,with.col=Cl,overlay=TRUE,col='red') {


  lchob <- get.current.chob()
  chobTA <- new("chobTA")
  chobTA@new <- !overlay

  # get the appropriate data - from the approp. src
  if(on==1) {
    x <- as.matrix(lchob@xdata)

    if(!is.OHLC(x) && missing(with.col)) with.col <- 1

    if(is.function(with.col)) {
      x.tmp <- do.call(with.col,list(x))
    } else x.tmp <- x[,with.col]
  } else {
    # get values from TA...
    which.TA <- which(sapply(lchob@passed.args$TA,function(x) x@new))
    target.TA <- eval(lchob@passed.args$TA[which.TA][on-1])[[1]]

    if(missing(with.col)) with.col <- 1

    x <- as.matrix(target.TA@TA.values)
    if(missing(with.col)) {
      warning('missing "with.col" argument')
      invisible(return())
    }
    if(is.function(with.col)) {
      x.tmp <- do.call(with.col,list(x))
    } else x.tmp <- x[,with.col]
  }

  chobTA@TA.values <- x.tmp[lchob@xsubset]
  chobTA@name <- "chartZLEMA"
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
                        col=col,n=n,ratio=ratio)
  if(is.null(sys.call(-1))) {
    TA <- lchob@passed.args$TA
    lchob@passed.args$TA <- c(TA,chobTA)
    lchob@windows <- lchob@windows + ifelse(chobTA@new,1,0)
    do.call('chartSeries.chob',list(lchob))
    invisible(chobTA)
  } else {
   return(chobTA)
  } 
} # }}}
# chartZLEMA {{{
`chartZLEMA` <-
function(x) {
    spacing <- x@params$spacing
    width <- x@params$width

    x.range <- x@params$xrange
    x.range <- seq(x.range[1],x.range[2]*spacing)

    multi.col <- x@params$multi.col
    color.vol <- x@params$color.vol

    if(length(x@params$n) < length(x@params$col)) {
      colors <- 3:10
    } else colors <- x@params$col
 
    for(li in 1:length(x@params$n)) {
      ma <- ZLEMA(x@TA.values,n=x@params$n[li],ratio=x@params$ratio)
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
`addExpiry` <- function(type='options',lty='dotted') {
  lchob <- get.current.chob()
  chobTA <- new("chobTA")
  chobTA@new <- FALSE

  # get the appropriate data - from the approp. src
  #if(from.fig==1) {
  x <- lchob@xdata

  if(type=='options') {
    index.of.exp <- options.expiry(x)
  } else index.of.exp <- futures.expiry(x)

  chobTA@TA.values <- index.of.exp[index.of.exp %in% lchob@xsubset] # single numeric vector
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
  if(is.null(sys.call(-1))) {
    TA <- lchob@passed.args$TA
    lchob@passed.args$TA <- c(TA,chobTA)
    lchob@windows <- lchob@windows + ifelse(chobTA@new,1,0)
    do.call('chartSeries.chob',list(lchob))
    invisible(chobTA)
  } else {
   return(chobTA)
  } 
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
      abline(v=x@TA.values[ex]*spacing,lty=x@params$lty,col=x@params$colors$Expiry)
    }
} # }}}

# get.current.chob {{{
`get.current.chob` <- function() {
  first.chob <- which(sapply(sys.frames(),function(x) exists('chob',envir=x)))[1]
  if(!is.na(first.chob)) {
    lchob <- get('chob',envir=first.chob)

#  if(exists('chob',envir=sys.frames()[[sys.parent()]])) {
#    if(identical(sys.frames()[[sys.parent()]],.GlobalEnv)) 
#      stop("why are you calling this directly?")
#    lchob <- get('chob',envir=sys.frames()[[sys.parent()]])
  } else {
    gchob <- get.chob()
    #protect against NULL device or windows not drawn to yet
    if(dev.cur()==1 || length(gchob) < dev.cur()) 
      stop("improperly set or missing graphics device")
    current.chob <- which(sapply(gchob,
                                 function(x) {
                                   ifelse(class(x)=="chob" &&
                                   x@device==as.numeric(dev.cur()),TRUE,FALSE)
                                 }))
    if(identical(current.chob,integer(0))) stop("no current plot")
    lchob <- gchob[[current.chob]]
  }
  return(lchob)
} #}}}
