# setTA {{{
`setTA` <-
function(type=c('chartSeries','barChart','candleChart')) {
  if('chartSeries' %in% type) setDefaults(chartSeries,TA=listTA())
  if('barChart' %in% type) setDefaults(barChart,TA=listTA())
  if('candleChart' %in% type) setDefaults(candleChart,TA=listTA())
}# }}}
# unsetTA {{{
`unsetTA` <-
function(type=c('chartSeries','barChart','candleChart')) {
  if('chartSeries' %in% type) setDefaults(chartSeries,TA=NULL)
  if('barChart' %in% type) setDefaults(barChart,TA=NULL)
  if('candleChart' %in% type) setDefaults(candleChart,TA=NULL)
}# }}}
# listTA {{{
`listTA` <-
function(dev) {
  if(missing(dev)) dev <- dev.cur()
  sapply(get.chob()[[dev]]@passed.args$TA,function(x) x@call)
} # }}}

# addVo {{{
`addVo` <- function() {
   lchob <- get.current.chob() 
  if(!lchob@show.vol) return()
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
  chobTA@params$thin <- ifelse(lchob@type %in% c('bars','matchsticks'),TRUE,FALSE)
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
    plot(x.range,seq(min(Volumes),max(Volumes),
         length.out=length(x.range)),
         type='n',axes=FALSE,ann=FALSE)
    grid(NA,NULL,col=x@params$colors$grid.col)
    x.pos <- 1 + spacing * (1:length(Volumes) - 1)
    if(!x@params$color.vol | !is.null(x@params$colors$Vo.bar.col)) {
      bar.col <- ifelse(!is.null(x@params$colors$Vo.bar.col),
                        x@params$colors$Vo.bar.col,
                        x@params$colors$border)
    } else
    if (x@params$multi.col) {
      last.Closes <- as.numeric(quantmod::Lag(Closes))
      last.Closes[1] <- Closes[1]
      bar.col <- ifelse(Opens < Closes, 
                        ifelse(Opens > last.Closes, 
                               x@params$colors$dn.up.col,
                               x@params$colors$up.up.col),
                        ifelse(Opens < last.Closes,
                               x@params$colors$dn.dn.col,
                               x@params$colors$up.dn.col))
    } else {
      bar.col <- ifelse(Opens < Closes, 
                        x@params$colors$up.col,
                        x@params$colors$dn.col)
    }

    # if border=NULL use color of bar
    border.col <- ifelse(is.null(x@params$colors$border),bar.col,x@params$colors$border)

    if(x@params$thin) {
      # plot thin volume bars if appropriate
      segments(x.pos,0,x.pos,Volumes,col=bar.col)
    } else {
      rect(x.pos-spacing/3,0,x.pos+spacing/3,Volumes,
           col=bar.col,border=border.col)
    }

    text(0, max(Volumes,na.rm=TRUE) * .9, "Volume:",pos=4)

    text(0, max(Volumes,na.rm=TRUE) * .9,
         paste("\n\n\n",last(Volumes)*vol.scale[[1]], sep = ""), 
         pos = 4,col=last(bar.col))

    axis(2)
    box(col=x@params$colors$fg.col)
} # }}}

# addSMI {{{
`addSMI` <- function(n=13,slow=25,fast=2,signal=9,ma.type='EMA') {

  stopifnot("package:TTR" %in% search() || require("TTR",quietly=TRUE))

  lchob <- get.current.chob()
  x <- as.matrix(eval(lchob@passed.args$x))
  chobTA <- new("chobTA")
  chobTA@new <- TRUE

  xx <- if(is.OHLC(x)) {
    cbind(Hi(x),Lo(x),Cl(x))
  } else x 

  smi <- SMI(xx, n=n, nFast=fast,
             nSlow=slow, nSig=signal, maType=ma.type)

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
                        n=n,slow=slow,fast=fast,signal=signal,
                        ma.type=ma.type)
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
# chartSMI {{{
`chartSMI` <-
function(x) {
    spacing <- x@params$spacing
    width <- x@params$width

    x.range <- x@params$xrange
    x.range <- seq(x.range[1],x.range[2]*spacing)

    multi.col <- x@params$multi.col
    color.vol <- x@params$color.vol

    smi <- x@TA.values

    y.range <- seq(-max(abs(smi[,1]), na.rm = TRUE), max(abs(smi[,1]), 
                   na.rm = TRUE), length.out = length(x.range)) * 1.05

    plot(x.range,y.range,type='n',axes=FALSE,ann=FALSE)

    grid(NA,NULL,col=x@params$colors$grid.col)

    COLOR <- "#0033CC"
    SIGNAL <- "#BFCFFF"

    lines(seq(1,length(x.range),by=spacing),
          smi[,1],col=COLOR,lwd=1,type='l')
    lines(seq(1,length(x.range),by=spacing),
          smi[,2],col=SIGNAL,lwd=1,lty='dotted',type='l')

    text(0, last(y.range) * .9,
         paste("Stochastic Momentum Index (",
         paste(x@params$n,x@params$fast,x@params$slow,x@params$signal,sep=','),
         "):", sep = ""), 
         pos = 4)

    text(0, last(y.range)*.9,
        paste("\n\n\nSMI: ",sprintf("%.3f",last(smi[,1])), sep = ""), col = COLOR, 
        pos = 4)

    text(0, last(y.range)*.9,
        paste("\n\n\n\n\nSignal: ",
              sprintf("%.3f",last(smi[,2])), sep = ""), col = SIGNAL, 
        pos = 4)

    axis(2)
    box(col=x@params$colors$fg.col)
} # }}}

# addWPR {{{
`addWPR` <- function(n=14) {

  stopifnot("package:TTR" %in% search() || require("TTR",quietly=TRUE))

  lchob <- get.current.chob()
  x <- as.matrix(eval(lchob@passed.args$x))
  chobTA <- new("chobTA")
  chobTA@new <- TRUE

  xx <- if(is.OHLC(x)) {
    cbind(Hi(x),Lo(x),Cl(x))
  } else x 

  wpr <- WPR(x,n=n)

  chobTA@TA.values <- as.numeric(wpr)
  chobTA@name <- "chartWPR"
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
# chartWPR {{{
`chartWPR` <-
function(x) {
    spacing <- x@params$spacing
    width <- x@params$width

    x.range <- x@params$xrange
    x.range <- seq(x.range[1],x.range[2]*spacing)

    multi.col <- x@params$multi.col
    color.vol <- x@params$color.vol

    n <- x@params$n
    wpr <- x@TA.values

    y.range <- seq(-max(abs(wpr), na.rm = TRUE), max(abs(wpr), 
                   na.rm = TRUE), length.out = length(x.range)) * 1.05

    # create appropriately scaled empty plot area
    plot(x.range,y.range,type='n',axes=FALSE,ann=FALSE)

    grid(NA,NULL,col=x@params$colors$grid.col)

    COLOR <- "#0033CC"

    lines(seq(1,length(x.range),by=spacing),wpr,col=COLOR,lwd=1,type='l')

    text(0, last(y.range)*.9,
         paste("Williams %R (", x@params$n,"):", sep = ""), 
        pos = 4)

    text(0, last(y.range)*.9,
        paste("\n\n\n",sprintf("%.3f",last(wpr)), sep = ""), col = COLOR, 
        pos = 4)

    axis(2)
    box(col=x@params$colors$fg.col)
} # }}}

# addCMF {{{
`addCMF` <- function(n=20) {

  stopifnot("package:TTR" %in% search() || require("TTR",quietly=TRUE))

  lchob <- get.current.chob()
  x <- as.matrix(eval(lchob@passed.args$x))
  chobTA <- new("chobTA")
  chobTA@new <- TRUE

  xx <- if(is.OHLC(x)) {
    cbind(Hi(x),Lo(x),Cl(x))
  } else stop("CMF only applicaple to HLC series")

  cmf <- CMF(xx,Vo(x),n=n)

  chobTA@TA.values <- cmf
  chobTA@name <- "chartCMF"
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
# chartCMF {{{
`chartCMF` <-
function(x) {
    spacing <- x@params$spacing
    width <- x@params$width

    x.range <- x@params$xrange
    x.range <- seq(x.range[1],x.range[2]*spacing)

    multi.col <- x@params$multi.col
    color.vol <- x@params$color.vol

    n <- x@params$n
    cmf <- x@TA.values

    y.range <- seq(-max(abs(cmf), na.rm = TRUE), max(abs(cmf), 
                   na.rm = TRUE), length.out = length(x.range)) * 1.05

    plot(x.range,y.range,
         type='n',axes=FALSE,ann=FALSE)
    grid(NA,NULL,col=x@params$colors$grid.col)

    xx <- seq(1,length(x.range),by=spacing)
    cmf.positive <- ifelse(cmf >= 0,cmf,0)
    cmf.negative <- ifelse(cmf <  0,cmf,0)

    polygon(c(xx,rev(xx)),c(cmf.positive,rep(0,length(cmf))),col=x@params$colors$up.col)
    polygon(c(xx,rev(xx)),c(cmf.negative,rep(0,length(cmf))),col=x@params$colors$dn.col)

    abline(h=0,col="#999999")

    text(0, last(y.range)*.9,
         paste("Chaiken Money Flow (", x@params$n,"):", sep = ""), 
        pos = 4)

    text(0, last(y.range)*.9,
        paste("\n\n\n",sprintf("%.3f",last(cmf)), sep = ""), 
        col = ifelse(last(cmf) > 0,x@params$colors$up.col,x@params$colors$dn.col), 
        pos = 4)

    axis(2)
    box(col=x@params$colors$fg.col)
} # }}}

# addCMO {{{
`addCMO` <- function(n=14) {

  stopifnot("package:TTR" %in% search() || require("TTR",quietly=TRUE))

  lchob <- get.current.chob()
  x <- as.matrix(eval(lchob@passed.args$x))
  chobTA <- new("chobTA")
  chobTA@new <- TRUE

  #  needs to accept any arguments for x, not just close

  xx <- if(is.OHLC(x)) {
    Cl(x)
  } else x 

  cmo <- CMO(xx,n=n)

  chobTA@TA.values <- cmo
  chobTA@name <- "chartCMO"
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
# chartCMO {{{
`chartCMO` <-
function(x) {
    spacing <- x@params$spacing
    width <- x@params$width

    x.range <- x@params$xrange
    x.range <- seq(x.range[1],x.range[2]*spacing)

    multi.col <- x@params$multi.col
    color.vol <- x@params$color.vol

    n <- x@params$n
    cmo <- x@TA.values

    y.range <- seq(-max(abs(cmo), na.rm = TRUE), max(abs(cmo), 
                   na.rm = TRUE), length.out = length(x.range)) * 1.05

    plot(x.range,y.range,
         type='n',axes=FALSE,ann=FALSE)
    grid(NA,NULL,col=x@params$colors$grid.col)

    COLOR="#0033CC"

    abline(h=0,col="#666666",lwd=1,lty='dotted')
    lines(seq(1,length(x.range),by=spacing),cmo,col=COLOR,lwd=1,type='l')

    text(0, last(y.range)*.9,
         paste("Chande Momentum Oscillator (", x@params$n,"):", sep = ""), 
        pos = 4)

    text(0, last(y.range)*.9,
        paste("\n\n\n",sprintf("%.3f",last(cmo)), sep = ""), col = COLOR, 
        pos = 4)

    axis(2)
    box(col=x@params$colors$fg.col)
} # }}}

# addMomentum {{{
`addMomentum` <- function(n=1) {

  stopifnot("package:TTR" %in% search() || require("TTR",quietly=TRUE))

  lchob <- get.current.chob()
  x <- as.matrix(eval(lchob@passed.args$x))
  chobTA <- new("chobTA")
  chobTA@new <- TRUE

  #  needs to accept any arguments for x, not just close

  xx <- if(is.OHLC(x)) {
    Cl(x)
  } else x 

  mom <- momentum(xx,n=n)

  chobTA@TA.values <- mom
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

  stopifnot("package:TTR" %in% search() || require("TTR",quietly=TRUE))

  lchob <- get.current.chob()
  x <- as.matrix(eval(lchob@passed.args$x))
  chobTA <- new("chobTA")
  chobTA@new <- TRUE

  xx <- if(is.OHLC(x)) {
    cbind(Hi(x),Lo(x),Cl(x))
  } else x 

  cci <- CCI(xx,n=n,maType=maType,c=c)

  chobTA@TA.values <- cci
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
    grid(NA,NULL,col=x@params$colors$grid.col)

    usr <- par('usr')

    # draw shading in -100:100 y-range 
    rect(usr[1],-100,usr[2],100,col="#282828")

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

  stopifnot("package:TTR" %in% search() || require("TTR",quietly=TRUE))

  lchob <- get.current.chob()
  x <- as.matrix(eval(lchob@passed.args$x))
  chobTA <- new("chobTA")
  chobTA@new <- TRUE

  if(!is.OHLC(x)) stop("only applicable to HLC series")

  adx <- ADX(cbind(Hi(x),Lo(x),Cl(x)),n=n,maType=maType,wilder=wilder)

  chobTA@TA.values <- adx
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
`addATR` <- function(n=14, maType="EMA", wilder=TRUE) {

  stopifnot("package:TTR" %in% search() || require("TTR",quietly=TRUE))

  lchob <- get.current.chob()
  x <- as.matrix(eval(lchob@passed.args$x))
  chobTA <- new("chobTA")
  chobTA@new <- TRUE

  if(!is.OHLC(x)) stop("only applicable to HLC series")

  atr <- ATR(cbind(Hi(x),Lo(x),Cl(x)),n=n,maType=maType,wilder=wilder)

  chobTA@TA.values <- atr
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
    grid(NA,NULL,col=x@params$colors$grid.col)

    # draw ADX
    lines(seq(1,length(x.range),by=spacing),atr[,2],col='blue',lwd=2,type='l')

    axis(2)
    box(col=x@params$colors$fg.col)
} # }}}

# addTRIX {{{
`addTRIX` <- function(n=20, signal=9, maType="EMA", percent=TRUE) {

  stopifnot("package:TTR" %in% search() || require("TTR",quietly=TRUE))

  lchob <- get.current.chob()
  x <- as.matrix(eval(lchob@passed.args$x))
  chobTA <- new("chobTA")
  chobTA@new <- TRUE

  xx <- if(is.OHLC(x)) {
    Cl(x)
  } else x 

  trix <- TRIX(xx,n=n,nSig=signal,maType=maType,percent=percent)

  chobTA@TA.values <- trix
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

  stopifnot("package:TTR" %in% search() || require("TTR",quietly=TRUE))

  lchob <- get.current.chob()
  x <- as.matrix(eval(lchob@passed.args$x))
  chobTA <- new("chobTA")
  chobTA@new <- TRUE
 
  # should really allow for _any_ series to be used, like MA (FIXME)

  xx <- if(is.OHLC(x)) {
    Cl(x)
  } else x 

  dpo <- DPO(xx,n=n,maType=maType,shift=shift,percent=percent)

  chobTA@TA.values <- dpo
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
    plot(x.range,seq(min(dpo*.975,na.rm=TRUE),
         max(dpo*1.05,na.rm=TRUE),length.out=length(x.range)),
         type='n',axes=FALSE,ann=FALSE)
    grid(NA,NULL,col=x@params$colors$grid.col)

    # draw DPO
    lines(seq(1,length(x.range),by=spacing),dpo,col='green',lwd=1,type='l')

    #title(ylab=paste('SMI(',paste(param,collapse=','),')',sep=''))
    axis(2)
    box(col=x@params$colors$fg.col)
} # }}}

# addRSI {{{
`addRSI` <- function(n=14,type='EMA',wilder=TRUE) {

  stopifnot("package:TTR" %in% search() || require("TTR",quietly=TRUE))

  lchob <- get.current.chob()
  x <- as.matrix(eval(lchob@passed.args$x))
  chobTA <- new("chobTA")
  chobTA@new <- TRUE


  xx <- if(is.OHLC(x)) {
    Cl(x)
  } else x 

  rsi <- RSI(xx,n=n,maType=type,wilder=wilder)
  chobTA@TA.values <- rsi
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
                        n=n, wilder=wilder,maType=type)
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

    plot(x.range,y.range,type='n',axes=FALSE,ann=FALSE)

    grid(NA,NULL,col=x@params$colors$grid.col)

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

  stopifnot("package:TTR" %in% search() || require("TTR",quietly=TRUE))

  lchob <- get.current.chob()
  x <- as.matrix(eval(lchob@passed.args$x))
  chobTA <- new("chobTA")
  chobTA@new <- TRUE

  xx <- if(is.OHLC(x)) {
    Cl(x)
  } else x 

  roc <- ROC(xx,n=n,type=type,na=NA)

  chobTA@TA.values <- roc
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
    plot(x.range,seq(min(roc*.975,na.rm=TRUE),max(roc*1.05,na.rm=TRUE),length.out=length(x.range)),
         type='n',axes=FALSE,ann=FALSE)
    grid(NA,NULL,col=x@params$colors$grid.col)
    lines(seq(1,length(x.range),by=spacing),roc,col=x@params$col,lwd=2,type='l')
    #title(ylab=paste('RSI(',paste(c(n.up,collapse=','),')',sep=''))
    axis(2)
    box(col=x@params$colors$fg.col)
} # }}}

# addBBands {{{
`addBBands` <- function(n=20,ma='SMA',sd=2,on=-1) {

  stopifnot("package:TTR" %in% search() || require("TTR",quietly=TRUE))

  lchob <- get.current.chob()
  x <- as.matrix(eval(lchob@passed.args$x))
  chobTA <- new("chobTA")
  chobTA@new <- FALSE


  x <- as.matrix(eval(lchob@passed.args$x))

  xx <- if(is.OHLC(x)) {
    cbind(Hi(x),Lo(x),Cl(x))
  } else x 

  bb <- BBands(xx,n=n,maType=ma,sd=sd)
  
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
    if(x@on[1] > 0) {
      lines(seq(1,length(x.range),by=spacing),bb[,1],col='red',lwd=1,lty='dashed')
      lines(seq(1,length(x.range),by=spacing),bb[,3],col='red',lwd=1,lty='dashed')
      #lines(seq(1,length(x.range),by=spacing),bb[,2],col='grey',lwd=1,lty='dotted')
    } else {
      xx <- seq(1,length(x.range),by=spacing)
      polygon(c(xx,rev(xx)), c(bb[,1],rev(bb[,3])),col='#282828',border=NA)
      lines(seq(1,length(x.range),by=spacing),bb[,1],col='red',lwd=1,lty='dashed')
      lines(seq(1,length(x.range),by=spacing),bb[,3],col='red',lwd=1,lty='dashed')
      #lines(seq(1,length(x.range),by=spacing),bb[,2],col='grey',lwd=1,lty='dotted')
    }
   
    # return the text to be pasted
    invisible(list(text=paste("Bollinger Bands (",
                   paste(x@params$n,x@params$sd,sep=","),") [Upper/Lower]: ",
                   sprintf("%.3f",last(bb[,3])),"/",
                   sprintf("%.3f",last(bb[,1])), sep = ""), col = 'red')) 

} # }}}

# addSAR {{{
`addSAR` <- function(accel=c(0.02,0.2),col='blue') {

  stopifnot("package:TTR" %in% search() || require("TTR",quietly=TRUE))

  lchob <- get.current.chob()
  x <- as.matrix(eval(lchob@passed.args$x))
  chobTA <- new("chobTA")
  chobTA@new <- FALSE


  x <- as.matrix(eval(lchob@passed.args$x))

  if(!is.OHLC(x)) stop("SAR requires HL series") 

  sar <- SAR(cbind(Hi(x),Lo(x)),accel=accel)

  chobTA@TA.values <- sar
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

  stopifnot("package:TTR" %in% search() || require("TTR",quietly=TRUE))

  lchob <- get.current.chob()
  x <- as.matrix(eval(lchob@passed.args$x))
  chobTA <- new("chobTA")
  chobTA@new <- TRUE

  col <- if(missing(col)) col <- c('#999999','#777777',
                              '#BBBBBB','#FF0000')

  xx <- if(is.OHLC(x)) {
    Cl(x)
  } else x 

  macd <- MACD(xx,nFast=fast,nSlow=slow,nSig=signal,maType=type)
  
  chobTA@TA.values <- macd
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

    plot(x.range,y.range,type='n',axes=FALSE,ann=FALSE)

    grid(NA,NULL,col=x@params$colors$grid.col)

    if(x@params$histo) {
      x.pos <- 1 + spacing * (1:NROW(macd) -1)
      cols <- ifelse((macd[,1]-macd[,2]) > 0, col[1],col[2])
      rect(x.pos - spacing/5,0,x.pos + spacing/5, macd[,1]-macd[,2],
           col=cols,border=cols)
    } 

    lines(seq(1,length(x.range),by=spacing),macd[,1],col=col[3],lwd=1)
    lines(seq(1,length(x.range),by=spacing),macd[,2],col=col[4],lwd=1,lty='dotted')

    text(0, last(y.range)*.9,
         paste("Moving Average Convergence Divergence (",
         paste(x@params$fast,x@params$slow,x@params$signal,sep=','),"):", sep = ""), 
         pos = 4)

    text(0, last(y.range)*.9,
        paste("\n\n\nMACD: ",sprintf("%.3f",last(macd[,1])), sep = ""),
        col = col[3],pos = 4)

    text(0, last(y.range)*.9,
        paste("\n\n\n\n\n\nSignal: ",sprintf("%.3f",last(macd[,2])), sep = ""),
        col = col[4],pos = 4)

    axis(2)
    box(col=x@params$colors$fg.col)
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

    }
    if(!is.null(x@params$h)) {
      # draw horizontal lines given positions specified in h
      if(length(x@params$h) > length(x@params$col)) {
        colors <- 3:10
      } else colors <- x@params$col
      for(li in 1:length(x@params$h)) {
        lines(seq(1,length(x.range),by=spacing),
              rep(x@params$h[li],length(x.range)/spacing), col=colors[li])
      }
    }
    if(!is.null(x@params$v)) {
      # draw vertical lines given positions specified in v
      if(length(x@params$v) > length(x@params$col)) {
        colors <- 3:10
      } else colors <- x@params$col
      for(li in 1:length(x@params$v)) {
        abline(v=x@params$v[li]*spacing,col=colors[li])
      }
    }

} # }}}

# addEMA {{{
`addEMA` <- function(n=10,wilder=FALSE,ratio=NULL,on=1,with.col=Cl,overlay=TRUE,col='blue') {

  stopifnot("package:TTR" %in% search() || require("TTR",quietly=TRUE))

  lchob <- get.current.chob()
  chobTA <- new("chobTA")
  chobTA@new <- !overlay


  # get the appropriate data - from the approp. src
  if(on==1) {
    x <- as.matrix(eval(lchob@passed.args$x))

    if(!is.OHLC(x) | missing(with.col)) with.col <- 1

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

  chobTA@TA.values <- ma.tmp 
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
        title(ylab=paste('EMA(',paste(x@params$n[li],collapse=','),')',sep=''))
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

  stopifnot("package:TTR" %in% search() || require("TTR",quietly=TRUE))

  lchob <- get.current.chob()
  chobTA <- new("chobTA")
  chobTA@new <- !overlay

  # get the appropriate data - from the approp. src
  if(on==1) {
    x <- as.matrix(eval(lchob@passed.args$x))

    if(!is.OHLC(x) | missing(with.col)) with.col <- 1

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

  chobTA@TA.values <- ma.tmp # single numeric vector
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
        title(ylab=paste('EMA(',paste(x@params$n[li],collapse=','),')',sep=''))
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

  stopifnot("package:TTR" %in% search() || require("TTR",quietly=TRUE))

  lchob <- get.current.chob()
  chobTA <- new("chobTA")
  chobTA@new <- !overlay

  # get the appropriate data - from the approp. src
  if(on==1) {
    x <- as.matrix(eval(lchob@passed.args$x))

    if(!is.OHLC(x) | missing(with.col)) with.col <- 1

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

  chobTA@TA.values <- x.tmp # single numeric vector
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

  stopifnot("package:TTR" %in% search() || require("TTR",quietly=TRUE))

  lchob <- get.current.chob()
  chobTA <- new("chobTA")
  chobTA@new <- !overlay

  # get the appropriate data - from the approp. src
  if(on==1) {
    x <- as.matrix(eval(lchob@passed.args$x))

    if(!is.OHLC(x) | missing(with.col)) with.col <- 1

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

  chobTA@TA.values <- x.tmp # single numeric vector
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

  stopifnot("package:TTR" %in% search() || require("TTR",quietly=TRUE))

  lchob <- get.current.chob()
  chobTA <- new("chobTA")
  chobTA@new <- !overlay

  # get the appropriate data - from the approp. src
  if(on==1) {
    x <- as.matrix(eval(lchob@passed.args$x))

    if(!is.OHLC(x) | missing(with.col)) with.col <- 1

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

  chobTA@TA.values <- cbind(x.tmp,Vo(x)) # Price + Volume
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

  stopifnot("package:TTR" %in% search() || require("TTR",quietly=TRUE))

  lchob <- get.current.chob()
  chobTA <- new("chobTA")
  chobTA@new <- !overlay

  # get the appropriate data - from the approp. src
  if(on==1) {
    x <- as.matrix(eval(lchob@passed.args$x))

    if(!is.OHLC(x) | missing(with.col)) with.col <- 1

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

  chobTA@TA.values <- x.tmp # single numeric vector
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
`addExpiry` <- function(type='options',lty='dotted',col='blue') {
  lchob <- get.current.chob()
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
      abline(v=x@TA.values[ex]*spacing,lty=x@params$lty,col=x@params$col)
    }
} # }}}

# get.current.chob {{{
`get.current.chob` <- function() {
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
  return(lchob)
} #}}}
