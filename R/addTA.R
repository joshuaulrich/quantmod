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

    text(0, max(Volumes, na.rm = TRUE)*.9, paste("Last: ",
         last(Volumes)*vol.scale[[1]], sep = ""), 
         pos = 4)

    title(ylab=paste("volume (",vol.scale[[2]],")"))
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

# smi <- SMI(cbind(Hi(x),Lo(x),Cl(x)),n=n,ma.slow=list(ma.type[1],n=slow),
#            ma.fast=list(ma.type[2],n=fast),
#            ma.sig=list(ma.type[3],n=signal))

  smi <- SMI(cbind(Hi(x),Lo(x),Cl(x)), n=n, nFast=fast,
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
    plot(x.range,
         seq(min(smi[,1]*.975,na.rm=TRUE),
             max(smi[,1]*1.05,na.rm=TRUE),
             length.out=length(x.range)),
         type='n',axes=FALSE,ann=FALSE)

    grid(NA,NULL,col=x@params$colors$grid.col)

    COLOR <- "#0033CC"

    lines(seq(1,length(x.range),by=spacing),
          smi[,1],col='#0033CC',lwd=2,type='l')
    lines(seq(1,length(x.range),by=spacing),
          smi[,2],col='#BFCFFF',lwd=1,lty='dotted',type='l')

    text(0, max(smi[,1], na.rm = TRUE)*.9, paste("Stochastic Momentum Index (",
         paste(x@params$n,x@params$fast,x@params$slow,x@params$signal,sep=','), 
        "):  ", sprintf("%.3f",last(smi[,1])), sep = ""), 
        col = COLOR, 
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

  wpr <- WPR(cbind(Hi(x),Lo(x),Cl(x)),n=n)

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
    plot(x.range,
         seq(min(wpr*.975,na.rm=TRUE),max(wpr*1.05,na.rm=TRUE),
         length.out=length(x.range)),
         type='n',axes=FALSE,ann=FALSE)

    grid(NA,NULL,col=x@params$colors$grid.col)

    COLOR <- "#0033CC"

    lines(seq(1,length(x.range),by=spacing),wpr,col=COLOR,lwd=2,type='l')

    text(0,max(wpr,na.rm=TRUE)*.9,
         paste("Williams %R (",x@params$n,"):  ",sprintf("%.3f",last(wpr)),sep=""),
         col=COLOR,pos=4)
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

  cmf <- CMF(cbind(Hi(x),Lo(x),Cl(x)),Vo(x),n=n)

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
    plot(x.range,seq(min(cmf*.975,na.rm=TRUE),
         max(cmf*1.05,na.rm=TRUE),length.out=length(x.range)),
         type='n',axes=FALSE,ann=FALSE)
    grid(NA,NULL,col=x@params$colors$grid.col)

    COLOR <- "#0033CC"

    abline(h=0,col="#999999")
    lines(seq(1,length(x.range),by=spacing),cmf,col=COLOR,lwd=2,type='l')

    text(0, max(cmf, na.rm = TRUE)*.9,
         paste("Chaiken Money Flow(", x@params$n, "):  ",
         sprintf("%.3f",last(cmf)), sep = ""), col = COLOR, 
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

  cmo <- CMO(Cl(x),n=n)

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
    lines(seq(1,length(x.range),by=spacing),cmo,col=COLOR,lwd=2,type='l')

    text(0, max(cmo, na.rm = TRUE)*.9,
         paste("Chande Momentum Osc (", x@params$n,"):", sep = ""), 
        pos = 4)

    text(0, max(cmo, na.rm = TRUE)*.9,
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

  mom <- momentum(Cl(x),n=n)

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

    text(0, max(abs(mom), na.rm = TRUE)*.9,
         paste("Momentum (", x@params$n, "):"),pos=4)

    text(0, max(abs(mom), na.rm = TRUE)*.9,
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

  cci <- CCI(cbind(Hi(x),Lo(x),Cl(x)),n=n,maType=maType,c=c)

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

    # set xx to the full width of the window
    xx <- seq(usr[1],usr[2]+width,by=spacing)

    # draw shading in -100:100 y-range 
    polygon(c(xx,rev(xx)),c(rep(100,length(xx)),rep(-100,length(xx))),col="#282828")

    # draw CCI
    lines(seq(1,length(x.range),by=spacing),cci,col='red',lwd=1,type='l')

    # draw dotted guide line at 0
    abline(h=0,col='#666666',lwd=1,lty='dotted')

    # add indicator name and last value
    text(0, max(abs(cci), na.rm = TRUE)*.9,
         paste("Commodity Channel Index (", x@params$n, ",",
         x@params$c,"):",sep=''),pos=4)
    text(0, max(abs(cci), na.rm = TRUE)*.9 - par('cex'),
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

  trix <- TRIX(Cl(x),n=n,nSig=signal,maType=maType,percent=percent)

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

    #title(ylab=paste('SMI(',paste(param,collapse=','),')',sep=''))
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

  dpo <- DPO(Cl(x),n=n,maType=maType,shift=shift,percent=percent)

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

  n.up <- n[1]; n.dn <- ifelse(length(n) > 1,n[2],n[1])
  type.up <- type[1]; type.dn <- ifelse(length(type) > 1,type[2],type[1])
  wilder.up <- wilder[1]; wilder.dn <- ifelse(length(wilder) > 1,wilder[2],wilder[1])

# rsi <- RSI(Cl(x),ma.up=list(type.up,n=n.up,wilder=wilder.up),
#                    ma.down=list(type.dn,n=n.dn,wilder=wilder.dn))
  rsi <- RSI(Cl(x),n=n,maType=type,wilder=wilder)
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
                        n.up=n.up,n.dn=n.dn,type.up=type.up,type.dn=type.dn,
                        wilder.up=wilder.up,wilder.dn=wilder.dn)
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
    plot(x.range,seq(min(rsi*.975,na.rm=TRUE),max(rsi*1.05,na.rm=TRUE),length.out=length(x.range)),
         type='n',axes=FALSE,ann=FALSE)
    grid(NA,NULL,col=x@params$colors$grid.col)
    lines(seq(1,length(x.range),by=spacing),rsi,col='#0033CC',lwd=2,type='l')
    lines(seq(1,length(x.range),by=spacing),rsi,col='#BFCFFF',lwd=1,lty='dotted',type='l')
    #title(ylab=paste('RSI(',paste(c(n.up,collapse=','),')',sep=''))
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

  roc <- ROC(Cl(x),n=n,type=type,na=NA)
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
`addBBands` <- function(n=20,ma='SMA',sd=2,on=1) {

  stopifnot("package:TTR" %in% search() || require("TTR",quietly=TRUE))

  lchob <- get.current.chob()
  x <- as.matrix(eval(lchob@passed.args$x))
  chobTA <- new("chobTA")
  chobTA@new <- FALSE


  x <- as.matrix(eval(lchob@passed.args$x))

#  bb <- bollingerBands(cbind(Hi(x),Lo(x),Cl(x)),ma=list(ma,n=n),sd=sd)
  bb <- BBands(cbind(Hi(x),Lo(x),Cl(x)),n=n,maType=ma,sd=sd)
  
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
    lines(seq(1,length(x.range),by=spacing),bb[,1],col='red',lwd=1,lty='dashed')
    lines(seq(1,length(x.range),by=spacing),bb[,3],col='red',lwd=1,lty='dashed')
    lines(seq(1,length(x.range),by=spacing),bb[,2],col='grey',lwd=1,lty='dotted')
} # }}}

# addSAR {{{
`addSAR` <- function(accel=c(0.02,0.2),col='blue') {

  stopifnot("package:TTR" %in% search() || require("TTR",quietly=TRUE))

  lchob <- get.current.chob()
  x <- as.matrix(eval(lchob@passed.args$x))
  chobTA <- new("chobTA")
  chobTA@new <- FALSE


  x <- as.matrix(eval(lchob@passed.args$x))

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

#    macd <- 
#    function (x,fast=12,slow=26,signal=9,type='EMA') 
#    {
#        if(length(type) != 3) type <- rep(type[1],3) 
#        oscillator <- MACD(x, list(type[1], n = fast), list(type[2], 
#            n = slow), list(type[3], n = signal))
#        return(oscillator)
#    }

  col <- if(missing(col)) col <- c('#999999','#777777',
                              '#BBBBBB','#FF0000')

#  macd <- macd(Cl(x),fast=fast,slow=slow,signal=signal,type=type)
  macd <- MACD(Cl(x),nFast=fast,nSlow=slow,nSig=signal,maType=type)
  
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
    macd.min <- min(macd[,1],macd[,2],macd[,1]-macd[,2],na.rm=TRUE)
    macd.max <- max(macd[,1],macd[,2],macd[,1]-macd[,2],na.rm=TRUE)
    plot(x.range,seq(macd.min*.975,macd.max*1.05,length.out=length(x.range)),
         type='n',axes=FALSE,ann=FALSE)
    grid(NA,NULL,col=x@params$colors$grid.col)
    if(x@params$histo) {
      x.pos <- 1 + spacing * (1:NROW(macd) -1)
      cols <- ifelse((macd[,1]-macd[,2]) > 0, col[1],col[2])
      rect(x.pos - spacing/5,0,x.pos + spacing/5, macd[,1]-macd[,2],
           col=cols,border=cols)
    } 
    lines(seq(1,length(x.range),by=spacing),macd[,1],col=col[3],lwd=2)
    lines(seq(1,length(x.range),by=spacing),macd[,2],col=col[4],lwd=1,lty='dotted')
    #title(ylab=paste('RSI(',paste(c(n.up,collapse=','),')',sep=''))
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

    if(length(x@params$n) < length(x@params$col)) {
      colors <- 3:10
    } else colors <- x@params$col
 
    for(li in 1:length(x@params$n)) {
      ma <- EMA(x@TA.values,n=x@params$n[li],wilder=x@params$wilder,
                ratio=x@params$ratio)
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

# addSMA {{{
`addSMA` <- function(n=10,on=1,with.col=Cl,overlay=TRUE,col='brown') {

  stopifnot("package:TTR" %in% search() || require("TTR",quietly=TRUE))

  lchob <- get.current.chob()
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

    if(length(x@params$n) < length(x@params$col)) {
      colors <- 3:10
    } else colors <- x@params$col
 
    for(li in 1:length(x@params$n)) {
      ma <- SMA(x@TA.values,n=x@params$n[li])
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

# addWMA {{{
`addWMA` <- function(n=10,wts=1:n,on=1,with.col=Cl,overlay=TRUE,col='green') {

  stopifnot("package:TTR" %in% search() || require("TTR",quietly=TRUE))

  lchob <- get.current.chob()
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
    if(is.function(with.col)) {
      x.tmp <- cbind(do.call(with.col,list(x)),Vo(x))
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

`zzzRSI` <-
function (price, ma.up = list("EMA", n = 14, wilder = TRUE), 
    ma.down = ma.up) 
{
    up <- momentum(price, n = 1)
    dn <- ifelse(up < 0, abs(up), 0)
    up <- ifelse(up > 0, up, 0)
    mavg.up <- do.call(ma.up[[1]], c(list(up), ma.up[-1]))
    mavg.dn <- do.call(ma.down[[1]], c(list(dn), ma.down[-1]))
    rsi <- 100 * mavg.up/(mavg.up + mavg.dn)
    return(rsi)
}


`zzzEMA` <-
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

