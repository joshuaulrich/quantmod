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
`addMomentum` <- function(n=1, with.col=Cl) {


  lenv <- new.env()
  lenv$chartMomentum <- function(x, n, with.col) {
    xdata <- lchob$Env$xdata
    xsubset <- lchob$Env$xsubset
    if(is.OHLC(xdata) && missing(with.col)) with.col <- 1
    
    if(is.function(with.col)) {
      xx <- do.call(with.col,list(xdata))
    } else xx <- xdata[,with.col]
    
    mom <- momentum(xx,n=n)[xsubset]
    spacing <- x$Env$theme$spacing
    x.pos <- 1 + spacing * (1:NROW(mom) - 1)
    xlim <- x$Env$xlim
    ylim <- c(-max(abs(mom),na.rm=TRUE),
              max(abs(mom),na.rm=TRUE)) * 1.05
    theme <- x$Env$theme
    # add inbox color
    rect(xlim[1], ylim[1], xlim[2], ylim[2], col=theme$fill)
    # add grid lines and left-side axis labels
    segments(xlim[1], y_grid_lines(ylim), 
             xlim[2], y_grid_lines(ylim), 
             col = theme$grid, lwd = x$Env$grid.ticks.lwd, lty = 3)
    text(xlim[1], y_grid_lines(ylim), y_grid_lines(ylim), 
         col = theme$labels, srt = theme$srt, 
         offset = 0.5, pos = 2, cex = theme$cex.axis, xpd = TRUE)
    # add border of plotting area
    rect(xlim[1], ylim[1], xlim[2], ylim[2], border=theme$labels)
    
    COLOR <- "#0033CC"
    
    segments(xlim[1],0,xlim[2],0,col="#666666",lwd=1,lty='dotted')
    
    lines(x.pos,mom,col=COLOR,lwd=2,type='l')
    
    text(0, ylim[2]*.9, 
         paste("Momentum (", n, "):"),col=theme$fg, pos=4)
    
    text(0, ylim[2]*.9,
         paste("\n\n\n",sprintf("%.2f",last(mom)),sep=''),
         col = COLOR, pos = 4)
  }
  mapply(function(name, value) {
    assign(name, value, envir = lenv)
  }, names(list(n = n, with.col = with.col)), list(n = n, with.col = with.col))
  exp <- parse(text = gsub("list", "chartMomentum", as.expression(substitute(list(x = current.chob(), 
                                                                                  n = n, with.col = with.col)))), srcfile = NULL)
  lchob <- current.chob()

  x <- lchob$Env$xdata
  xsubset <- lchob$Env$xsubset

  if(is.OHLC(x) && missing(with.col)) with.col <- 1
  
  if(is.function(with.col)) {
    xx <- do.call(with.col,list(x))
  } else xx <- x[,with.col]

  mom <- momentum(xx,n=n)[xsubset]
  
  lchob$add_frame(ylim=c(-max(abs(mom),na.rm=TRUE),
                         max(abs(mom),na.rm=TRUE)) * 1.05, asp=1, fixed=TRUE)
  lchob$next_frame()
  lchob$replot(exp, env=c(lenv, lchob$Env), expr=TRUE)
  lchob
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


  lenv <- new.env()
  lenv$chartCCI <- function(x, n, maType, c) {
    xdata <- x$Env$xdata
    xsubset <- x$Env$xsubset
    xx <- if(is.OHLC(xdata)) {
      cbind(Hi(xdata),Lo(xdata),Cl(xdata))
    } else xdata 
    cci <- CCI(xx,n=n,maType=maType,c=c)[xsubset]
    spacing <- x$Env$theme$spacing
    x.pos <- 1 + spacing * (1:NROW(cci) - 1)
    xlim <- x$Env$xlim
    ylim <- c(-max(abs(cci),na.rm=TRUE),
              max(abs(cci),na.rm=TRUE))*1.05
    theme <- x$Env$theme
    # add inbox color
    rect(xlim[1], ylim[1], xlim[2], ylim[2], col=theme$fill)
    # add grid lines and left-side axis labels
    segments(xlim[1], y_grid_lines(ylim), 
             xlim[2], y_grid_lines(ylim), 
             col = theme$grid, lwd = x$Env$grid.ticks.lwd, lty = 3)
    text(xlim[1], y_grid_lines(ylim), y_grid_lines(ylim), 
         col = theme$labels, srt = theme$srt, 
         offset = 0.5, pos = 2, cex = theme$cex.axis, xpd = TRUE)
    # add border of plotting area
    rect(xlim[1], ylim[1], xlim[2], ylim[2], border=theme$labels)
    
    # draw shading in -100:100 y-range 
    rect(xlim[1],-100,xlim[2],100,col=theme$bbands$col$fill,border=theme$fg)
    
    # fill upper and lower areas
    cci.above <- ifelse(cci >=  100,cci, 100)
    cci.below <- ifelse(cci <= -100,cci,-100)
    
    polygon(c(x.pos,rev(x.pos)),cbind(cci.above,rep(100,length(cci))),col="red",border=theme$fg)
    polygon(c(x.pos,rev(x.pos)),cbind(cci.below,rep(-100,length(cci))),col="red",border=theme$fg)
    
    # draw CCI
    lines(x.pos,cci,col='red',lwd=1,type='l')
    
    # draw dotted guide line at 0
    segments(xlim[1],0,xlim[2],0,col='#666666',lwd=1,lty='dotted')
    
    # add indicator name and last value
    text(0, ylim[2]*.9,
         paste("Commodity Channel Index (", n, ",",
               c,"):",sep=''),col=theme$fg,pos=4)
    text(0, ylim[2]*.9,
         paste("\n\n\n",sprintf("%.2f",last(cci)),sep=''), col = 'red', 
         pos = 4)
  }
  mapply(function(name, value) {
    assign(name, value, envir = lenv)
  }, names(list(n = n, maType = maType, c = c)), list(n = n, maType = maType, c = c))
  exp <- parse(text = gsub("list", "chartCCI", as.expression(substitute(list(x = current.chob(), 
                                                                             n = n, maType = maType, c = c)))), srcfile = NULL)
  lchob <- current.chob()

  x <- lchob$Env$xdata
  xsubset <- lchob$Env$xsubset

  xx <- if(is.OHLC(x)) {
    cbind(Hi(x),Lo(x),Cl(x))
  } else x 

  cci <- CCI(xx,n=n,maType=maType,c=c)[xsubset]
  lchob$Env$cci <- cci
  lchob$add_frame(ylim=c(-max(abs(cci), na.rm = TRUE), 
                         max(abs(cci), na.rm = TRUE))*1.05,asp=1,fixed=TRUE)
  lchob$next_frame()
  lchob$replot(exp, env=c(lenv,lchob$Env), expr=TRUE)
  lchob
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

  lenv <- new.env()
  lenv$chartADX <- function(x, n, maType, wilder) {
    xdata <- x$Env$xdata
    xsubset <- x$Env$xsubset
    adx <- ADX(cbind(Hi(xdata), Lo(xdata), Cl(xdata)), n=n, maType=maType, wilder=wilder)[xsubset]
    spacing <- x$Env$theme$spacing
    x.pos <- 1 + spacing * (1:NROW(adx) - 1)
    xlim <- x$Env$xlim
    ylim <- c(min(adx*0.975, na.rm = TRUE), 
              max(adx*1.05, na.rm = TRUE))
    theme <- x$Env$theme
    # add inbox color
    rect(xlim[1], ylim[1], xlim[2], ylim[2], col=theme$fill)
    # add grid lines and left-side axis labels
    segments(xlim[1], y_grid_lines(ylim), 
             xlim[2], y_grid_lines(ylim), 
             col = theme$grid, lwd = x$Env$grid.ticks.lwd, lty = 3)
    text(xlim[1], y_grid_lines(ylim), y_grid_lines(ylim), 
         col = theme$labels, srt = theme$srt, 
         offset = 0.5, pos = 2, cex = theme$cex.axis, xpd = TRUE)
    # add border of plotting area
    rect(xlim[1], ylim[1], xlim[2], ylim[2], border=theme$labels)
    segments(xlim[1], 20, xlim[2], 20, col = "#666666", lty = "dotted")
    segments(xlim[1], 40, xlim[2], 40, col = "#666666", lty = "dotted")
    
    # draw DIp
    lines(x.pos,adx[,1],col='green',lwd=1,type='l')
    # draw DIn
    lines(x.pos,adx[,2],col='red',lwd=1,type='l')
    # draw ADX
    lines(x.pos,adx[,4],col='blue',lwd=2,type='l')
  }
  mapply(function(name, value) {
    assign(name, value, envir = lenv)
  }, names(list(n = n, maType = maType, wilder = wilder)), 
  list(n = n, maType = maType, wilder = wilder))
  exp <- parse(text = gsub("list", "chartADX", as.expression(substitute(list(x = current.chob(), 
                                                                               n = n, maType = maType, wilder = wilder)))), srcfile = NULL)
  
  lchob <- current.chob()

  x <- lchob$Env$xdata
  xsubset <- lchob$Env$xsubset

  if(!is.OHLC(x)) stop("only applicable to HLC series")

  adx <- ADX(cbind(Hi(x),Lo(x),Cl(x)),n=n,maType=maType,wilder=wilder)[xsubset]
  lchob$Env$adx <- adx
  lchob$add_frame(ylim=c(min(adx*0.975, na.rm = TRUE), 
                         max(adx*1.05, na.rm = TRUE)),asp=1,fixed=TRUE)
  lchob$next_frame()
  lchob$replot(exp, env=c(lenv,lchob$Env), expr=TRUE)
  lchob
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

  lenv <- new.env()
  lenv$chartATR <- function(x, n, maType) {
    xdata <- x$Env$xdata
    xsubset <- x$Env$xsubset
    atr <- ATR(cbind(Hi(xdata), Lo(xdata), Cl(xdata)), n=n, maType=maType)[xsubset]
    spacing <- x$Env$theme$spacing
    x.pos <- 1 + spacing * (1:NROW(atr) - 1)
    xlim <- x$Env$xlim
    ylim <- c(min(atr[,2]*0.975, na.rm = TRUE), 
              max(atr[,2]*1.05, na.rm = TRUE))
    theme <- x$Env$theme
    # add inbox color
    rect(xlim[1], ylim[1], xlim[2], ylim[2], col=theme$fill)
    # add grid lines and left-side axis labels
    segments(xlim[1], y_grid_lines(ylim), 
             xlim[2], y_grid_lines(ylim), 
             col = theme$grid, lwd = x$Env$grid.ticks.lwd, lty = 3)
    text(xlim[1], y_grid_lines(ylim), y_grid_lines(ylim), 
         col = theme$labels, srt = theme$srt, 
         offset = 0.5, pos = 2, cex = theme$cex.axis, xpd = TRUE)
    # add border of plotting area
    rect(xlim[1], ylim[1], xlim[2], ylim[2], border=theme$labels)
    
    lines(x.pos,atr[,2],col='blue',lwd=2,type='l')
  }
  mapply(function(name, value) {
    assign(name, value, envir = lenv)
  }, names(list(n = n, maType = maType)), list(n = n, maType = maType))
  exp <- parse(text = gsub("list", "chartATR", as.expression(substitute(list(x = current.chob(), 
                                                                             n = n, maType = maType)))), srcfile = NULL)
  
  lchob <- current.chob()

  x <- lchob$Env$xdata
  xsubset <- lchob$Env$xsubset

  if(!is.OHLC(x)) stop("only applicable to HLC series")

  atr <- ATR(cbind(Hi(x),Lo(x),Cl(x)),n=n,maType=maType,...)[xsubset]
  lchob$Env$atr <- atr
  lchob$add_frame(ylim=c(min(atr[,2]*0.975, na.rm = TRUE), 
                         max(atr[,2]*1.05, na.rm = TRUE)),asp=1,fixed=TRUE)
  lchob$next_frame()
  lchob$replot(exp, env=c(lenv,lchob$Env), expr=TRUE)
  lchob
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


  lenv <- new.env()
  lenv$chartTRIX <- function(x, n, signal, maType, percent) {
    xdata <- x$Env$xdata
    xsubset <- x$Env$xsubset
    
    xx <- if(is.OHLC(xdata)) {
      Cl(xdata)
    } else xdata
    
    trix <- TRIX(xx,n=n,nSig=signal,maType=maType,percent=percent)[xsubset]
    spacing <- x$Env$theme$spacing
    x.pos <- 1 + spacing * (1:NROW(trix) - 1)
    xlim <- x$Env$xlim
    ylim <- c(min(trix[,1]*.975,na.rm=TRUE),
              max(trix[,1]*1.05,na.rm=TRUE))
    theme <- x$Env$theme
    # add inbox color
    rect(xlim[1], ylim[1], xlim[2], ylim[2], col=theme$fill)
    # add grid lines and left-side axis labels
    segments(xlim[1], y_grid_lines(ylim), 
             xlim[2], y_grid_lines(ylim), 
             col = theme$grid, lwd = x$Env$grid.ticks.lwd, lty = 3)
    text(xlim[1], y_grid_lines(ylim), y_grid_lines(ylim), 
         col = theme$labels, srt = theme$srt, 
         offset = 0.5, pos = 2, cex = theme$cex.axis, xpd = TRUE)
    # add border of plotting area
    rect(xlim[1], ylim[1], xlim[2], ylim[2], border=theme$labels)
    
    # draw TRIX
    lines(x.pos,trix[,1],col='green',lwd=1,type='l')
    # draw Signal
    lines(x.pos,trix[,2],col='#999999',lwd=1,type='l')
  }
  mapply(function(name, value) {
    assign(name, value, envir = lenv)
  }, names(list(n = n, signal = signal, maType = maType, percent = TRUE)), 
  list(n = n, signal = signal, maType = maType, percent = TRUE))
  exp <- parse(text = gsub("list", "chartTRIX", as.expression(substitute(list(x = current.chob(), 
                                                                              n = n, signal = signal, maType = maType, percent = TRUE)))), srcfile = NULL)
  lchob <- current.chob()

  x <- lchob$Env$xdata
  xsubset <- lchob$Env$xsubset

  xx <- if(is.OHLC(x)) {
    Cl(x)
  } else x 

  trix <- TRIX(xx,n=n,nSig=signal,maType=maType,percent=percent)[xsubset]
  lchob$add_frame(ylim=c(min(trix[,1]*.975,na.rm=TRUE),
                         max(trix[,1]*1.05,na.rm=TRUE)), asp=1, fixed=TRUE)
  lchob$next_frame()
  lchob$replot(exp, env=c(lenv, lchob$Env), expr=TRUE)
  lchob
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


  lenv <- new.env()
  lenv$chartDPO <- function(x, n, maType, shift, percent) {
    xdata <- x$Env$xdata
    xsubset <- x$Env$xsubset
    xx <- if(is.OHLC(xdata)) {
      Cl(xdata)
    } else xdata
    dpo <- DPO(xx,n=n,maType=maType,shift=shift,percent=percent)[xsubset]
    spacing <- x$Env$theme$spacing
    x.pos <- 1 + spacing * (1:NROW(dpo) - 1)
    xlim <- x$Env$xlim
    ylim <- c(-max(abs(dpo), na.rm = TRUE), 
              max(abs(dpo), na.rm = TRUE))*1.05
    theme <- x$Env$theme
    # add inbox color
    rect(xlim[1], ylim[1], xlim[2], ylim[2], col=theme$fill)
    # add grid lines and left-side axis labels
    segments(xlim[1], y_grid_lines(ylim), 
             xlim[2], y_grid_lines(ylim), 
             col = theme$grid, lwd = x$Env$grid.ticks.lwd, lty = 3)
    text(xlim[1], y_grid_lines(ylim), y_grid_lines(ylim), 
         col = theme$labels, srt = theme$srt, 
         offset = 0.5, pos = 2, cex = theme$cex.axis, xpd = TRUE)
    # add border of plotting area
    rect(xlim[1], ylim[1], xlim[2], ylim[2], border=theme$labels)
    segments(xlim[1], 0, xlim[2], 0, col = "#999999")
    
    dpo.tmp <- dpo
    dpo.tmp[is.na(dpo)] <- 0
    dpo.positive <- ifelse(dpo.tmp >= 0,dpo.tmp,0)
    dpo.negative <- ifelse(dpo.tmp <  0,dpo.tmp,0)
    
    polygon(c(x.pos,rev(x.pos)),cbind(dpo.positive,rep(0,length(dpo))),col=theme$up.col, border="#999999")
    polygon(c(x.pos,rev(x.pos)),cbind(dpo.negative,rep(0,length(dpo))),col=theme$dn.col, border="#999999")
    
    text(0, ylim[2]*.9,
         paste("De-trended Price Oscillator (", n,"):", sep = ""), 
         col = theme$fg, pos = 4)
    
    text(0, ylim[2]*.9,
         paste("\n\n\n",sprintf("%.3f",last(na.omit(dpo))), sep = ""), 
         col = ifelse(last(na.omit(dpo)) > 0,theme$up.col,theme$dn.col), 
         pos = 4)
  }
  mapply(function(name, value) {
    assign(name, value, envir = lenv)
  }, names(list(n = n, maType = maType, shift = shift, percent = percent)), 
  list(n = n, maType = maType, shift = shift, percent = percent))
  exp <- parse(text = gsub("list", "chartDPO", as.expression(substitute(list(x = current.chob(), 
                                                                             n = n, maType = maType, shift = shift, percent = percent)))), srcfile = NULL)
  lchob <- current.chob()

  x <- lchob$Env$xdata
  xsubset <- lchob$Env$xsubset

  # should really allow for _any_ series to be used, like MA (FIXME)

  xx <- if(is.OHLC(x)) {
    Cl(x)
  } else x 

  dpo <- DPO(xx,n=n,maType=maType,shift=shift,percent=percent)[xsubset]
  lchob$Env$dpo <- dpo
  lchob$add_frame(ylim=c(-max(abs(dpo), na.rm = TRUE), 
                         max(abs(dpo), na.rm = TRUE)) * 1.05,asp=1,fixed=TRUE)
  lchob$next_frame()
  lchob$replot(exp, env=c(lenv,lchob$Env), expr=TRUE)
  lchob
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


  lenv <- new.env()
  lenv$chartRSI <- function(x, n, maType, wilder) {
    xdata <- x$Env$xdata
    xsubset <- x$Env$xsubset
    xx <- if(is.OHLC(xdata)) {
      Cl(xdata)
    } else xdata
    rsi <- RSI(xx,n=n,maType=maType,wilder=wilder)[xsubset]
    spacing <- x$Env$theme$spacing
    x.pos <- 1 + spacing * (1:NROW(rsi) - 1)
    xlim <- x$Env$xlim
    ylim <- c(min(rsi,na.rm=TRUE)*.975,max(rsi,na.rm=TRUE)*1.05)
    theme <- x$Env$theme
    # add inbox color
    rect(xlim[1], ylim[1], xlim[2], ylim[2], col=theme$fill)
    # add grid lines and left-side axis labels
    segments(xlim[1], y_grid_lines(ylim), 
             xlim[2], y_grid_lines(ylim), 
             col = theme$grid, lwd = x$Env$grid.ticks.lwd, lty = 3)
    text(xlim[1], y_grid_lines(ylim), y_grid_lines(ylim), 
         col = theme$labels, srt = theme$srt, 
         offset = 0.5, pos = 2, cex = theme$cex.axis, xpd = TRUE)
    # add border of plotting area
    rect(xlim[1], ylim[1], xlim[2], ylim[2], border=theme$labels)
    
    lines(x.pos,rsi,col='#0033CC',lwd=2,type='l')
    lines(x.pos,rsi,col='#BFCFFF',lwd=1,lty='dotted',type='l')
    
    text(0, ylim[2]*.9,
         paste("Relative Strength Index (", n,"):", sep = ""), col = theme$fg,  
         pos = 4)
    
    text(0, ylim[2]*.9,
         paste("\n\n\n",sprintf("%.3f",last(rsi)), sep = ""), col = '#0033CC', 
         pos = 4)
  }
  mapply(function(name, value) {
    assign(name, value, envir = lenv)
  }, names(list(n = n, maType = maType, wilder = wilder)), 
  list(n = n, maType = maType, wilder = wilder))
  exp <- parse(text = gsub("list", "chartRSI", as.expression(substitute(list(x = current.chob(), 
                                                                             n = n, maType = maType, wilder = wilder)))), srcfile = NULL)
  lchob <- current.chob()

  x <- lchob$Env$xdata
  xsubset <- lchob$Env$xsubset

  xx <- if(is.OHLC(x)) {
    Cl(x)
  } else x 

  rsi <- RSI(xx,n=n,maType=maType,wilder=wilder)[xsubset]
  lchob$Env$rsi <- rsi
  lchob$add_frame(ylim=c(min(rsi,na.rm=TRUE)*.975,max(rsi,na.rm=TRUE)*1.05),asp=1,fixed=TRUE)
  lchob$next_frame()
  lchob$replot(exp, env=c(lenv,lchob$Env), expr=TRUE)
  lchob
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


  lenv <- new.env()
  lenv$chartROC <- function(x, n, type, col) {
    xdata <- x$Env$xdata
    xsubset <- x$Env$xsubset
    
    xx <- if(is.OHLC(xdata)) {
      Cl(xdata)
    } else xdata 
    
    roc <- ROC(xx,n=n,type=type[1],na.pad=TRUE)[xsubset]
    spacing <- x$Env$theme$spacing
    x.pos <- 1 + spacing * (1:NROW(roc) - 1)
    xlim <- x$Env$xlim
    ylim <- c(-max(abs(roc), na.rm = TRUE), 
              max(abs(roc), na.rm = TRUE))*1.05
    theme <- x$Env$theme
    # add inbox color
    rect(xlim[1], ylim[1], xlim[2], ylim[2], col=theme$fill)
    # add grid lines and left-side axis labels
    segments(xlim[1], y_grid_lines(ylim), 
             xlim[2], y_grid_lines(ylim), 
             col = theme$grid, lwd = x$Env$grid.ticks.lwd, lty = 3)
    text(xlim[1], y_grid_lines(ylim), y_grid_lines(ylim), 
         col = theme$labels, srt = theme$srt, 
         offset = 0.5, pos = 2, cex = theme$cex.axis, xpd = TRUE)
    # add border of plotting area
    rect(xlim[1], ylim[1], xlim[2], ylim[2], border=theme$labels)
    
    lines(x.pos,roc,col=col,lwd=2,type='l')
  }
  mapply(function(name, value) {
    assign(name, value, envir = lenv)
  }, names(list(n = n, type = type, col = col)), list(n = n, type = type, col = col))
  exp <- parse(text = gsub("list", "chartROC", as.expression(substitute(list(x = current.chob(), 
                                                                             n = n, type = type, col = col)))), srcfile = NULL)
  lchob <- current.chob()

  x <- lchob$Env$xdata
  xsubset <- lchob$Env$xsubset

  xx <- if(is.OHLC(x)) {
    Cl(x)
  } else x 

  roc <- ROC(xx,n=n,type=type[1],na.pad=TRUE)[xsubset]
  lchob$Env$roc <- roc
  lchob$add_frame(ylim=c(-max(abs(roc), na.rm = TRUE), 
                         max(abs(roc), na.rm = TRUE))*1.05, asp=1, fixed=TRUE)
  lchob$next_frame()
  lchob$replot(exp, env=c(lenv, lchob$Env), expr=TRUE)
  lchob
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


  draw.options <- c("bands", "percent", "width")
  draw <- draw.options[pmatch(draw, draw.options)]
  lenv <- new.env()
  lenv$chartBBands <- function(x, n, sd, maType, draw, on) {
    xdata <- x$Env$xdata
    xsubset <- x$Env$xsubset
    
    xx <- if(is.OHLC(xdata)) {
      cbind(Hi(xdata),Lo(xdata),Cl(xdata))
    } else xdata 
    
    bb <- BBands(xx,n=n,maType=maType,sd=sd)[xsubset]
    spacing <- x$Env$theme$spacing
    x.pos <- 1 + spacing * (1:NROW(bb) - 1)
    xlim <- x$Env$xlim
    theme <- x$Env$theme
    bband.col <- ifelse(!is.null(theme$bbands$col),
                        theme$bbands$col$upper,'red') 
    bband.fill <- ifelse(!is.null(theme$bbands$col$fill),
                         theme$bbands$col$fill,theme$bg)
    
    # bband col vector
    # lower.band, middle.band, upper.band, %b, bb.width
    if(length(bband.col) == 1) # no user specified
      bband.col <- c(bband.col,'grey',rep(bband.col,3))
    
    if(draw == 'bands') {
      # draw Bollinger Bands on price chart
      if(on[1] > 0) {
        lines(x.pos,
              bb[,1],col=bband.col[1],lwd=1,lty='dashed')
        lines(x.pos,
              bb[,3],col=bband.col[3],lwd=1,lty='dashed')
        lines(x.pos,
              bb[,2],col=bband.col[2],lwd=1,lty='dotted')
      } else {
        
        polygon(c(x.pos,rev(x.pos)),
                c(as.numeric(bb[,1]),as.numeric(rev(bb[,3]))),col=bband.fill,border=NA)
        lines(x.pos,
              bb[,1],col=bband.col[1],lwd=1,lty='dashed')
        lines(x.pos,
              bb[,3],col=bband.col[3],lwd=1,lty='dashed')
        lines(x.pos,
              bb[,2],col=bband.col[2],lwd=1,lty='dotted')
      }
      
      lc <- xts:::legend.coords("topleft", xlim, lchob$get_ylim()[[2]])
      legend(lc$x,lc$y,
             legend=paste("Bollinger Bands (",
                          paste(n,sd,sep=","),") [Upper/Lower]: ",
                          sprintf("%.3f",last(bb[,3])),"/",
                          sprintf("%.3f",last(bb[,1])), sep = ""), 
             text.col = bband.col[3],
             xjust = lc$xjust, 
             yjust = 1.5, 
             bty = "n", 
             y.intersp=0.95) 
      
    } else 
      if(draw == 'percent') {
        
        rect(xlim[1], ylim[1], xlim[2], ylim[2], col=theme$fill)
        # add grid lines and left-side axis labels
        segments(xlim[1], y_grid_lines(ylim), 
                 xlim[2], y_grid_lines(ylim), 
                 col = theme$grid, lwd = x$Env$grid.ticks.lwd, lty = 3)
        text(xlim[1], y_grid_lines(ylim), y_grid_lines(ylim), 
             col = theme$labels, srt = theme$srt, 
             offset = 0.5, pos = 2, cex = theme$cex.axis, xpd = TRUE)
        # add border of plotting area
        rect(xlim[1], ylim[1], xlim[2], ylim[2], border=theme$labels)
        
        # draw %B in new frame
        y.range <- seq(min(bb[,4], na.rm = TRUE) * .9,
                       max(abs(bb[,4]), na.rm = TRUE) * 1.05,
                       length.out = length(x.pos))
        
        lines(x.pos, bb[,4], col=bband.col[4],lwd=1)
        
        text(0,last(y.range) * .9, paste("Bollinger %b (",
                                         paste(n,sd,sep=","), "): ",
                                         sep=""), pos=4, col=theme$fg)
        text(0,last(y.range) * .9, paste("\n\n\n",
                                         sprintf("%.3f",last(bb[,4])), sep = ""),
             pos=4, col=bband.col[4])
        
      } else {
        
        rect(xlim[1], ylim[1], xlim[2], ylim[2], col=theme$fill)
        # add grid lines and left-side axis labels
        segments(xlim[1], y_grid_lines(ylim), 
                 xlim[2], y_grid_lines(ylim), 
                 col = theme$grid, lwd = x$Env$grid.ticks.lwd, lty = 3)
        text(xlim[1], y_grid_lines(ylim), y_grid_lines(ylim), 
             col = theme$labels, srt = theme$srt, 
             offset = 0.5, pos = 2, cex = theme$cex.axis, xpd = TRUE)
        # add border of plotting area
        rect(xlim[1], ylim[1], xlim[2], ylim[2], border=theme$labels)
        
        # draw width in new frame
        # (high band - low band) / middle band
        bbw <- (bb[,3] - bb[,1]) / bb[,2]
        
        y.range <- seq(min(bbw, na.rm = TRUE) * .9,
                       max(abs(bbw), na.rm = TRUE) * 1.05,
                       length.out = length(x.pos))
        
        lines(x.pos, bbw, col=bband.col[5],lwd=1)
        
        text(0,last(y.range) * .9, paste("Bollinger Band Width (",
                                         paste(n,sd,sep=","), "): ",
                                         sep=""), pos=4, col=theme$fg)
        text(0,last(y.range) * .9, paste("\n\n\n",
                                         sprintf("%.3f",last(bbw)), sep = ""),
             pos=4, col=bband.col[5])
      }
  }
  mapply(function(name, value) {
    assign(name, value, envir = lenv)
  }, names(list(n = n, sd = sd, maType = maType, draw = draw, on = on)), 
  list(n = n, sd = sd, maType = maType, draw = draw, on = on))
  exp <- parse(text = gsub("list", "chartBBands", as.expression(substitute(list(x = current.chob(), 
                                                                                n = n, sd = sd, maType = maType, draw = draw, on = on)))), srcfile = NULL)
#  draw.options <- c('bands','percent','width')
#  draw <- draw.options[pmatch(draw,draw.options)]

  lchob <- current.chob()

  x <- lchob$Env$xdata
  xsubset <- lchob$Env$xsubset

  xx <- if(is.OHLC(x)) {
    cbind(Hi(x),Lo(x),Cl(x))
  } else x 

  bb <- BBands(xx,n=n,maType=maType,sd=sd)[xsubset]
  lchob$Env$bb <- bb
  if(draw == 'bands') {
    # draw Bollinger Bands on price chart
    lchob$set_frame(-2)
    
  } else 
    if(draw == 'percent') {
      # draw %B in new frame
      ylim <- c(min(bb[,4], na.rm = TRUE) * .9,
                max(abs(bb[,4]), na.rm = TRUE) * 1.05)
      
      lchob$add_frame(ylim=c(ylim[1], ylim[2]),asp=1,fixed=TRUE)
      lchob$next_frame()
      
    } else {
      # draw width in new frame
      # (high band - low band) / middle band
      bbw <- (bb[,3] - bb[,1]) / bb[,2]
      
      ylim <- c(min(bbw, na.rm = TRUE) * .9,
                     max(abs(bbw), na.rm = TRUE) * 1.05)
      
      lchob$add_frame(ylim=c(ylim[1], ylim[2]),asp=1,fixed=TRUE)
      lchob$next_frame()
    }
  lchob$replot(exp, env=c(lenv,lchob$Env), expr=TRUE)
  lchob
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


  lenv <- new.env()
  lenv$chartEnvelope <- function(x, n, p, maType, ..., on) {
    xdata <- x$Env$xdata
    xsubset <- x$Env$xsubset
    
    xx <- if(is.OHLC(xdata)) {
      Cl(xdata)
    } else xdata 
    
    ma <- do.call(maType,list(xx,n=n,...))
    mae <- cbind(ma*(1-p/100),ma,ma*(1+p/100))
    
    spacing <- x$Env$theme$spacing
    x.pos <- 1 + spacing * (1:NROW(mae) - 1)
    xlim <- x$Env$xlim
    theme <- x$Env$theme
    if(on[1] > 0) {
      lines(x.pos,mae[,1],col='blue',lwd=1,lty='dotted')
      lines(x.pos,mae[,3],col='blue',lwd=1,lty='dotted')
      #lines(x.pos,mae[,2],col='grey',lwd=1,lty='dotted')
    } else {
      xx <- x.pos
      polygon(c(xx,rev(xx)), c(as.numeric(mae[,1]),rev(as.numeric(mae[,3]))),col='#282828',border=NA)
      lines(x.pos,mae[,1],col='blue',lwd=1,lty='dotted')
      lines(x.pos,mae[,3],col='blue',lwd=1,lty='dotted')
      #lines(x.pos,mae[,2],col='grey',lwd=1,lty='dotted')
    }
    
    lc <- xts:::legend.coords("topleft", xlim, lchob$get_ylim()[[2]])
    legend(lc$x,lc$y,
           legend=paste("Moving Ave. Envelope (",
                        paste(n,p,sep=","),") [Upper/Lower]: ",
                        sprintf("%.3f",last(mae[,3])),"/",
                        sprintf("%.3f",last(mae[,1])), sep = ""), 
           text.col = "blue",
           xjust = lc$xjust, 
           yjust = 1.5, 
           bty = "n", 
           y.intersp=0.95) 
  }
  mapply(function(name, value) {
    assign(name, value, envir = lenv)
  }, names(list(n = n, p = p, maType = maType, ..., on = on)), 
  list(n = n, p = p, maType = maType, ..., on = on))
  exp <- parse(text = gsub("list", "chartEnvelope", as.expression(substitute(list(x = current.chob(), 
                                                                                  n = n, p = p, maType = maType, ..., on = on)))), srcfile = NULL)
  
  lchob <- current.chob()

  x <- lchob$Env$xdata
  xsubset <- lchob$Env$xsubset

  xx <- if(is.OHLC(x)) {
    Cl(x)
  } else x 

  ma <- do.call(maType,list(xx,n=n,...))
  mae <- cbind(ma*(1-p/100),ma,ma*(1+p/100))[xsubset]
  lchob$Env$mae <- mae
  lchob$set_frame(on+1)
  lchob$replot(exp, env=c(lenv, lchob$Env), expr=TRUE)
  lchob
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


  lenv <- new.env()
  lenv$chartSAR <- function(x, accel, col) {
    xdata <- x$Env$xdata
    xsubset <- x$Env$xsubset
    sar <- SAR(cbind(Hi(xdata),Lo(xdata)),accel=accel)[xsubset]
    spacing <- x$Env$theme$spacing
    x.pos <- 1 + spacing * (1:NROW(sar) - 1)

    points(x.pos,sar,col=col,cex=1)
  }
  mapply(function(name, value) {
    assign(name, value, envir = lenv)
  }, names(list(accel = accel, col = col)), list(accel = accel, col = col))
  exp <- parse(text = gsub("list", "chartSAR", as.expression(substitute(list(x = current.chob(), 
                                                                             accel = accel, col = col)))), srcfile = NULL)
  lchob <- current.chob()

  x <- lchob$Env$xdata
  xsubset <- lchob$Env$xsubset

  if(!is.OHLC(x)) stop("SAR requires HL series") 

  sar <- SAR(cbind(Hi(x),Lo(x)),accel=accel)[xsubset]
  lchob$Env$sar <- sar
  lchob$set_frame(2)
  lchob$replot(exp, env=c(lenv, lchob$Env), expr=TRUE)
  lchob
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


  
  lenv <- new.env()
  lenv$chartMACD <- function(x, fast, slow, signal, type, histogram, col) {
    xdata <- x$Env$xdata
    xsubset <- x$Env$xsubset
    
    xx <- if(is.OHLC(xdata)) {
      Cl(xdata)
    } else xdata 
    
    macd <- MACD(xx,nFast=fast,nSlow=slow,nSig=signal,maType=type)[xsubset]
    spacing <- x$Env$theme$spacing
    x.pos <- 1 + spacing * (1:NROW(macd) - 1)
    xlim <- x$Env$xlim
    ylim <- c(-max(abs(macd),na.rm=TRUE),
              max(abs(macd),na.rm=TRUE))*1.05
    theme <- x$Env$theme
    # add inbox color
    rect(xlim[1], ylim[1], xlim[2], ylim[2], col=theme$fill)
    # add grid lines and left-side axis labels
    segments(xlim[1], y_grid_lines(ylim), 
             xlim[2], y_grid_lines(ylim), 
             col = theme$grid, lwd = x$Env$grid.ticks.lwd, lty = 3)
    text(xlim[1], y_grid_lines(ylim), y_grid_lines(ylim), 
         col = theme$labels, srt = theme$srt, 
         offset = 0.5, pos = 2, cex = theme$cex.axis, xpd = TRUE)
    # add border of plotting area
    rect(xlim[1], ylim[1], xlim[2], ylim[2], border=theme$labels)
    if(histogram) {
      cols <- ifelse((macd[,1]-macd[,2]) > 0, col[1],col[2])
      rect(x.pos - spacing/5,0,x.pos + spacing/5, macd[,1]-macd[,2],
           col=cols,border=cols)
    } 
    
    lines(x.pos,macd[,1],col=col[3],lwd=1)
    lines(x.pos,macd[,2],col=col[4],lwd=1,lty='dotted')
    
    lc <- xts:::legend.coords("topleft", xlim, ylim)
    legend(lc$x, lc$y, 
           legend=c(paste("Moving Average Convergence Divergence (",
                          paste(fast,slow,signal,sep=','),"):", sep = ""),
                    paste("MACD:",sprintf("%.3f",last(macd[,1]))),
                    paste("Signal:",sprintf("%.3f",last(macd[,2])))),
           text.col=c(theme$fg, col[3], col[4]),
           xjust=lc$xjust,
           yjust=lc$yjust,
           bty='n',
           y.intersp=0.95) 
  }
  col <- if(missing(col)) col <- c('#999999','#777777',
                                   '#BBBBBB','#FF0000')
  mapply(function(name, value) {
    assign(name, value, envir = lenv)
  }, names(list(fast = fast,slow = slow,signal = signal,type = type,histogram = histogram,col = col)), 
  list(fast = fast,slow = slow,signal = signal,type = type,histogram = histogram,col = col))
  exp <- parse(text = gsub("list", "chartMACD", as.expression(substitute(list(x = current.chob(), 
                                                                             fast = fast,slow = slow,signal = signal,type = type,histogram = histogram,col = col)))), srcfile = NULL)
  lchob <- current.chob()

  x <- lchob$Env$xdata
  xsubset <- lchob$Env$xsubset

  xx <- if(is.OHLC(x)) {
    Cl(x)
  } else x 

  macd <- MACD(xx,nFast=fast,nSlow=slow,nSig=signal,maType=type)[xsubset]
  lchob$Env$macd <- macd
  lchob$add_frame(ylim=c(-max(abs(macd),na.rm=TRUE),
                         max(abs(macd),na.rm=TRUE))*1.05, asp=1, fixed=TRUE)
  lchob$next_frame()
  lchob$replot(exp, env=c(lenv, lchob$Env), expr=TRUE)
  lchob
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

  lenv <- new.env()
  lenv$chartShading <- function(x, when, on, overlay, col) {
    xdata <- x$Env$xdata
    xsubset <- x$Env$xsubset
    xdata <- xdata[xsubset]
    xlim <- x$Env$xlim
    ylim <- x$get_ylim()[[abs(on)+1L]]
    theme <- x$Env$theme
    spacing <- theme$spacing
    width <- theme$width
    i <- when
    indexClass(xdata) <- "POSIXct"
    POSIXindex <- index(xdata)
    if (missing(i)) 
      i <- 1:NROW(xdata)
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
    
    if(!overlay) {
      # add inbox color
      rect(xlim[1], ylim[1], xlim[2], ylim[2], col=theme$fill)
      # add grid lines and left-side axis labels
      segments(xlim[1], y_grid_lines(ylim), 
               xlim[2], y_grid_lines(ylim), 
               col = theme$grid, lwd = x$Env$grid.ticks.lwd, lty = 3)
      text(xlim[1], y_grid_lines(ylim), y_grid_lines(ylim), 
           col = theme$labels, srt = theme$srt, 
           offset = 0.5, pos = 2, cex = theme$cex.axis, xpd = TRUE)
      # add border of plotting area
      rect(xlim[1], ylim[1], xlim[2], ylim[2], border=theme$labels)
    }
    
    rect(((xstart-1)*spacing+1)-width/2, rep(ylim[1],length(xstart)),
         ((xend-1)*spacing+1)+width/2, rep(ylim[2],length(xend)),
         col=c(theme$bbands$col$fill),border=NA)
  }
  mapply(function(name, value) {
    assign(name, value, envir = lenv)
  }, names(list(when = when, on = on, overlay = overlay, col = col)), 
  list(when = when, on = on, overlay = overlay, col = col))
  exp <- parse(text = gsub("list", "chartShading", as.expression(substitute(list(x = current.chob(), 
                                                                                 when = when, on = on, overlay = overlay, col = col)))), srcfile = NULL)
  lchob <- current.chob()

    if(overlay) {
      lchob$set_frame(sign(on)*(abs(on)+1L))
    } else {
      lchob$add_frame(ylim=c(lchob$get_ylim()[[abs(on)+1L]][1], 
                             lchob$get_ylim()[[abs(on)+1L]][2]), asp=1, fixed=TRUE)
      lchob$next_frame()
    }
    lchob$replot(exp, env=c(lenv, lchob$Env), expr=TRUE)
    lchob
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
 
  lenv <- new.env()
  lenv$chartPoints <- function(x, type, pch, offset, col, bg, cex, on, overlay) {
    xdata <- x$Env$xdata
    x.points <- which(x$Env$xsubset %in% x$Env$x)
    y.points <- x$Env$y
    spacing <- x$Env$theme$spacing
    
    # if OHLC and above - get Hi, else Lo
    # if univariate - get value
    y.data <- if(is.OHLC(xdata)) {
      if(offset > 1) {
        Hi(xdata)
      } else Lo(xdata)
    } else xdata
    
    if(is.null(y.points)) y.points <- y.data[x.points] * offset
    
    if(!overlay) {
      x.pos <- 1 + spacing * (1:NROW(x.points) - 1)
      xlim <- x$Env$xlim
      ylim <- x$get_ylim()[[2]]
      theme <- x$Env$theme
      # add inbox color
      rect(xlim[1], ylim[1], xlim[2], ylim[2], col=theme$fill)
      # add grid lines and left-side axis labels
      segments(xlim[1], y_grid_lines(ylim), 
               xlim[2], y_grid_lines(ylim), 
               col = theme$grid, lwd = x$Env$grid.ticks.lwd, lty = 3)
      text(xlim[1], y_grid_lines(ylim), y_grid_lines(ylim), 
           col = theme$labels, srt = theme$srt, 
           offset = 0.5, pos = 2, cex = theme$cex.axis, xpd = TRUE)
      # add border of plotting area
      rect(xlim[1], ylim[1], xlim[2], ylim[2], border=theme$labels)
      segments(xlim[1], 0, xlim[2], 0, col = "#666666", lty = "dotted")
    }
    
    points(x=x.pos, y=y.points, type=type,pch=pch,col=col,bg=bg,cex=cex)
  }
  mapply(function(name,value) { assign(name,value,envir=lenv) }, 
         names(list(type = type, pch = pch, offset = offset, col = col, 
                    bg = bg, cex = cex, on = on, overlay = overlay)), 
         list(type = type, pch = pch, offset = offset, col = col, 
              bg = bg, cex = cex, on = on, overlay = overlay))
  exp <- parse(text=gsub("list","chartPoints",as.expression(substitute(list(x=current.chob(),
                                                                            type = type, pch = pch, offset = offset, col = col, 
                                                                            bg = bg, cex = cex, on = on, overlay = overlay)))),
               srcfile=NULL)
  lchob <- current.chob()
  xdata <- lchob$Env$xdata
  xsubset <- lchob$Env$xsubset
  xdata <- xdata[xsubset]


  
  if(missing(bg)) bg <- col

    xsubset <- x %in% xsubset
    if(NROW(x) != NROW(y)) stop('x and y must be of equal lengths')
    x <- x[xsubset]
    if(!is.null(y))
      y <- y[xsubset]
    
    lchob$Env$x <- x
    lchob$Env$y <- y


    if(overlay)
      lchob$set_frame(on+1)
    else {
      lchob$add_frame(ylim=lchob$get_ylim()[[2]], asp=1, fixed=TRUE)
      lchob$next_frame()
    }
    lchob$replot(exp, env=c(lenv, lchob$Env), expr=TRUE)
    lchob
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


  lenv <- new.env()
  lenv$chartSMA <- function(x, n, on, with.col, overlay, col) {
    # get the appropriate data - from the approp. src
    if(on==1) {
      xdata <- x$Env$xdata
      
      if(!is.OHLC(xdata) && missing(with.col)) with.col <- 1
      
      if(is.function(with.col)) {
        x.tmp <- do.call(with.col,list(xdata))
      } else x.tmp <- xdata[,with.col]
    } else {
      # get values from TA...
      name.TA <- sub("\\(.*", "", sub(".*chart", "", paste(deparse(x$get_actions(on+1)[[1]]), collapse = "")))
      which.TA <- which(tolower(names(x$Env)) == tolower(name.TA))
      target.TA <- names(x$Env)[which.TA]
      xdata <- get(target.TA, envir = x$Env)
      
      if(missing(with.col)) with.col <- 1
      
#      if(is.function(with.col)) {
#        x.tmp <- do.call(with.col,list(x))
#      } else x.tmp <- x[,with.col]
      x.tmp <- xdata
    }
    xsubset <- x$Env$xsubset
    x.tmp <- x.tmp[xsubset]
    spacing <- x$Env$theme$spacing
    x.pos <- 1 + spacing * (1:NROW(x.tmp) - 1)
    xlim <- x$Env$xlim
    ylim <- x$get_ylim()[[abs(on)+1L]]
    if(length(n) != length(col)) {
      colors <- c(4:10,3)
    } else colors <- col
    
    if(!overlay) {
      # add inbox color
      rect(xlim[1], ylim[1], xlim[2], ylim[2], col=theme$fill)
      # add grid lines and left-side axis labels
      segments(xlim[1], y_grid_lines(ylim), 
               xlim[2], y_grid_lines(ylim), 
               col = theme$grid, lwd = x$Env$grid.ticks.lwd, lty = 3)
      text(xlim[1], y_grid_lines(ylim), y_grid_lines(ylim), 
           col = theme$labels, srt = theme$srt, 
           offset = 0.5, pos = 2, cex = theme$cex.axis, xpd = TRUE)
      # add border of plotting area
      rect(xlim[1], ylim[1], xlim[2], ylim[2], border=theme$labels)
    }
    
    ma.tmp <- NULL
    for(i in 1:length(n)) {
      ma <- SMA(x.tmp,n=n[i])
      ma.tmp <- cbind(ma.tmp,ma)
      
      lines(x.pos,ma,col=colors[i],lwd=1,type='l')
    }
  }
  mapply(function(name, value) {
    assign(name, value, envir = lenv)
  }, names(list(n = n, on = on, with.col = with.col, overlay = overlay, col = col)), 
  list(n = n, on = on, with.col = with.col, overlay = overlay, col = col))
  exp <- parse(text = gsub("list", "chartSMA", as.expression(substitute(list(x = current.chob(), 
                                                                              n = n, on = on, with.col = with.col, overlay = overlay, col = col)))), srcfile = NULL)
  lchob <- current.chob()
  if(overlay) {
    lchob$set_frame(sign(on)*(abs(on)+1L))
  } else {
    lchob$add_frame(ylim=lchob$get_ylim()[[abs(on)+1L]], asp=1, fixed=TRUE)
    lchob$next_frame()
  }
  lchob$replot(exp, env=c(lenv, lchob$Env), expr=TRUE)
  lchob
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


  lenv <- new.env()
  lenv$chartWMA <- function(x, n, wts, on, with.col, overlay, col) {
    # get the appropriate data - from the approp. src
    if(on==1) {
      xdata <- lchob$Env$xdata
      
      if(!is.OHLC(xdata) && missing(with.col)) with.col <- 1
      
      if(is.function(with.col)) {
        x.tmp <- do.call(with.col,list(xdata))
      } else x.tmp <- xdata[,with.col]
    } else {
      # get values from TA...
      name.TA <- sub("\\(.*", "", sub(".*chart", "", paste(deparse(x$get_actions(on+1)[[1]]), collapse = "")))
      which.TA <- which(tolower(names(x$Env)) == tolower(name.TA))
      target.TA <- names(x$Env)[which.TA]
      xdata <- get(target.TA, envir = x$Env)
      
      if(missing(with.col)) with.col <- 1
      
      #    if(is.function(with.col)) {
      #      x.tmp <- do.call(with.col,list(x))
      #    } else x.tmp <- x[,with.col]
      #  }
      x.tmp <- xdata
    }
    xsubset <- x$Env$xsubset
    x.tmp <- x.tmp[xsubset]
    spacing <- x$Env$theme$spacing
    x.pos <- 1 + spacing * (1:NROW(x.tmp) - 1)
    xlim <- x$Env$xlim
    theme <- x$Env$theme
    if(length(n) < length(col)) {
      colors <- 3:10
    } else colors <- col
    
    for(li in 1:length(n)) {
      ma <- WMA(x.tmp,n=n[li],wts=wts)[xsubset]
      if(!overlay) {
        ylim <- c(min(ma*0.975, na.rm=TRUE), max(ma*1.05, na.rm=TRUE))
        # add inbox color
        rect(xlim[1], ylim[1], xlim[2], ylim[2], col=theme$fill)
        # add grid lines and left-side axis labels
        segments(xlim[1], y_grid_lines(ylim), 
                 xlim[2], y_grid_lines(ylim), 
                 col = theme$grid, lwd = x$Env$grid.ticks.lwd, lty = 3)
        text(xlim[1], y_grid_lines(ylim), y_grid_lines(ylim), 
             col = theme$labels, srt = theme$srt, 
             offset = 0.5, pos = 2, cex = theme$cex.axis, xpd = TRUE)
        # add border of plotting area
        rect(xlim[1], ylim[1], xlim[2], ylim[2], border=theme$labels)
      }
      lines(x.pos,ma,col=colors[li],lwd=1,type='l')
    }
  }
  mapply(function(name, value) {
    assign(name, value, envir = lenv)
  }, names(list(n = n, wts = wts, on = on, with.col = with.col, overlay = overlay, col = col)), 
  list(n = n, wts = wts, on = on, with.col = with.col, overlay = overlay, col = col))
  exp <- parse(text = gsub("list", "chartWMA", as.expression(substitute(list(x = current.chob(), 
                                                                             n = n, wts = wts, on = on, with.col = with.col, overlay = overlay, col = col)))), srcfile = NULL)
  lchob <- current.chob()
  x <- lchob$Env$xdata
  xsubset <- lchob$Env$xsubset
  if(!is.OHLC(x) && missing(with.col)) with.col <- 1
  
  if(is.function(with.col)) {
    x.tmp <- do.call(with.col,list(x))
  } else x.tmp <- x[,with.col]
  
  if(overlay)
    lchob$set_frame(on+1)
  else {
    for(li in 1:length(n)) {
      ma <- WMA(x.tmp,n=n[li],wts=wts)[xsubset]
      lchob$add_frame(ylim=c(min(ma*0.975, na.rm=TRUE), 
                             max(ma*1.05, na.rm=TRUE)), asp=1, fixed=TRUE)
      lchob$next_frame()
    }
  }
  lchob$replot(exp, env=c(lenv, lchob$Env), expr=TRUE)
  lchob
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
        plot(x.range,seq(min(ma*.975, na.rm=TRUE),max(ma*1.05, na.rm=TRUE),length.out=length(x.range)),
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


  lenv <- new.env()
  lenv$chartDEMA <- function(x, n, on, with.col, overlay, col) {
    # get the appropriate data - from the approp. src
    if(on==1) {
      xdata <- x$Env$xdata
      
      if(!is.OHLC(xdata) && missing(with.col)) with.col <- 1
      
      if(is.function(with.col)) {
        x.tmp <- do.call(with.col,list(xdata))
      } else x.tmp <- xdata[,with.col]
    } else {
      # get values from TA...
      name.TA <- sub("\\(.*", "", sub(".*chart", "", paste(deparse(x$get_actions(on+1)[[1]]), collapse = "")))
      which.TA <- which(tolower(names(x$Env)) == tolower(name.TA))
      target.TA <- names(x$Env)[which.TA]
      xdata <- get(target.TA, envir = x$Env)

      if(missing(with.col)) with.col <- 1
      
#      if(is.function(with.col)) {
#        x.tmp <- do.call(with.col,list(xdata))
#      } else x.tmp <- xdata[,with.col]
      x.tmp <- xdata
    }
    xsubset <- x$Env$xsubset
    x.tmp <- x.tmp[xsubset]
    spacing <- x$Env$theme$spacing
    x.pos <- 1 + spacing * (1:NROW(x.tmp) - 1)
    xlim <- x$Env$xlim
    if(length(n) < length(col)) {
      colors <- 3:10
    } else colors <- col
    
    for(li in 1:length(n)) {
      ma <- DEMA(x.tmp,n=n[li])
#      if(x@new) {
#        par(new=TRUE)
#        plot(x.range,seq(min(ma*.975),max(ma*1.05),length.out=length(x.range)),
#             type='n',axes=FALSE,ann=FALSE)
#        title(ylab=paste('EMA(',paste(x@params$n[li],collapse=','),')',sep=''))
#        axis(2)
#        box(col=x@params$colors$fg.col)
#      }
      lines(x.pos,ma,col=colors[li],lwd=1,type='l')
    }
  }
  mapply(function(name, value) {
    assign(name, value, envir = lenv)
  }, names(list(n = n, on = on, with.col = with.col, overlay = overlay, col = col)), 
  list(n = n, on = on, with.col = with.col, overlay = overlay, col = col))
  exp <- parse(text = gsub("list", "chartDEMA", as.expression(substitute(list(x = current.chob(), 
                                                                              n = n, on = on, with.col = with.col, overlay = overlay, col = col)))), srcfile = NULL)
  lchob <- current.chob()
  lchob$set_frame(on+1)
  lchob$replot(exp, env=c(lenv, lchob$Env), expr=TRUE)
  lchob
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


  lenv <- new.env()
  lenv$chartEVWMA <- function(x, n, on, with.col, overlay, col) {
    
    # get the appropriate data - from the approp. src
    if(on==1) {
      xdata <- x$Env$xdata
      vo <- x$Env$vo
      
      if(!is.OHLC(xdata) && missing(with.col)) with.col <- 1
      
      if(is.function(with.col)) {
        x.tmp <- cbind(do.call(with.col,list(xdata)),vo)
      } else x.tmp <- xdata[,with.col]
    } else {
      # get values from TA...
      name.TA <- sub("\\(.*", "", sub(".*chart", "", paste(deparse(x$get_actions(on+1)[[1]]), collapse = "")))
      which.TA <- which(tolower(names(x$Env)) == tolower(name.TA))
      target.TA <- names(x$Env)[which.TA]
      xdata <- get(target.TA, envir = x$Env)
      
      if(missing(with.col)) with.col <- 1
      
#      if(is.function(with.col)) {
#        x.tmp <- do.call(with.col,list(xdata))
#      } else x.tmp <- xdata[,with.col]
      x.tmp <- xdata
    }
    
    if(!has.Vo(x.tmp)) return()
    
    xsubset <- x$Env$xsubset
    x.tmp <- x.tmp[xsubset]
    spacing <- x$Env$theme$spacing
    x.pos <- 1 + spacing * (1:NROW(x.tmp) - 1)
    xlim <- x$Env$xlim
    if(length(n) < length(col)) {
      colors <- 3:10
    } else colors <- col
    
    for(li in 1:length(n)) {
      ma <- EVWMA(x.tmp[, 1],x.tmp[, 2],n=n[li])
      #      if(x@new) {
      #        par(new=TRUE)
      #        plot(x.range,seq(min(ma*.975),max(ma*1.05),length.out=length(x.range)),
      #             type='n',axes=FALSE,ann=FALSE)
      #        title(ylab=paste('EMA(',paste(x@params$n[li],collapse=','),')',sep=''))
      #        axis(2)
      #        box(col=x@params$colors$fg.col)
      #      }
      lines(x.pos,ma,col=colors[li],lwd=1,type='l')
    }
  }
  mapply(function(name, value) {
    assign(name, value, envir = lenv)
  }, names(list(n = n, on = on, with.col = with.col, overlay = overlay, col = col)), 
  list(n = n, on = on, with.col = with.col, overlay = overlay, col = col))
  exp <- parse(text = gsub("list", "chartEVWMA", as.expression(substitute(list(x = current.chob(), 
                                                                              n = n, on = on, with.col = with.col, overlay = overlay, col = col)))), srcfile = NULL)
  lchob <- current.chob()
  lchob$set_frame(on+1)
  lchob$replot(exp, env=c(lenv, lchob$Env), expr=TRUE)
  lchob
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
  lenv <- new.env()
  lenv$chartExpiry <- function(x, type, lty) {
    
    xdata <- x$Env$xdata
    xsubset <- x$Env$xsubset
    xdata <- xdata[xsubset]
    spacing <- x$Env$theme$spacing
    theme <- x$Env$theme
    
    if(type=='options') {
      index.of.exp <- options.expiry(xdata)
    } else index.of.exp <- futures.expiry(xdata)
    
    for(ex in 1:length(index.of.exp)) {
      abline(v=index.of.exp[ex]*spacing, lty=lty,col=theme$Expiry)
    }
  }
  mapply(function(name, value) {
    assign(name, value, envir = lenv)
  }, names(list(type=type,lty=lty)), list(type=type,lty=lty))
  exp <- parse(text = gsub("list", "chartExpiry", as.expression(substitute(list(x = current.chob(), 
                                                                                type=type,lty=lty)))), srcfile = NULL)
  lchob <- current.chob()
  
  lchob$set_frame(-2)
  lchob$replot(exp, env=c(lenv, lchob$Env), expr=TRUE)
  lchob
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
