
# addSMI {{{
`addSMI` <- function(n=13,slow=25,fast=2,signal=9,ma.type='EMA') {


  lenv <- new.env()
  lenv$chartSMI <- function(x, n, slow, fast, signal, ma.type) {
    xdata <- x$Env$xdata
    xsubset <- x$Env$xsubset
    
    xx <- if(is.OHLC(xdata)) {
      cbind(Hi(xdata),Lo(xdata),Cl(xdata))
    } else if(is.null(dim(xdata))) {
      xdata
    } else {
      xdata[,1] 
    }
    
    smi <- SMI(xx, n=n, nFast=fast,
               nSlow=slow, nSig=signal, maType=ma.type)[xsubset]
    
    spacing <- x$Env$theme$spacing
    x.pos <- 1 + spacing * (1:NROW(smi) - 1)
    xlim <- x$Env$xlim
    ylim <- c(-max(abs(smi[,1]), na.rm = TRUE), 
                max(abs(smi[,1]), na.rm = TRUE))*1.05
    theme <- x$Env$theme
    
    lines(x.pos,smi[,1],col=theme$SMI$col$smi,lwd=1,type='l')
    lines(x.pos,smi[,2],col=theme$SMI$col$signal,lwd=1,lty='dotted',type='l')
    
  }
  mapply(function(name, value) {
    assign(name, value, envir = lenv)
  }, names(list(n = n,fast = fast,slow = slow,signal = signal,ma.type = ma.type)), 
  list(n = n,fast = fast,slow = slow,signal = signal,ma.type = ma.type))
  exp <- parse(text = gsub("list", "chartSMI", as.expression(substitute(list(x = current.chob(), 
                                                                              n = n,fast = fast,slow = slow,signal = signal,ma.type = ma.type)))), srcfile = NULL)
  exp <- c(exp, expression(
    text(0, max(abs(smi[,1]), na.rm = TRUE)*.9,
         paste("Stochastic Momentum Index (",
               paste(n,fast,slow,signal,sep=','),
               "):", sep = ""), col = theme$fg, 
         pos = 4),
    
    text(0, max(abs(smi[,1]), na.rm = TRUE)*.9,
         paste("\n\n\nSMI: ",sprintf("%.3f",last(smi[xsubset,1])), sep = ""), 
         col = theme$SMI$col$smi, pos = 4),
    
    text(0, max(abs(smi[,1]), na.rm = TRUE)*.9,
         paste("\n\n\n\n\nSignal: ",
               sprintf("%.3f",last(smi[xsubset,2])), sep = ""), 
         col = theme$SMI$col$signal, pos = 4)))
  exp <- c(expression(
    smi <- TA$smi,
    # add inbox color
    rect(xlim[1], -max(abs(smi[,1]), na.rm = TRUE)*1.05, xlim[2], max(abs(smi[,1]), na.rm = TRUE)*1.05, col=theme$fill),
    # add grid lines and left-side axis labels
    segments(xlim[1], y_grid_lines(c(-max(abs(smi[,1]), na.rm = TRUE),max(abs(smi[,1]), na.rm = TRUE))*1.05), 
             xlim[2], y_grid_lines(c(-max(abs(smi[,1]), na.rm = TRUE),max(abs(smi[,1]), na.rm = TRUE))*1.05), 
             col = theme$grid, lwd = x$Env$grid.ticks.lwd, lty = 3),
    text(xlim[1], y_grid_lines(c(-max(abs(smi[,1]), na.rm = TRUE),max(abs(smi[,1]), na.rm = TRUE))*1.05), y_grid_lines(c(-max(abs(smi[,1]), na.rm = TRUE),max(abs(smi[,1]), na.rm = TRUE))*1.05), 
         col = theme$labels, srt = theme$srt, 
         offset = 0.5, pos = 2, cex = theme$cex.axis, xpd = TRUE),
    # add border of plotting area
    rect(xlim[1], -max(abs(smi[,1]), na.rm = TRUE)*1.05, xlim[2], max(abs(smi[,1]), na.rm = TRUE)*1.05, border=theme$labels)), exp)
  
  lchob <- current.chob()
  ncalls <- length(lchob$Env$call_list)
  lchob$Env$call_list[[ncalls + 1]] <- match.call()
  if (is.null(lchob$Env$theme$SMI)) {
    lchob$Env$theme$SMI$col$smi <- "#0033CC"
      lchob$Env$theme$SMI$col$signal <- "#BFCFFF"
  }
  
  x <- lchob$Env$xdata
  xsubset <- lchob$Env$xsubset

  xx <- if(is.OHLC(x)) {
    cbind(Hi(x),Lo(x),Cl(x))
  } else if(is.null(dim(x))) {
    x
  } else {
    x[,1] 
  }

  smi <- SMI(xx, n=n, nFast=fast,
             nSlow=slow, nSig=signal, maType=ma.type)
  lchob$Env$TA$smi <- smi
  
  lchob$add_frame(ylim=c(-max(abs(smi[,1]), na.rm = TRUE), 
                         max(abs(smi[,1]), na.rm = TRUE))*1.05, asp=1, fixed=TRUE)
  lchob$next_frame()
  lchob$replot(exp, env=c(lenv, lchob$Env), expr=TRUE)
  lchob
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

    if(x@new) {
      plot(x.range,y.range,type='n',axes=FALSE,ann=FALSE)
  
      coords <- par('usr')
      rect(coords[1],coords[3],coords[2],coords[4],col=x@params$colors$area)
      grid(NA,NULL,col=x@params$colors$grid.col)
    } 
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

