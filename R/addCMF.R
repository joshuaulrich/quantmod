
# addCMF {{{
`addCMF` <- function(n=20) {

  lenv <- new.env()
  lenv$chartCMF <- function(x, n) {
    xdata <- x$Env$xdata
    xsubset <- x$Env$xsubset
    xdata <- cbind(Hi(xdata),Lo(xdata),Cl(xdata))
    vo <- x$Env$vo
    cmf <- CMF(xdata,vo,n=n)[xsubset]
    spacing <- x$Env$theme$spacing
    x.pos <- 1 + spacing * (1:NROW(cmf) - 1)
    xlim <- x$Env$xlim
    ylim <- c(-max(abs(cmf), na.rm = TRUE), 
              max(abs(cmf), na.rm = TRUE))*1.05
    theme <- x$Env$theme

    cmf.positive <- ifelse(cmf >= 0,cmf,0)
    cmf.negative <- ifelse(cmf <  0,cmf,0)
    
    polygon(c(x.pos,rev(x.pos)),cbind(cmf.positive,rep(0,length(cmf))),col=theme$up.col)
    polygon(c(x.pos,rev(x.pos)),cbind(cmf.negative,rep(0,length(cmf))),col=theme$dn.col)
  }
  mapply(function(name, value) {
    assign(name, value, envir = lenv)
  }, names(list(n = n)), list(n = n))
  exp <- parse(text = gsub("list", "chartCMF", as.expression(substitute(list(x = current.chob(), 
                                                                                  n = n)))), srcfile = NULL)
  exp <- c(exp, expression(
    lc <- xts:::legend.coords("topleft", xlim, c(-max(abs(cmf), na.rm = TRUE),max(abs(cmf), na.rm = TRUE))*1.05),
    legend(x = lc$x, y = lc$y, 
           legend = c(paste(legend, ":"),
                      paste(sprintf("%.3f",last(cmf)), sep = "")),
           text.col = c(theme$fg, ifelse(last(cmf) > 0,theme$up.col,theme$dn.col)), 
           xjust = lc$xjust, 
           yjust = lc$yjust, 
           bty = "n", 
           y.intersp=0.95)))
  exp <- c(expression(    
    cmf <- TA$cmf,
    # add inbox color
    rect(xlim[1], -max(abs(cmf), na.rm = TRUE)*1.05, xlim[2], max(abs(cmf), na.rm = TRUE)*1.05, col=theme$fill),
    # add grid lines and left-side axis labels
    segments(xlim[1], y_grid_lines(c(-max(abs(cmf), na.rm = TRUE),max(abs(cmf), na.rm = TRUE))*1.05), 
             xlim[2], y_grid_lines(c(-max(abs(cmf), na.rm = TRUE),max(abs(cmf), na.rm = TRUE))*1.05), 
             col = theme$grid, lwd = x$Env$grid.ticks.lwd, lty = 3),
    text(xlim[1], y_grid_lines(c(-max(abs(cmf), na.rm = TRUE),max(abs(cmf), na.rm = TRUE))*1.05), y_grid_lines(c(-max(abs(cmf), na.rm = TRUE),max(abs(cmf), na.rm = TRUE))*1.05), 
         col = theme$labels, srt = theme$srt, 
         offset = 0.5, pos = 2, cex = theme$cex.axis, xpd = TRUE),
    # add border of plotting area
    rect(xlim[1], -max(abs(cmf), na.rm = TRUE)*1.05, xlim[2], max(abs(cmf), na.rm = TRUE)*1.05, border=theme$labels),
    segments(xlim[1], 0, xlim[2], 0, col = "#999999")), exp)
  
  lchob <- current.chob()
  ncalls <- length(lchob$Env$call_list)
  lchob$Env$call_list[[ncalls + 1]] <- match.call()
  xdata <- lchob$Env$xdata
  xdata <- if(is.OHLC(xdata)) {
    cbind(Hi(xdata),Lo(xdata),Cl(xdata))
  } else stop("CMF only applicaple to HLC series")
  xsubset <- lchob$Env$xsubset
  vo <- lchob$Env$vo

  cmf <- CMF(xdata,vo,n=n)[xsubset]
  lchob$Env$TA$cmf <- cmf
  if(!is.character(legend) || legend == "auto")
    lchob$Env$legend <- paste("Chaikin Money Flow (", n, ")", sep="")
  lchob$add_frame(ylim=c(-max(abs(cmf), na.rm = TRUE), 
                         max(abs(cmf), na.rm = TRUE))*1.05,asp=1,fixed=TRUE)
  lchob$next_frame()
  lchob$replot(exp, env=c(lenv,lchob$Env), expr=TRUE)
  lchob
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
    coords <- par('usr')
    rect(coords[1],coords[3],coords[2],coords[4],col=x@params$colors$area)
    grid(NA,NULL,col=x@params$colors$grid.col)

    xx <- seq(1,length(x.range),by=spacing)
    cmf.positive <- ifelse(cmf >= 0,cmf,0)
    cmf.negative <- ifelse(cmf <  0,cmf,0)

    polygon(c(xx,rev(xx)),c(cmf.positive,rep(0,length(cmf))),col=x@params$colors$up.col)
    polygon(c(xx,rev(xx)),c(cmf.negative,rep(0,length(cmf))),col=x@params$colors$dn.col)

    abline(h=0,col="#999999")

    text(0, last(y.range)*.9,
         paste("Chaikin Money Flow (", x@params$n,"):", sep = ""), 
        pos = 4)

    text(0, last(y.range)*.9,
        paste("\n\n\n",sprintf("%.3f",last(cmf)), sep = ""), 
        col = ifelse(last(cmf) > 0,x@params$colors$up.col,x@params$colors$dn.col), 
        pos = 4)

    axis(2)
    box(col=x@params$colors$fg.col)
} # }}}

