
# addWPR {{{
`addWPR` <- function(n=14) {


  lenv <- new.env()
  lenv$chartWPR <- function(x, n) {
    xsubset <- x$Env$xsubset
    
    wpr <- wpr[xsubset]
    spacing <- x$Env$theme$spacing
    x.pos <- 1 + spacing * (1:NROW(wpr) - 1)
    xlim <- x$Env$xlim
    frame <- x$get_frame()
    ylim <- x$get_ylim()[[frame]]
    theme <- x$Env$theme
    y_grid_lines <- x$Env$y_grid_lines
    
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
    
    lines(x.pos,wpr,col=theme$WPR$col,lwd=1,type='l')
    
  }
  mapply(function(name, value) {
    assign(name, value, envir = lenv)
  }, names(list(n = n)), 
  list(n = n))
  exp <- parse(text = gsub("list", "chartWPR", as.expression(substitute(list(x = current.chob(), 
                                                                               n = n)))), srcfile = NULL)
  exp <- c(exp, expression(
    frame <- get_frame(),
    lc <- xts:::legend.coords("topleft", xlim, ylim[[frame]]),
    legend(x = lc$x, y = lc$y, 
           legend = c(paste("Williams %R (", n,"):", sep = ""),
                      paste(sprintf("%.3f",last(wpr[xsubset])), sep = "")), 
           text.col = c(theme$fg, theme$WPR$col), 
           xjust = lc$xjust, 
           yjust = lc$yjust, 
           bty = "n", 
           y.intersp=0.95)))
  
  lchob <- current.chob()
  ncalls <- length(lchob$Env$call_list)
  lchob$Env$call_list[[ncalls + 1]] <- match.call()
  if (is.null(lchob$Env$theme$WPR)) {
    lchob$Env$theme$WPR$col <- "#0033CC"
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


  wpr <- WPR(xx,n=n)
  lenv$xdata <- structure(wpr, .Dimnames=list(NULL, "wpr"))
  lenv$wpr <- lchob$Env$TA$wpr <- wpr
  lenv$get_frame <- lchob$get_frame
  lchob$add_frame(ylim=c(-0.1, max(abs(lenv$wpr[xsubset]), na.rm = TRUE)) * 1.05, asp=1, fixed=FALSE)
  lchob$next_frame()
  lchob$replot(exp, env=c(lenv, lchob$Env), expr=TRUE)
  lchob
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

    y.range <- seq(-0.1, max(abs(wpr), 
                   na.rm = TRUE), length.out = length(x.range)) * 1.05

    # create appropriately scaled empty plot area
    plot(x.range,y.range,type='n',axes=FALSE,ann=FALSE)

    coords <- par('usr')
    rect(coords[1],coords[3],coords[2],coords[4],col=x@params$colors$area)
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

