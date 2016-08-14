
# addCMO {{{
`addCMO` <- function(n=14) {


  lenv <- new.env()
  lenv$chartCMO <- function(x, n) {
    xdata <- x$Env$xdata
    xsubset <- x$Env$xsubset
    xx <- if(has.Cl(xdata)) {
      Cl(xdata)
    } else if(NCOL(xdata)==1) {
      xdata
    } else {
      xdata[,1] 
    }
    cmo <- CMO(xx,n=n)[xsubset]
    spacing <- x$Env$theme$spacing
    x.pos <- 1 + spacing * (1:NROW(cmo) - 1)
    xlim <- x$Env$xlim
    ylim <- c(-max(abs(cmo), na.rm = TRUE), 
              max(abs(cmo), na.rm = TRUE))*1.05
    theme <- x$Env$theme
    
    lines(x.pos, cmo, col = "#0033CC", lwd = 1, lend = 2)
  }
  mapply(function(name, value) {
    assign(name, value, envir = lenv)
  }, names(list(n = n)), list(n = n))
  exp <- parse(text = gsub("list", "chartCMO", as.expression(substitute(list(x = current.chob(), 
                                                                             n = n)))), srcfile = NULL)
  exp <- c(exp, expression(
    lc <- xts:::legend.coords("topleft", xlim, c(-max(abs(cmo), na.rm = TRUE),max(abs(cmo), na.rm = TRUE))*1.05),
    legend(x = lc$x, y = lc$y, 
           legend = c(paste(legend, ":"),
                      paste(sprintf("%.3f",last(cmo[xsubset])), sep = "")),
           text.col = c(theme$fg, "#0033CC"), 
           xjust = lc$xjust, 
           yjust = lc$yjust, 
           bty = "n", 
           y.intersp=0.95)))
  exp <- c(expression(
    cmo <- TA$cmo,
    # add inbox color
    rect(xlim[1], -max(abs(cmo), na.rm = TRUE)*1.05, xlim[2], max(abs(cmo), na.rm = TRUE)*1.05, col=theme$fill),
    # add grid lines and left-side axis labels
    segments(xlim[1], y_grid_lines(c(-max(abs(cmo), na.rm = TRUE),max(abs(cmo), na.rm = TRUE))*1.05), 
             xlim[2], y_grid_lines(c(-max(abs(cmo), na.rm = TRUE),max(abs(cmo), na.rm = TRUE))*1.05), 
             col = theme$grid, lwd = x$Env$grid.ticks.lwd, lty = 3),
    text(xlim[1], y_grid_lines(c(-max(abs(cmo), na.rm = TRUE),max(abs(cmo), na.rm = TRUE))*1.05), y_grid_lines(c(-max(abs(cmo), na.rm = TRUE),max(abs(cmo), na.rm = TRUE))*1.05), 
         col = theme$labels, srt = theme$srt, 
         offset = 0.5, pos = 2, cex = theme$cex.axis, xpd = TRUE),
    # add border of plotting area
    rect(xlim[1], -max(abs(cmo), na.rm = TRUE)*1.05, xlim[2], max(abs(cmo), na.rm = TRUE)*1.05, border=theme$labels),
    segments(xlim[1], 0, xlim[2], 0, col = "#666666", lty = "dotted")), exp)
  
  lchob <- current.chob()
  ncalls <- length(lchob$Env$call_list)
  lchob$Env$call_list[[ncalls + 1]] <- match.call()

  x <- lchob$Env$xdata
  xsubset <- lchob$Env$xsubset

  #  needs to accept any arguments for x, not just close

  xx <- if(has.Cl(x)) {
    Cl(x)
  } else if(NCOL(x)==1) {
    x
  } else {
    x[,1] 
  }

  cmo <- CMO(xx,n=n)
  lchob$Env$TA$cmo <- cmo
  if(!is.character(legend) || legend == "auto")
    lchob$Env$legend <- paste("Chande Momentum Oscillator (", n, ") ", sep="")
  lchob$add_frame(ylim=c(-max(abs(cmo), na.rm = TRUE), 
                         max(abs(cmo), na.rm = TRUE))*1.05,asp=1,fixed=TRUE)
  lchob$next_frame()
  lchob$replot(exp, env=c(lenv,lchob$Env), expr=TRUE)
  lchob
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
    coords <- par('usr')
    rect(coords[1],coords[3],coords[2],coords[4],col=x@params$colors$area)
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

