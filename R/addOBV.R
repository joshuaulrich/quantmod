# On Balance Volume by Josh Ulrich from TTR
#
# chartSeries implementation by Jeffrey A. Ryan 2008
#
#  addOBV

`addOBV` <-
function (..., on = NA, legend = "auto") 
{
    lenv <- new.env()
    lenv$chartOBV <- function(x, ..., on, legend) {
      xdata <- try.xts(x$Env$xdata, error=FALSE)
      xsubset <- x$Env$xsubset
      vo <- x$Env$vo
      obv <- OBV(price = Cl(xdata), volume = vo)[xsubset]
      spacing <- x$Env$theme$spacing
      x.pos <- 1 + spacing * (1:NROW(obv) - 1)
      xlim <- x$Env$xlim
      ylim <- range(obv, na.rm=TRUE) * 1.05 
      theme <- x$Env$theme

      lines(x.pos, obv, col = theme$OBV$col, lwd = 1, lend = 2, ...)
      
    }
    if(!is.character(legend) || legend == "auto")
      legend <- gsub("^.*[(]", " On Balance Volume (", deparse(match.call()))#, 
    #extended = TRUE)
    mapply(function(name, value) {
      assign(name, value, envir = lenv)
    }, names(list(..., on = on, legend = legend)), 
    list(..., on = on, legend = legend))
    exp <- parse(text = gsub("list", "chartOBV", as.expression(substitute(list(x = current.chob(), 
                                                                                 ..., on = on, legend = legend)))), srcfile = NULL)
    exp <- c(exp, expression(
      lc <- xts:::legend.coords("topleft", xlim, range(obv, na.rm=TRUE) * 1.05),
      legend(x = lc$x, y = lc$y, 
             legend = c(paste(legend, ":"),
                        paste(format(last(obv[xsubset]),nsmall = 3L))),
             text.col = c(theme$fg, theme$OBV$col), 
             xjust = lc$xjust, 
             yjust = lc$yjust, 
             bty = "n", 
             y.intersp=0.95)))
    exp <- c(expression(
      obv <- TA$obv,
      # add inbox color
      rect(xlim[1], range(obv, na.rm=TRUE)[1] * 1.05, xlim[2], range(obv, na.rm=TRUE)[2] * 1.05, col=theme$fill),
      # add grid lines and left-side axis labels
      segments(xlim[1], y_grid_lines(range(obv, na.rm=TRUE) * 1.05), 
               xlim[2], y_grid_lines(range(obv, na.rm=TRUE) * 1.05), 
               col = theme$grid, lwd = x$Env$grid.ticks.lwd, lty = 3),
      text(xlim[1], y_grid_lines(range(obv, na.rm=TRUE) * 1.05), y_grid_lines(range(obv, na.rm=TRUE) * 1.05), 
           col = theme$labels, srt = theme$srt, 
           offset = 0.5, pos = 2, cex = theme$cex.axis, xpd = TRUE),
      # add border of plotting area
      rect(xlim[1], range(obv, na.rm=TRUE)[1] * 1.05, xlim[2], range(obv, na.rm=TRUE)[2] * 1.05, border=theme$labels)), exp)
    
    lchob <- current.chob()
    ncalls <- length(lchob$Env$call_list)
    lchob$Env$call_list[[ncalls + 1]] <- match.call()
    if (is.null(lchob$Env$theme$OBV)) {
      lchob$Env$theme$OBV$col <- 4
    }
    x <- try.xts(lchob$Env$xdata, error=FALSE)
    xsubset <- lchob$Env$xsubset
    vo <- lchob$Env$vo
    obv <- OBV(price = Cl(x), volume = vo)
    lchob$Env$TA$obv <- obv
    if(is.na(on)) {
      lchob$add_frame(ylim=range(obv, na.rm=TRUE) * 1.05 ,asp=1,fixed=TRUE)
      lchob$next_frame()
    }
    else {
      lchob$set_frame(sign(on)*(abs(on)+1L))
    }
    lchob$replot(exp, env=c(lenv,lchob$Env), expr=TRUE)
    lchob
}
