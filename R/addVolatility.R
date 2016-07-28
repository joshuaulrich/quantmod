# volatility from TTR by Josh Ulrich
#
# chartSeries interface by Jeffrey A. Ryan 2008
#
#  addVolatility

`addVolatility` <-
function (n = 10, calc = "close", N = 260, ..., on = NA, legend = "auto") 
{
    lenv <- new.env()
    lenv$chartVolatility <- function(x, n, calc, N, ..., on, legend) {
      xdata <- x$Env$xdata
      xsubset <- x$Env$xsubset
      xdata <- OHLC(xdata)
      vol <- volatility(OHLC = xdata, n = n, calc = calc, N = N)[xsubset]
      spacing <- x$Env$theme$spacing
      x.pos <- 1 + spacing * (1:NROW(vol) - 1)
      xlim <- x$Env$xlim
      ylim <- c(min(vol, na.rm=TRUE) * 0.95, max(vol, na.rm=TRUE) * 1.05)
      theme <- x$Env$theme

      lines(x.pos, vol, col = 8, lwd = 1, lend = 2, ...)
      
    }
    if(!is.character(legend) || legend == "auto")
      legend <- gsub("^add", "", deparse(match.call()))
    mapply(function(name, value) {
      assign(name, value, envir = lenv)
    }, names(list(n = n, calc = calc, N = N, ..., on = on, legend = legend)), 
    list(n = n, calc = calc, N = N, ..., on = on, legend = legend))
    exp <- parse(text = gsub("list", "chartVolatility", as.expression(substitute(list(x = current.chob(), 
                                                                                      n = n, calc = calc, N = N, ..., on = on, legend = legend)))), srcfile = NULL)
    exp <- c(exp, expression(
      lc <- xts:::legend.coords("topleft", xlim, c(min(vol, na.rm=TRUE) * 0.95,max(vol, na.rm=TRUE) * 1.05)),
      legend(x = lc$x, y = lc$y, 
             legend = c(paste(legend, ":"),
                        format(last(vol),nsmall = 3L)),
             text.col = c(theme$fg, 8), 
             xjust = lc$xjust, 
             yjust = lc$yjust, 
             bty = "n", 
             y.intersp=0.95)))
    exp <- c(expression(
      # add inbox color
      rect(xlim[1], min(vol, na.rm=TRUE) * 0.95, xlim[2], max(vol, na.rm=TRUE) * 1.05, col=theme$fill),
      # add grid lines and left-side axis labels
      segments(xlim[1], y_grid_lines(c(min(vol, na.rm=TRUE) * 0.95,max(vol, na.rm=TRUE) * 1.05)), 
               xlim[2], y_grid_lines(c(min(vol, na.rm=TRUE) * 0.95,max(vol, na.rm=TRUE) * 1.05)), 
               col = theme$grid, lwd = x$Env$grid.ticks.lwd, lty = 3),
      text(xlim[1], y_grid_lines(c(min(vol, na.rm=TRUE) * 0.95,max(vol, na.rm=TRUE) * 1.05)), y_grid_lines(c(min(vol, na.rm=TRUE) * 0.95,max(vol, na.rm=TRUE) * 1.05)), 
           col = theme$labels, srt = theme$srt, 
           offset = 0.5, pos = 2, cex = theme$cex.axis, xpd = TRUE),
      # add border of plotting area
      rect(xlim[1], min(vol, na.rm=TRUE) * 0.95, xlim[2], max(vol, na.rm=TRUE) * 1.05, border=theme$labels)), exp)
    
    lchob <- current.chob()
    x <- lchob$Env$xdata
    xsubset <- lchob$Env$xsubset
    x <- OHLC(x)
    vol <- volatility(OHLC = x, n = n, calc = calc, N = N)[xsubset]
    lchob$Env$vol <- vol
    if (any(is.na(on))) {
        lchob$add_frame(ylim=c(min(vol, na.rm=TRUE) * 0.95, 
                               max(vol, na.rm=TRUE) * 1.05), asp=1, fixed=TRUE)
        lchob$next_frame()
    }
    else {
        lchob$set_frame(sign(on)*(abs(on)+1L))
    }
    lchob$replot(exp, env=c(lenv, lchob$Env), expr=TRUE)
    lchob
}
