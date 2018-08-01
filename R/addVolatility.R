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
      xsubset <- x$Env$xsubset
      vol <- vol[xsubset]
      spacing <- x$Env$theme$spacing
      x.pos <- 1 + spacing * (1:NROW(vol) - 1)
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
      
      lines(x.pos, vol, col = theme$Volatility$col, lwd = 1, lend = 2, ...)
      
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
      frame <- get_frame(),
      lc <- xts:::legend.coords("topleft", xlim, ylim[[frame]]),
      legend(x = lc$x, y = lc$y, 
             legend = c(paste(legend, ":"),
                        sprintf("%.3f",last(vol[xsubset]))),
             text.col = c(theme$fg, theme$Volatility$col), 
             xjust = lc$xjust, 
             yjust = lc$yjust, 
             bty = "n", 
             y.intersp=0.95)))
    
    lchob <- current.chob()
    ncalls <- length(lchob$Env$call_list)
    lchob$Env$call_list[[ncalls + 1]] <- match.call()
    if (is.null(lchob$Env$theme$Vol)) {
      lchob$Env$theme$Volatility$col <- 8
    }
    x <- lchob$Env$xdata
    xsubset <- lchob$Env$xsubset
    x <- OHLC(x)
    vol <- volatility(OHLC = x, n = n, calc = calc, N = N)
    lenv$xdata <- structure(vol, .Dimnames=list(NULL, "vol"))
    lenv$vol <- lchob$Env$TA$vol <- vol
    lenv$get_frame <- lchob$get_frame
    if (any(is.na(on))) {
        lchob$add_frame(ylim=c(min(lenv$vol[xsubset], na.rm=TRUE) * 0.95, 
                               max(lenv$vol[xsubset], na.rm=TRUE) * 1.05), asp=1, fixed=FALSE)
        lchob$next_frame()
    }
    else {
        lchob$set_frame(sign(on)*(abs(on)+1L))
    }
    lchob$replot(exp, env=c(lenv, lchob$Env), expr=TRUE)
    lchob
}
