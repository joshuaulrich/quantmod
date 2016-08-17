# Trend Direction Index from TTR by Josh Ulrich
#
# chartSeries interface by Jeffrey A. Ryan 2008
#
#  addTDI

`addTDI` <-
function (n = 20, multiple = 2, ..., on = NA, legend = "auto") 
{
    lenv <- new.env()
    lenv$chartTDI <- function(x, n, multiple, ..., on, legend) {
      xsubset <- x$Env$xsubset
      tdi <- tdi[xsubset]
      spacing <- x$Env$theme$spacing
      x.pos <- 1 + spacing * (1:NROW(tdi) - 1)
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
      
      lines(x.pos, tdi[,1], col = theme$TDI$col$tdi, lwd = 1, lend = 2, ...)
      lines(x.pos, tdi[,2], col = theme$TDI$col$di, lwd = 1, lend = 2, ...)
      
    }
    if(!is.character(legend) || legend == "auto")
      legend <- gsub("^addTDI", "Trend Detection Index ", deparse(match.call()))
    mapply(function(name, value) {
      assign(name, value, envir = lenv)
    }, names(list(n = n, multiple = multiple, ..., on = on, legend = legend)), 
    list(n = n, multiple = multiple, ..., on = on, legend = legend))
    exp <- parse(text = gsub("list", "chartTDI", as.expression(substitute(list(x = current.chob(), 
                                                                               n = n, multiple = multiple, ..., on = on, legend = legend)))), srcfile = NULL)
    exp <- c(exp, expression(
      frame <- get_frame(),
      lc <- xts:::legend.coords("topleft", xlim, ylim[[frame]]),
      legend(x = lc$x, y = lc$y, 
             legend = c(paste(legend, ":"),
                        paste("tdi :",format(last(tdi[xsubset,1]),nsmall = 3L)), 
                        paste("di :",format(last(tdi[xsubset,1]),nsmall = 3L))),
             text.col = c(theme$fg, theme$TDI$col$tdi, theme$TDI$col$di), 
             xjust = lc$xjust, 
             yjust = lc$yjust, 
             bty = "n", 
             y.intersp=0.95)))
    
    lchob <- current.chob()
    ncalls <- length(lchob$Env$call_list)
    lchob$Env$call_list[[ncalls + 1]] <- match.call()
    if (is.null(lchob$Env$theme$TDI)) {
      lchob$Env$theme$TDI$col$tdi <- 5
      lchob$Env$theme$TDI$col$di <- 6
    }
    x <- lchob$Env$xdata
    xsubset <- lchob$Env$xsubset
    x <- Cl(x)
    tdi <- TDI(price = x, n = n, multiple = multiple)
    lenv$xdata <- structure(tdi, .Dimnames=list(NULL, c("tdi", "di")))
    lenv$tdi <- lchob$Env$TA$tdi <- tdi
    lenv$get_frame <- lchob$get_frame
    if (any(is.na(on))) {
        lchob$add_frame(ylim=range(lenv$tdi[xsubset], na.rm=TRUE)*1.05, asp=1, fixed=FALSE)
        lchob$next_frame()
    }
    else {
        lchob$set_frame(sign(on)*(abs(on)+1L))
    }
    lchob$replot(exp, env=c(lenv, lchob$Env), expr=TRUE)
    lchob
}
