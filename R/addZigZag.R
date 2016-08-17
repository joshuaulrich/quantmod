# ZigZag from TTR by Josh Ulrich
#
# chartSeries interface by Jeffrey A. Ryan 2008
#
#   addZigZag

`addZigZag` <-
function (change = 10, percent = TRUE, retrace = FALSE, lastExtreme = TRUE, 
    ..., on = -1, legend = "auto") 
{
    lenv <- new.env()
    lenv$chartZigZag <- function(x, change, percent, retrace, lastExtreme, ..., on, legend) {
      xsubset <- x$Env$xsubset
      zigzag <- zigzag[xsubset]
      spacing <- x$Env$theme$spacing
      x.pos <- 1 + spacing * (1:NROW(zigzag) - 1)
      xlim <- x$Env$xlim
      ylim <- c(min(zigzag, na.rm=TRUE)*0.975, max(zigzag, na.rm=TRUE)*1.05)
      theme <- x$Env$theme
      y_grid_lines <- x$Env$y_grid_lines
      
      if(any(is.na(on))) {
        legend.name <- c(paste(legend, ":"),
                         paste(format(last(na.omit(zigzag)),nsmall = 3L)))
        text.col <- c(x$Env$theme$fg, 4)
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
        yjust <- 1
      } else {
        ylim <- x$get_ylim()[[2]]
        legend.name <- paste(legend, ":", format(last(na.omit(zigzag)),nsmall = 3L))
      }
      lines(x.pos, zigzag, col = theme$ZigZag$col, lwd = 4, lend = 2, ...)
      lc <- xts:::legend.coords("topleft", xlim, ylim)
      legend(x = lc$x, y = lc$y, 
             legend = legend.name,
             text.col = theme$ZigZag$col, 
             xjust = lc$xjust, 
             yjust = 2, 
             bty = "n", 
             y.intersp=0.95)
    }
    if(!is.character(legend) || legend == "auto")
      legend <- gsub("^add", "", deparse(match.call()))
    mapply(function(name, value) {
      assign(name, value, envir = lenv)
    }, names(list(change = change, percent = percent, retrace = retrace, lastExtreme = lastExtreme, ..., on = on, legend = legend)), 
    list(change = change, percent = percent, retrace = retrace, lastExtreme = lastExtreme, ..., on = on, legend = legend))
    exp <- parse(text = gsub("list", "chartZigZag", as.expression(substitute(list(x = current.chob(), 
                                                                                  change = change, percent = percent, retrace = retrace, lastExtreme = lastExtreme, ..., on = on, legend = legend)))), srcfile = NULL)
    lchob <- current.chob()
    ncalls <- length(lchob$Env$call_list)
    lchob$Env$call_list[[ncalls + 1]] <- match.call()
    if (is.null(lchob$Env$theme$ZigZag)) {
      lchob$Env$theme$ZigZag$col <- 4
    }
    x <- lchob$Env$xdata
    xsubset <- lchob$Env$xsubset
    x <- cbind(Hi(x),Lo(x))
    zigzag <- ZigZag(HL = x, change = change, percent = percent, retrace = retrace, 
        lastExtreme = lastExtreme)
    lenv$xdata <- structure(zigzag, .Dimnames=list(NULL, "zigzag"))
    lenv$zigzag <- lchob$Env$TA$zigzag <- zigzag
    
    if (any(is.na(on))) {
        lchob$add_frame(ylim=c(min(zigzag, na.rm=TRUE)*0.975, 
                               max(zigzag, na.rm=TRUE)*1.05), asp=1, fixed=TRUE)
        lchob$next_frame()
    }
    else {
      lchob$set_frame(sign(on)*(abs(on)+1L))
    }
    lchob$replot(exp, env=c(lenv, lchob$Env), expr=TRUE)
    lchob
}
