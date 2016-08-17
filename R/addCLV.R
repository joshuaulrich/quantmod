# Close Location Value from TTR by Josh Ulrich
#
# chartSeries implementation by Jeffrey A. Ryan 2008
#
#  addCLV

`addCLV` <-
function (..., on = NA, legend = "auto") 
{
    lenv <- new.env()
    lenv$chartCLV <- function(x, ..., on, legend) {
      xsubset <- x$Env$xsubset
      clv <- clv[xsubset]
      spacing <- x$Env$theme$spacing
      x.pos <- 1 + spacing * (1:NROW(clv) - 1)
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
      
      lines(x.pos, clv, type = "h", col = theme$CLV$col, 
            lwd = 1, lend = 2, ...)
    }
    if(!is.character(legend) || legend == "auto")
      legend <- gsub("^addCLV", "Close Location Value", deparse(match.call()))
    mapply(function(name, value) {
      assign(name, value, envir = lenv)
    }, names(list(..., on = on, legend = legend)), 
    list(..., on = on, legend = legend))
    exp <- parse(text = gsub("list", "chartCLV", as.expression(substitute(list(x = current.chob(), 
                                                                                    ..., on = on, legend = legend)))), srcfile = NULL)
    exp <- c(exp, expression(
      frame <- get_frame(),
      lc <- xts:::legend.coords("topleft", xlim, ylim[[frame]]),
      legend(x = lc$x, y = lc$y, 
             legend = c(paste(legend, ":"),
                        paste(format(last(clv[xsubset]),nsmall = 3L))),
             text.col = c(theme$fg, theme$CLV$col), 
             xjust = lc$xjust, 
             yjust = lc$yjust, 
             bty = "n", 
             y.intersp=0.95)))
    
    lchob <- current.chob()
    ncalls <- length(lchob$Env$call_list)
    lchob$Env$call_list[[ncalls + 1]] <- match.call()
    if (is.null(lchob$Env$theme$CLV)) {
      lchob$Env$theme$CLV$col <- 5
    }
    xdata <- lchob$Env$xdata
    xsubset <- lchob$Env$xsubset
    clv <- CLV(HLC=HLC(xdata))
    lenv$xdata <- structure(clv, .Dimnames = list(NULL, "clv"))
    lenv$clv <- lchob$Env$TA$clv <- clv
    lenv$get_frame <- lchob$get_frame
    if(is.na(on)) {
      lchob$add_frame(ylim=range(lenv$clv[xsubset],na.rm=TRUE),asp=1,fixed=FALSE)
      lchob$next_frame()
    }
    else {
      lchob$set_frame(sign(on)*abs(on))
    }
    lchob$replot(exp, env=c(lenv,lchob$Env), expr=TRUE)
    lchob
}
