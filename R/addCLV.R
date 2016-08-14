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
      xdata <- x$Env$xdata
      xsubset <- x$Env$xsubset
      clv <- CLV(HLC=HLC(xdata))[xsubset]
      spacing <- x$Env$theme$spacing
      x.pos <- 1 + spacing * (1:NROW(clv) - 1)
      xlim <- x$Env$xlim
      ylim <- range(clv,na.rm=TRUE)
      theme <- x$Env$theme
      
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
      lc <- xts:::legend.coords("topleft", xlim, range(clv,na.rm=TRUE)),
      legend(x = lc$x, y = lc$y, 
             legend = c(paste(legend, ":"),
                        paste(format(last(clv[xsubset]),nsmall = 3L))),
             text.col = c(theme$fg, theme$CLV$col), 
             xjust = lc$xjust, 
             yjust = lc$yjust, 
             bty = "n", 
             y.intersp=0.95)))
    exp <- c(expression(      
      clv <- TA$clv,
      # add inbox color
      rect(xlim[1], range(clv, na.rm=TRUE)[1], xlim[2], range(clv, na.rm=TRUE)[2], col=theme$fill),
      # add grid lines and left-side axis labels
      segments(xlim[1], y_grid_lines(range(clv, na.rm=TRUE)), 
               xlim[2], y_grid_lines(range(clv, na.rm=TRUE)), 
               col = theme$grid, lwd = x$Env$grid.ticks.lwd, lty = 3),
      text(xlim[1], y_grid_lines(range(clv, na.rm=TRUE)), y_grid_lines(range(clv, na.rm=TRUE)), 
           col = theme$labels, srt = theme$srt, 
           offset = 0.5, pos = 2, cex = theme$cex.axis, xpd = TRUE),
      # add border of plotting area
      rect(xlim[1], range(clv, na.rm=TRUE)[1], xlim[2], range(clv, na.rm=TRUE)[2], border=theme$labels)), exp)
    
    lchob <- current.chob()
    ncalls <- length(lchob$Env$call_list)
    lchob$Env$call_list[[ncalls + 1]] <- match.call()
    if (is.null(lchob$Env$theme$CLV)) {
      lchob$Env$theme$CLV$col <- 5
    }
    xdata <- lchob$Env$xdata
    xsubset <- lchob$Env$xsubset
    clv <- CLV(HLC=HLC(xdata))
    lchob$Env$TA$clv <- clv
    if(is.na(on)) {
      lchob$add_frame(ylim=range(clv,na.rm=TRUE),asp=1,fixed=TRUE)
      lchob$next_frame()
    }
    else {
      lchob$set_frame(sign(on)*abs(on))
    }
    lchob$replot(exp, env=c(lenv,lchob$Env), expr=TRUE)
    lchob
}
