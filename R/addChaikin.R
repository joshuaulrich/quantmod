# Chaikin Functions
# chaikinAD and chaikinVolatility by Josh Ulrich from TTR
#
# chartSeries implementation by Jeffrey A. Ryan 2008
#  
#   addChAD
#   addChVol

`addChAD` <-
function (..., on = NA, legend = "auto") 
{
    lenv <- new.env()
    lenv$chartChAD <- function(x, ..., on, legend) {
      xsubset <- x$Env$xsubset
      ChaikinAD <- ChaikinAD[xsubset]
      spacing <- x$Env$theme$spacing
      x.pos <- 1 + spacing * (1:NROW(ChaikinAD) - 1)
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
      
      lines(x.pos, ChaikinAD, col = theme$ChAD$col$chaikinAD, 
            lwd = 1, lend = 2, ...)
    }
    if(!is.character(legend) || legend == "auto")
      legend <- gsub("^addChAD", "Chaikin Acc/Dist", deparse(match.call()))
    mapply(function(name, value) {
      assign(name, value, envir = lenv)
    }, names(list(..., on = on, legend = legend)), 
    list(..., on = on, legend = legend))
    exp <- parse(text = gsub("list", "chartChAD", as.expression(substitute(list(x = current.chob(), 
                                                                                ..., on = on, legend = legend)))), srcfile = NULL)
    exp <- c(exp, expression(
      frame <- get_frame(),
      lc <- xts:::legend.coords("topleft", xlim, ylim[[frame]]),
      legend(x = lc$x, y = lc$y, 
             legend = c(paste(legend, ":"),
                        paste(format(last(ChaikinAD[xsubset]),nsmall = 3L))),
             text.col = c(theme$fg, theme$ChAD$col$chaikinAD), 
             xjust = lc$xjust, 
             yjust = lc$yjust, 
             bty = "n", 
             y.intersp=0.95)))
    
    lchob <- current.chob()
    ncalls <- length(lchob$Env$call_list)
    lchob$Env$call_list[[ncalls + 1]] <- match.call()
    if (is.null(lchob$Env$theme$ChAD)) {
      lchob$Env$theme$ChAD$col$chaikinAD <- 3
    }
    xdata <- lchob$Env$xdata
    xsubset <- lchob$Env$xsubset
    vo <- lchob$Env$vo
    ChaikinAD <- chaikinAD(HLC = HLC(xdata), volume = vo)
    lenv$xdata <- structure(ChaikinAD, .Dimnames=list(NULL, "ChaikinAD"))
    lenv$ChaikinAD <- lchob$Env$TA$ChaikinAD <- ChaikinAD
    lenv$get_frame <- lchob$get_frame
    if(is.na(on)) {
      lchob$add_frame(ylim=range(lenv$ChaikinAD[xsubset],na.rm=TRUE),asp=1,fixed=FALSE)
      lchob$next_frame()
    }
    else {
      lchob$set_frame(sign(on)*(abs(on)+1L))
    }
    lchob$replot(exp, env=c(lenv,lchob$Env), expr=TRUE)
    lchob
}

`addChVol` <-
function (n = 10, maType, ..., on = NA, legend = "auto") 
{
    lenv <- new.env()
    lenv$chartChVol <- function(x, n, maType, ..., on, legend) {
      xsubset <- x$Env$xsubset
      ChaikinVol <- ChaikinVol[xsubset]
      spacing <- x$Env$theme$spacing
      x.pos <- 1 + spacing * (1:NROW(ChaikinVol) - 1)
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
      
      lines(x.pos, ChaikinVol, col = theme$ChVol$col$chaikinVol, 
            lwd = 1, lend = 2, ...)
    }
    if(missing(maType)) maType <- "SMA"
    if(!is.character(legend) || legend == "auto")
      legend <- gsub("^addChVol", "Chaikin Volatility", deparse(match.call()))
    mapply(function(name, value) {
      assign(name, value, envir = lenv)
    }, names(list(n = n, maType = maType, ..., on = on, legend = legend)), 
    list(n = n, maType = maType, ..., on = on, legend = legend))
    exp <- parse(text = gsub("list", "chartChVol", as.expression(substitute(list(x = current.chob(), 
                                                                                 n = n, maType = maType, ..., on = on, legend = legend)))), srcfile = NULL)
    exp <- c(exp, expression(
      frame <- get_frame(),
      lc <- xts:::legend.coords("topleft", xlim, ylim[[frame]]),
      legend(x = lc$x, y = lc$y, 
             legend = c(paste(legend, ":"),
                        paste(sprintf("%.3f", last(ChaikinVol[xsubset])))),
             text.col = c(theme$fg, theme$ChVol$col$chaikinVol), 
             xjust = lc$xjust, 
             yjust = lc$yjust, 
             bty = "n", 
             y.intersp=0.95)))

    lchob <- current.chob()
    ncalls <- length(lchob$Env$call_list)
    lchob$Env$call_list[[ncalls + 1]] <- match.call()
    if (is.null(lchob$Env$theme$ChVol)) {
      lchob$Env$theme$ChVol$col$chaikinVol <- "#F5F5F5"
    }
    xdata <- lchob$Env$xdata
    xsubset <- lchob$Env$xsubset
    ChaikinVol <- chaikinVolatility(HL = HLC(xdata)[,-3], n = n, maType = maType)
    lenv$xdata <- structure(ChaikinVol, .Dimnames=list(NULL, "ChaikinVol"))
    lenv$ChaikinVol <- lchob$Env$TA$ChaikinVol <- ChaikinVol
    lenv$get_frame <- lchob$get_frame
    if(is.na(on)) {
      lchob$add_frame(ylim=range(lenv$ChaikinVol[xsubset],na.rm=TRUE),asp=1,fixed=FALSE)
      lchob$next_frame()
    }
    else {
      lchob$set_frame(sign(on)*(abs(on)+1L))
    }
    lchob$replot(exp, env=c(lenv,lchob$Env), expr=TRUE)
    lchob
}
