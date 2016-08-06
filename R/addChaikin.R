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
      xdata <- x$Env$xdata
      xsubset <- x$Env$xsubset
      vo <- x$Env$vo
      ChaikinAD <- chaikinAD(HLC = HLC(xdata), volume = vo)[xsubset]
      spacing <- x$Env$theme$spacing
      x.pos <- 1 + spacing * (1:NROW(ChaikinAD) - 1)
      xlim <- x$Env$xlim
      ylim <- range(ChaikinAD,na.rm=TRUE)
      theme <- x$Env$theme

      lines(x.pos, ChaikinAD, col = theme$chaikin$col$chaikinad, 
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
      lc <- xts:::legend.coords("topleft", xlim, range(ChaikinAD,na.rm=TRUE)),
      legend(x = lc$x, y = lc$y, 
             legend = c(paste(legend, ":"),
                        paste(format(last(ChaikinAD),nsmall = 3L))),
             text.col = c(theme$fg, theme$chaikin$col$chaikinad), 
             xjust = lc$xjust, 
             yjust = lc$yjust, 
             bty = "n", 
             y.intersp=0.95)))
    exp <- c(expression(      
      ChaikinAD <- TA$ChaikinAD,
      # add inbox color
      rect(xlim[1], range(ChaikinAD, na.rm=TRUE)[1], xlim[2], range(ChaikinAD, na.rm=TRUE)[2], col=theme$fill),
      # add grid lines and left-side axis labels
      segments(xlim[1], y_grid_lines(range(ChaikinAD, na.rm=TRUE)), 
               xlim[2], y_grid_lines(range(ChaikinAD, na.rm=TRUE)), 
               col = theme$grid, lwd = x$Env$grid.ticks.lwd, lty = 3),
      text(xlim[1], y_grid_lines(range(ChaikinAD, na.rm=TRUE)), y_grid_lines(range(ChaikinAD, na.rm=TRUE)), 
           col = theme$labels, srt = theme$srt, 
           offset = 0.5, pos = 2, cex = theme$cex.axis, xpd = TRUE),
      # add border of plotting area
      rect(xlim[1], range(ChaikinAD, na.rm=TRUE)[1], xlim[2], range(ChaikinAD, na.rm=TRUE)[2], border=theme$labels)), exp)
    
    lchob <- current.chob()
    ncalls <- length(lchob$Env$call_list)
    lchob$Env$call_list[[ncalls + 1]] <- match.call()
    if (is.null(lchob$Env$theme$chaikin$col$chaikinad)) {
      lchob$Env$theme$chaikin$col$chaikinad <- 3
    }
    xdata <- lchob$Env$xdata
    xsubset <- lchob$Env$xsubset
    vo <- lchob$Env$vo
    ChaikinAD <- chaikinAD(HLC = HLC(xdata), volume = vo)[xsubset]
    lchob$Env$TA$ChaikinAD <- ChaikinAD
    if(is.na(on)) {
      lchob$add_frame(ylim=range(ChaikinAD,na.rm=TRUE),asp=1,fixed=TRUE)
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
      xdata <- x$Env$xdata
      xsubset <- x$Env$xsubset
      ChaikinVol <- chaikinVolatility(HL = HLC(xdata)[,-3], n = n, maType = maType)[xsubset]
      spacing <- x$Env$theme$spacing
      x.pos <- 1 + spacing * (1:NROW(ChaikinVol) - 1)
      xlim <- x$Env$xlim
      ylim <- range(ChaikinVol,na.rm=TRUE)
      theme <- x$Env$theme
      
      lines(x.pos, ChaikinVol, col = theme$chaikin$col$chaikinvol, 
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
      lc <- xts:::legend.coords("topleft", xlim, range(ChaikinVol,na.rm=TRUE)),
      legend(x = lc$x, y = lc$y, 
             legend = c(paste(legend, ":"),
                        paste(format(last(ChaikinVol),nsmall = 3L))),
             text.col = c(theme$fg, theme$chaikin$col$chaikinvol), 
             xjust = lc$xjust, 
             yjust = lc$yjust, 
             bty = "n", 
             y.intersp=0.95)))
    exp <- c(expression(   
      ChaikinVol <- TA$ChaikinVol,
      # add inbox color
      rect(xlim[1], range(ChaikinVol, na.rm=TRUE)[1], xlim[2], range(ChaikinVol, na.rm=TRUE)[2], col=theme$fill),
      # add grid lines and left-side axis labels
      segments(xlim[1], y_grid_lines(range(ChaikinVol, na.rm=TRUE)), 
               xlim[2], y_grid_lines(range(ChaikinVol, na.rm=TRUE)), 
               col = theme$grid, lwd = x$Env$grid.ticks.lwd, lty = 3),
      text(xlim[1], y_grid_lines(range(ChaikinVol, na.rm=TRUE)), y_grid_lines(range(ChaikinVol, na.rm=TRUE)), 
           col = theme$labels, srt = theme$srt, 
           offset = 0.5, pos = 2, cex = theme$cex.axis, xpd = TRUE),
      # add border of plotting area
      rect(xlim[1], range(ChaikinVol, na.rm=TRUE)[1], xlim[2], range(ChaikinVol, na.rm=TRUE)[2], border=theme$labels)), exp)
    lchob <- current.chob()
    ncalls <- length(lchob$Env$call_list)
    lchob$Env$call_list[[ncalls + 1]] <- match.call()
    if (is.null(lchob$Env$theme$chaikin$col$chaikinvol)) {
      lchob$Env$theme$chaikin$col$chaikinvol <- "#F5F5F5"
    }
    xdata <- lchob$Env$xdata
    xsubset <- lchob$Env$xsubset
    ChaikinVol <- chaikinVolatility(HL = HLC(xdata)[,-3], n = n, maType = maType)[xsubset]
    lchob$Env$TA$ChaikinVol <- ChaikinVol
    if(is.na(on)) {
      lchob$add_frame(ylim=range(ChaikinVol,na.rm=TRUE),asp=1,fixed=TRUE)
      lchob$next_frame()
    }
    else {
      lchob$set_frame(sign(on)*(abs(on)+1L))
    }
    lchob$replot(exp, env=c(lenv,lchob$Env), expr=TRUE)
    lchob
}
