# aroon from TTR
# 
# chartSeries interface by Jeffrey A. Ryan 2008
#
#  addAroon
#  addAroonOsc

`addAroon` <-
function (n = 20, ..., on = NA, legend = "auto") 
{
    lenv <- new.env()
    lenv$chartAroon <- function(x, n, ..., on, legend) {
      xdata <- x$Env$xdata
      xsubset <- x$Env$xsubset
      xdata <- cbind(Hi(xdata),Lo(xdata))
      Aroon <- aroon(HL=xdata,n=n)[xsubset,-3]
      spacing <- x$Env$theme$spacing
      x.pos <- 1 + spacing * (1:NROW(Aroon) - 1)
      xlim <- x$Env$xlim
      ylim <- c(0,100)
      theme <- x$Env$theme
      
      lines(x.pos, Aroon[,1], col = theme$aroon$col$aroonUp, 
            lwd = 1, lend = 2, ...)
      lines(x.pos, Aroon[,2], col = theme$aroon$col$aroonDn, 
            lwd = 1, lend = 2, ...)
    }
    if(!is.character(legend) || legend == "auto")
      legend <- gsub("^add", "", deparse(match.call()))
    mapply(function(name, value) {
      assign(name, value, envir = lenv)
    }, names(list(n = n, ..., on = on, legend = legend)), 
    list(n = n, ..., on = on, legend = legend))
    exp <- parse(text = gsub("list", "chartAroon", as.expression(substitute(list(x = current.chob(), 
                                                                               n = n, ..., on = on, legend = legend)))), srcfile = NULL)
    exp <- c(exp, expression(
      lc <- xts:::legend.coords("topleft", xlim, range(Aroon,na.rm=TRUE)),
      legend(x = lc$x, y = lc$y, 
             legend = c(paste(legend, ":"),
                        paste("aroonUp :",format(last(Aroon[,1]),nsmall = 3L)), 
                        paste("aroonDn :",format(last(Aroon[,2]),nsmall = 3L))),
             text.col = c(theme$fg, theme$aroon$col$aroonUp, theme$aroon$col$aroonDn), 
             xjust = lc$xjust, 
             yjust = lc$yjust, 
             bty = "n", 
             y.intersp=0.95)))
    exp <- c(expression(
      # add inbox color
      rect(xlim[1], 0, xlim[2], 100, col=theme$fill),
      # add grid lines and left-side axis labels
      segments(xlim[1], y_grid_lines(c(0, 100)), 
               xlim[2], y_grid_lines(c(0, 100)), 
               col = theme$grid, lwd = x$Env$grid.ticks.lwd, lty = 3),
      text(xlim[1], y_grid_lines(c(0, 100)), y_grid_lines(c(0, 100)), 
           col = theme$labels, srt = theme$srt, 
           offset = 0.5, pos = 2, cex = theme$cex.axis, xpd = TRUE),
      # add border of plotting area
      rect(xlim[1], 0, xlim[2], 100, border=theme$labels)), exp)
    
    lchob <- current.chob()
    if (is.null(lchob$Env$theme$aroon$col$arronUp)) {
      lchob$Env$theme$aroon$col$aroonUp <- 3
      lchob$Env$theme$aroon$col$aroonDn <- 4
    }
    xdata <- lchob$Env$xdata
    xdata <- cbind(Hi(xdata),Lo(xdata))
    xsubset <- lchob$Env$xsubset
    Aroon <- aroon(HL=xdata,n=n)[xsubset,-3]
    lchob$Env$Aroon <- Aroon
#    lenv$xdata <- structure(Aroon, .Dimnames = list(NULL, "aroon"))
    if(is.na(on)) {
      lchob$add_frame(ylim=c(0,100),asp=1,fixed=TRUE)
      lchob$next_frame()
    }
    else {
      lchob$set_frame(sign(on)*(abs(on)+1L))
    }
    lchob$replot(exp, env=c(lenv,lchob$Env), expr=TRUE)
    lchob
}

`addAroonOsc` <-
function (n = 20, ..., on = NA, legend = "auto") 
{
    lenv <- new.env()
    lenv$chartAroonOsc <- function(x, n, ..., on, legend) {
      xdata <- x$Env$xdata
      xsubset <- x$Env$xsubset
      xdata <- cbind(Hi(xdata),Lo(xdata))
      AroonOsc <- aroon(HL=xdata,n=n)[xsubset,3]
      spacing <- x$Env$theme$spacing
      x.pos <- 1 + spacing * (1:NROW(AroonOsc) - 1)
      xlim <- x$Env$xlim
      ylim <- range(AroonOsc,na.rm=TRUE)
      theme <- x$Env$theme
      
      lines(x.pos, AroonOsc, col = theme$aroon$col$aroonOsc, 
            lwd = 1, lend = 2, ...)
    }
    if(!is.character(legend) || legend == "auto")
      legend <- gsub("^addAroonOsc", "Aroon Oscillator", deparse(match.call()))
    mapply(function(name, value) {
      assign(name, value, envir = lenv)
    }, names(list(n = n, ..., on = on, legend = legend)), 
    list(n = n, ..., on = on, legend = legend))
    exp <- parse(text = gsub("list", "chartAroonOsc", as.expression(substitute(list(x = current.chob(), 
                                                                                 n = n, ..., on = on, legend = legend)))), srcfile = NULL)
    exp <- c(exp, expression(
      lc <- xts:::legend.coords("topleft", xlim, range(AroonOsc,na.rm=TRUE)),
      legend(x = lc$x, y = lc$y, 
             legend = c(paste(legend, ":"),
                        paste(format(last(AroonOsc),nsmall = 3L))),
             text.col = c(theme$fg, 4), 
             xjust = lc$xjust, 
             yjust = lc$yjust, 
             bty = "n", 
             y.intersp=0.95)))
    exp <- c(expression(
      # add inbox color
      rect(xlim[1], c(min(AroonOsc,na.rm=TRUE)*0.95, max(AroonOsc)*1.05)[1], xlim[2], c(min(AroonOsc,na.rm=TRUE)*0.95, max(AroonOsc)*1.05)[2], col=theme$fill),
      # add grid lines and left-side axis labels
      segments(xlim[1], y_grid_lines(c(min(AroonOsc,na.rm=TRUE)*0.95, max(AroonOsc, na.rm=TRUE)*1.05)), 
               xlim[2], y_grid_lines(c(min(AroonOsc,na.rm=TRUE)*0.95, max(AroonOsc, na.rm=TRUE)*1.05)), 
               col = theme$grid, lwd = x$Env$grid.ticks.lwd, lty = 3),
      text(xlim[1], y_grid_lines(c(min(AroonOsc,na.rm=TRUE)*0.95, max(AroonOsc, na.rm=TRUE)*1.05)), y_grid_lines(c(min(AroonOsc,na.rm=TRUE)*0.95, max(AroonOsc, na.rm=TRUE)*1.05)), 
           col = theme$labels, srt = theme$srt, 
           offset = 0.5, pos = 2, cex = theme$cex.axis, xpd = TRUE),
      # add border of plotting area
      rect(xlim[1], c(min(AroonOsc,na.rm=TRUE)*0.95, max(AroonOsc, na.rm=TRUE)*1.05)[1], xlim[2], c(min(AroonOsc,na.rm=TRUE)*0.95, max(AroonOsc, na.rm=TRUE)*1.05)[2], border=theme$labels)), exp)
    
    lchob <- current.chob()
    if (is.null(lchob$Env$theme$aroon$col$aroonOsc)) {
      lchob$Env$theme$aroon$col$aroonOsc <- 3
    }
    xdata <- lchob$Env$xdata
    xdata <- cbind(Hi(xdata),Lo(xdata))
    xsubset <- lchob$Env$xsubset
    AroonOsc <- aroon(HL=xdata,n=n)[xsubset,3]
    lchob$Env$AroonOsc <- AroonOsc
    if(is.na(on)) {
      lchob$add_frame(ylim=c(min(AroonOsc,na.rm=TRUE)*0.95, max(AroonOsc, na.rm=TRUE)*1.05),asp=1,fixed=TRUE)
      lchob$next_frame()
    }
    else {
      lchob$set_frame(sign(on)*(abs(on)+1L))
    }
    lchob$replot(exp, env=c(lenv,lchob$Env), expr=TRUE)
    lchob
}

