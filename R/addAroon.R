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
      xsubset <- x$Env$xsubset
      Aroon <- Aroon[xsubset,-3]
      spacing <- x$Env$theme$spacing
      x.pos <- 1 + spacing * (1:NROW(Aroon) - 1)
      xlim <- x$Env$xlim
      ylim <- c(0,100)
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
      
      lines(x.pos, Aroon[,1], col = theme$Aroon$col$aroonUp, 
            lwd = 1, lend = 2, ...)
      lines(x.pos, Aroon[,2], col = theme$Aroon$col$aroonDn, 
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
      frame <- get_frame(),
      lc <- xts:::legend.coords("topleft", xlim, ylim[[frame]]),
      legend(x = lc$x, y = lc$y, 
             legend = c(paste(legend, ":"),
                        paste("aroonUp :",format(last(Aroon[xsubset,1]),nsmall = 3L)), 
                        paste("aroonDn :",format(last(Aroon[xsubset,2]),nsmall = 3L))),
             text.col = c(theme$fg, theme$Aroon$col$aroonUp, theme$Aroon$col$aroonDn), 
             xjust = lc$xjust, 
             yjust = lc$yjust, 
             bty = "n", 
             y.intersp=0.95)))
    
    lchob <- current.chob()
    ncalls <- length(lchob$Env$call_list)
    lchob$Env$call_list[[ncalls + 1]] <- match.call()
    if (is.null(lchob$Env$theme$Aroon)) {
      lchob$Env$theme$Aroon$col$aroonUp <- 3
      lchob$Env$theme$Aroon$col$aroonDn <- 4
      lchob$Env$theme$Aroon$col$aroonOsc <- 3
    }
    xdata <- lchob$Env$xdata
    xdata <- cbind(Hi(xdata),Lo(xdata))
    xsubset <- lchob$Env$xsubset
    Aroon <- aroon(HL=xdata,n=n)[,-3]
    lenv$xdata <- structure(Aroon, .Dimnames = list(NULL, c("aroonUp", "aroonDn")))
    lenv$Aroon <- lchob$Env$TA$Aroon <- Aroon
    lenv$get_frame <- lchob$get_frame
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
      xsubset <- x$Env$xsubset
      AroonOsc <- AroonOsc[xsubset]
      spacing <- x$Env$theme$spacing
      x.pos <- 1 + spacing * (1:NROW(AroonOsc) - 1)
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
      
      lines(x.pos, AroonOsc, col = theme$Aroon$col$aroonOsc, 
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
      frame <- get_frame(),
      lc <- xts:::legend.coords("topleft", xlim, ylim[[frame]]),
      legend(x = lc$x, y = lc$y, 
             legend = c(paste(legend, ":"),
                        paste(format(last(AroonOsc[xsubset]),nsmall = 3L))),
             text.col = c(theme$fg, theme$Aroon$col$aroonOsc), 
             xjust = lc$xjust, 
             yjust = lc$yjust, 
             bty = "n", 
             y.intersp=0.95)))
    
    lchob <- current.chob()
    ncalls <- length(lchob$Env$call_list)
    lchob$Env$call_list[[ncalls + 1]] <- match.call()
    if (is.null(lchob$Env$theme$Aroon)) {
      lchob$Env$theme$Aroon$col$aroonUp <- 3
      lchob$Env$theme$Aroon$col$aroonDn <- 4
      lchob$Env$theme$Aroon$col$aroonOsc <- 3
    }
    xdata <- lchob$Env$xdata
    xdata <- cbind(Hi(xdata),Lo(xdata))
    xsubset <- lchob$Env$xsubset
    AroonOsc <- aroon(HL=xdata,n=n)[,3]
    lenv$xdata <- structure(AroonOsc, .Dimnames = list(NULL, "aroonOsc"))
    lenv$AroonOsc <- lchob$Env$TA$AroonOsc <- AroonOsc
    lenv$get_frame <- lchob$get_frame
    if(is.na(on)) {
      lchob$add_frame(ylim=c(min(lenv$AroonOsc[xsubset],na.rm=TRUE)*0.95, 
                             max(lenv$AroonOsc[xsubset], na.rm=TRUE)*1.05),asp=1,fixed=FALSE)
      lchob$next_frame()
    }
    else {
      lchob$set_frame(sign(on)*(abs(on)+1L))
    }
    lchob$replot(exp, env=c(lenv,lchob$Env), expr=TRUE)
    lchob
}

