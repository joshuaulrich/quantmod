# Know Sure Thing from TTR by Josh Ulrich
#
# chartSeries interface by Jeffrey A. Ryan 2008
#
#   addKST
#

`addKST` <-
function (n = c(10, 10, 10, 15), nROC = c(10, 15, 20, 30), nSig = 9, 
    maType, wts = 1:NROW(n), ..., on = NA, legend = "auto") 
{
    lenv <- new.env()
    lenv$chartKST <- function(x, n, nROC, nSig, maType, wts, ..., on, legend) {
      xsubset <- x$Env$xsubset
      kst <- kst[xsubset]
      spacing <- x$Env$theme$spacing
      x.pos <- 1 + spacing * (1:NROW(kst) - 1)
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
      
      
      lines(x.pos, kst[,1], col = theme$KST$col$kst, lwd = 1, lend = 2, ...)
      lines(x.pos, kst[,2], col = theme$KST$col$signal, lwd = 1, lend = 2, ...)
    }
    if(missing(maType)) maType <- "SMA"
    if(!is.character(legend) || legend == "auto")
      legend <- gsub("^addKST", "Know Sure Thing", deparse(match.call()))
    mapply(function(name, value) {
      assign(name, value, envir = lenv)
    }, names(list(n = n, nROC = nROC, nSig = nSig, 
                  maType = maType, wts = wts, ..., on = on, legend = legend)), 
    list(n = n, nROC = nROC, nSig = nSig, 
         maType = maType, wts = wts, ..., on = on, legend = legend))
    exp <- parse(text = gsub("list", "chartKST", as.expression(substitute(list(x = current.chob(), 
                                                                               n = n, nROC = nROC, nSig = nSig, 
                                                                               maType = maType, wts = wts, ..., on = on, legend = legend)))), srcfile = NULL)
    exp <- c(exp, expression(
      frame <- get_frame(),
      lc <- xts:::legend.coords("topleft", xlim, ylim[[frame]]),
      legend(x = lc$x, y = lc$y, 
             legend = c(legend,
                        paste("kst :",format(last(kst[xsubset,1]),nsmall = 3L)), 
                        paste("signal :",format(last(kst[xsubset,2]),nsmall = 3L))),
             text.col = c(theme$fg, theme$KST$col$kst, theme$KST$col$signal), 
             xjust = lc$xjust, 
             yjust = lc$yjust, 
             bty = "n", 
             y.intersp=0.95)))
    
    lchob <- current.chob()
    ncalls <- length(lchob$Env$call_list)
    lchob$Env$call_list[[ncalls + 1]] <- match.call()
    if (is.null(lchob$Env$theme$KST)) {
      lchob$Env$theme$KST$col$kst <- 6
      lchob$Env$theme$KST$col$signal <- 7
    }
    x <- lchob$Env$xdata
    xsubset <- lchob$Env$xsubset
    x <- Cl(x)
    kst <- KST(price = x, n = n, nROC = nROC, nSig = nSig, maType = maType, 
        wts = wts)
    lenv$xdata <- structure(kst, .Dimnames=list(NULL, c("kst", "signal")))
    lenv$kst <- lchob$Env$TA$kst <- kst
    lenv$get_frame <- lchob$get_frame
    if(is.na(on)) {
      lchob$add_frame(ylim=range(lenv$kst[xsubset], na.rm=TRUE) * 1.05,asp=1,fixed=FALSE)
      lchob$next_frame()
    }
    else {
      lchob$set_frame(sign(on)*(abs(on)+1L))
    }
    lchob$replot(exp, env=c(lenv,lchob$Env), expr=TRUE)
    lchob
}