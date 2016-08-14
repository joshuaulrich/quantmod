# Arm's Ease of Movement Index by Josh Ulrich from TTR
#
# chartSeries implementation by Jeffrey A. Ryan 2008
#
#  addEMV

`addEMV` <-
function (volume, n = 9, maType, vol.divisor = 10000, ..., on = NA, 
    legend = "auto") 
{
    lenv <- new.env()
    lenv$chartEMV <- function(x, volume, n, maType, vol.divisor, ..., on, legend) {
      xdata <- x$Env$xdata
      xsubset <- x$Env$xsubset
      volume <- x$Env$TA$volume
      emv <- EMV(HL=HLC(xdata)[,-3], volume = volume, n = n, maType = maType, 
                 legend = legend)[xsubset]
      spacing <- x$Env$theme$spacing
      x.pos <- 1 + spacing * (1:NROW(emv) - 1)
      xlim <- x$Env$xlim
      ylim <- range(emv,na.rm=TRUE)*1.05
      theme <- x$Env$theme

      lines(x.pos, emv$emv, col = 6, lwd = 1, lend = 2, ...)
      lines(x.pos, emv$maEMV, col = 7, lwd = 1, lend = 2, ...)
    }
    lchob <- current.chob()
    ncalls <- length(lchob$Env$call_list)
    lchob$Env$call_list[[ncalls + 1]] <- match.call()
    if(missing(volume)) volume <- lchob$Env$vo
    if(missing(maType)) maType <- "SMA"
    if(!is.character(legend) || legend == "auto")
      legend <- gsub("^.*[(]", "Ease of Movement (", deparse(match.call()))
    mapply(function(name, value) {
      assign(name, value, envir = lenv)
    }, names(list(volume = volume, n = n, maType = maType, vol.divisor = vol.divisor, ..., 
                  on = on, legend = legend)), 
    list(volume = volume, n = n, maType = maType, vol.divisor = vol.divisor, ..., 
         on = on, legend = legend))
    exp <- parse(text = gsub("list", "chartEMV", 
                             as.expression(substitute(list(x = current.chob(), volume = volume, n = n, maType = maType, vol.divisor = vol.divisor, ..., 
                                                           on = on, legend = legend)))), srcfile = NULL)
    exp <- c(exp, expression(
      lc <- xts:::legend.coords("topleft", xlim, range(emv,na.rm=TRUE)*1.05),
      legend(x = lc$x, y = lc$y, 
             legend = c(paste(legend, ":"),
                        paste("emv :", sprintf("%.3f",last(emv$emv[xsubset]))),
                        paste("maEMV :", sprintf("%.3f",last(emv$maEMV[xsubset])))),
             text.col = c(theme$fg, 6, 7), 
             xjust = lc$xjust, 
             yjust = lc$yjust, 
             bty = "n", 
             y.intersp=0.95)))
    exp <- c(expression(
      emv <- TA$emv,
      # add inbox color
      rect(xlim[1], range(emv,na.rm=TRUE)[1]*1.05, xlim[2], range(emv,na.rm=TRUE)[2]*1.05, col=theme$fill),
      # add grid lines and left-side axis labels
      segments(xlim[1], y_grid_lines(range(emv,na.rm=TRUE)*1.05), 
               xlim[2], y_grid_lines(range(emv,na.rm=TRUE)*1.05), 
               col = theme$grid, lwd = x$Env$grid.ticks.lwd, lty = 3),
      text(xlim[1], y_grid_lines(range(emv,na.rm=TRUE)*1.05), y_grid_lines(range(emv,na.rm=TRUE)*1.05), 
           col = theme$labels, srt = theme$srt, 
           offset = 0.5, pos = 2, cex = theme$cex.axis, xpd = TRUE),
      # add border of plotting area
      rect(xlim[1], range(emv,na.rm=TRUE)[1]*1.05, xlim[2], range(emv,na.rm=TRUE)[2]*1.05, border=theme$labels)), exp)
    
    xdata <- lchob$Env$xdata
    xsubset <- lchob$Env$xsubset
    emv <- EMV(HL = HLC(xdata)[,-3], volume = volume, n = n, maType = maType, 
               vol.divisor = vol.divisor)
    lchob$Env$TA$emv <- emv
    lchob$Env$TA$volume <- volume
    if(is.na(on)) {
      lchob$add_frame(ylim=range(emv,na.rm=TRUE)*1.05,asp=1,fixed=TRUE)
      lchob$next_frame()
    }
    else {
      lchob$set_frame(sign(on)*abs(on))
    }
    lchob$replot(exp, env=c(lenv,lchob$Env), expr=TRUE)
    lchob
}
