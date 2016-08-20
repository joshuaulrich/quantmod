
# addVo {{{
`addVo` <- function(log.scale=FALSE) {
  lenv <- new.env()
  
  lenv$chartVo <- function(x) {
    xdata <- x$Env$xdata
    xsubset <- x$Env$xsubset
    vo <- x$Env$TA$vo[xsubset]
    
    spacing <- x$Env$theme$spacing
    width <- x$Env$theme$width
    
    x.pos <- 1 + spacing * (1:NROW(vo) - 1)
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
    
    thin <- theme$thin
    
    #    multi.col <- x$Env$multi.col
    color.vol <- x$Env$color.vol
    log.scale <- ifelse(x$Env$log.scale,"y","")
    
    bar.col <- if(color.vol) {
      theme$bar.col[xsubset]
    } else theme$border.col[xsubset]
    
    border.col <- theme$border.col[xsubset]
    
    if(x$Env$theme$thin) {
      # plot thin volume bars if appropriate
      segments(x.pos,ylim[1],x.pos,vo,col=bar.col)
    } else {
      rect(x.pos-spacing/3,ylim[1],x.pos+spacing/3,vo,
           col=bar.col,border=border.col)
    }
  }
  
  exp <- parse(text=gsub("list","chartVo",as.expression(substitute(list(x=current.chob(),...)))),
               srcfile=NULL)
  exp <- c(exp, expression(
    frame <- get_frame(),
    lc <- xts:::legend.coords("topleft", xlim, ylim[[frame]]),
    legend(x = lc$x, y = lc$y, 
           legend = c(paste("Volume (",vol.scale[[2]],"):",sep=''),format(last(TA$vo[xsubset]),big.mark=',')), 
           text.col = c(theme$fg, last(theme$bar.col[xsubset])), 
           xjust = lc$xjust, 
           yjust = lc$yjust, 
           bty = "n", 
           y.intersp=0.95)))
  
  lchob <- current.chob()
  ncalls <- length(lchob$Env$call_list)
  lchob$Env$call_list[[ncalls + 1]] <- match.call()
  xdata <- lchob$Env$vo
  xsubset <- lchob$Env$xsubset
  x <- lchob$Env$xdata
  theme <- lchob$Env$theme
  vo <- xdata
  
  if(lchob$Env$color.vol) {
    # calculate colors for bars, if applicable.
    Opens  <- Op(x)
    Closes <- Cl(x)
    if(lchob$Env$multi.col) {
      # colored bars - 4 color
      last.Closes <- as.numeric(Lag(Closes))
      last.Closes[1] <- Closes[1]
      bar.col <- ifelse(Opens < Closes,
                        ifelse(Opens < last.Closes,
                               lchob$Env$theme$dn.up.col,
                               lchob$Env$theme$up.up.col),
                        ifelse(Opens < last.Closes,
                               lchob$Env$theme$dn.dn.col,
                               lchob$Env$theme$up.dn.col))
    } else {
      # colored bars - 2 color
      bar.col <- ifelse(Opens < Closes,
                        lchob$Env$theme$up.col,
                        lchob$Env$theme$dn.col)
    }
    # 1 color bars
  } else bar.col <- ifelse(rep(!is.null(lchob$Env$theme$Vo.bar.col), NROW(xdata[,1])),
                           lchob$Env$theme$Vo.bar.col,lchob$Env$theme$border)
  border.col <- ifelse(rep(is.null(lchob$Env$theme$border),NROW(xdata[,1])),
                       bar.col,lchob$Env$theme$border)
  
  lchob$Env$theme$border.col <- border.col
  lchob$Env$theme$bar.col <- bar.col
  
  lchob$Env$theme$thin <- ifelse(lchob$Env$type %in% c('bars','matchsticks'),TRUE,FALSE)
  
  max.vol <- max(vo,na.rm=TRUE)
  vol.scale <- list(100, "100s")
  if (max.vol > 10000) 
    vol.scale <- list(1000, "1000s")
  if (max.vol > 1e+05) 
    vol.scale <- list(10000, "10,000s")
  if (max.vol > 1e+06) 
    vol.scale <- list(1e+05, "100,000s")
  if (max.vol > 1e+07) 
    vol.scale <- list(1e+06, "millions")
  lchob$Env$vol.scale <- vol.scale
  lchob$Env$TA$vo <- vo/vol.scale[[1]]
  lenv$get_frame <- lchob$get_frame
  
  lchob$add_frame(ylim=c(min(lchob$Env$TA$vo, na.rm=TRUE), 
                         max(lchob$Env$TA$vo, na.rm=TRUE) * 1.05), asp=1, fixed=TRUE)  # need to have a value set for ylim
  lchob$next_frame()
  lchob$replot(exp,env=c(lenv, lchob$Env),expr=TRUE)
  lchob
} # }}}
# chartVo {{{
`chartVo` <-
function(x) {
  # if volume is to be plotted, do so here
    # scale volume - vol.divisor
    if(class(x) != "chobTA") stop("chartVo requires a suitable chobTA object")
    Volumes <- x@TA.values

    spacing <- x@params$spacing
    width <- x@params$width

    x.range <- x@params$xrange
    x.range <- seq(x.range[1],x.range[2]*spacing)

#    multi.col <- x@params$multi.col
    color.vol <- x@params$color.vol
    log.scale <- ifelse(x@params$log.scale,"y","")

    vol.scale <- x@params$vol.scale

    if(x@new) {
      plot.new()
      plot.window(xlim=c(1, x@params$xrange[2] * spacing),
                  ylim=c(min(Volumes,na.rm=TRUE),max(Volumes,na.rm=TRUE)),
                  log=log.scale)
      coords <- par('usr')
      rect(coords[1],coords[3],coords[2],coords[4],col=x@params$colors$area)
      abline(h=axTicks(2), col=x@params$colors$grid.col, lty='dotted')
    }

    x.pos <- 1 + spacing * (1:length(Volumes) - 1)

    bar.col <- if(x@params$color.vol) {
                 x@params$bar.col
               } else x@params$border.col

    border.col <- x@params$border.col

    if(x@params$thin) {
      # plot thin volume bars if appropriate
      segments(x.pos,0,x.pos,Volumes,col=bar.col)
    } else {
      rect(x.pos-spacing/3,0,x.pos+spacing/3,Volumes,
           col=bar.col,border=border.col)
    }
    legend.text <- list(list(
           legend=c(paste("Volume (",vol.scale[[2]],"):",sep=''),format(last(Volumes)*vol.scale[[1]],big.mark=',')),
           text.col=c(x@params$colors$fg.col, last(bar.col))
           ))
    legend("topleft",
           legend=c(paste("Volume (",vol.scale[[2]],"):",sep=''),format(last(Volumes)*vol.scale[[1]],big.mark=',')),
           text.col=c(x@params$colors$fg.col, last(bar.col)), bty="n", y.intersp=0.95)
#   text(0, max(Volumes,na.rm=TRUE) * .9, "Volume:",pos=4)

#   text(0, max(Volumes,na.rm=TRUE) * .9,
#        paste("\n\n\n",format(last(Volumes)*vol.scale[[1]],big.mark=','), sep = ""), 
#        pos = 4,col=last(bar.col))

    axis(2)
    box(col=x@params$colors$fg.col)
    invisible(vector('list',2))
} # }}}

