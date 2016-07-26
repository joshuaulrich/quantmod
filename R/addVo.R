
# addVo {{{
`addVo` <- function(log.scale=FALSE, ...) {
  lenv <- new.env()
  
  lenv$chartVo <- function(x, ...) {
    xdata <- x$Env$xdata
    xsubset <- x$Env$xsubset
    vo <- x$Env$vo
    
    spacing <- x$Env$theme$spacing
    width <- x$Env$theme$width
    
    x.range <- x$get_xlim()
    x.range <- seq(x.range[1],x.range[2]*spacing)
    
    #    multi.col <- x$Env$multi.col
    color.vol <- x$Env$color.vol
    log.scale <- ifelse(x$Env$log.scale,"y","")
    
    vol.scale <- x$Env$vol.scale
    
    x.pos <- 1 + spacing * (1:length(vo) - 1)
    
    bar.col <- if(x$Env$color.vol) {
      x$Env$theme$bar.col
    } else x$Env$theme$border.col
    
    border.col <- x$Env$theme$border.col
    min.vol <- min(vo)
    
    if(x$Env$theme$thin) {
      # plot thin volume bars if appropriate
      segments(x.pos,min.vol,x.pos,vo,col=bar.col)
    } else {
      rect(x.pos-spacing/3,min.vol,x.pos+spacing/3,vo,
           col=bar.col,border=border.col)
    }
    legend.text <- list(list(
      legend=c(paste("Volume (",vol.scale[[2]],"):",sep=''),format(last(vo)*vol.scale[[1]],big.mark=',')),
      text.col=c(x$Env$theme$fg, last(bar.col))
    ))
    lc <- xts:::legend.coords("topleft", x$Env$xlim, range(vo))
    legend(x = lc$x, y = lc$y, 
           legend = c(paste("Volume (",vol.scale[[2]],"):",sep=''),format(last(x$Env$TA.values)*vol.scale[[1]],big.mark=',')), 
           text.col = c(x$Env$theme$fg, last(bar.col)), 
           xjust = lc$xjust, 
           yjust = lc$yjust, 
           bty = "n", 
           y.intersp=0.95)
  }
  
  # map all passed args (if any) to 'lenv' environment
  mapply(function(name,value) { assign(name,value,envir=lenv) }, names(list(...)), list(...))
  exp <- parse(text=gsub("list","chartVo",as.expression(substitute(list(x=current.chob(),...)))),
               srcfile=NULL)
  lchob <- current.chob() 
  xdata <- lchob$Env$vo
  xsubset <- lchob$Env$xsubset
  x <- lchob$Env$xdata
  theme <- lchob$Env$theme
  vo <- xdata[xsubset]
  lchob$Env$vo <- vo
  yrange <- c(range(vo, na.rm=TRUE)[1], range(vo, na.rm=TRUE)[2] * 1.05)
  lenv$xdata <- xdata
  
  # add inbox color
  exp <- c(expression(yrange <- c(range(vo, na.rm=TRUE)[1], range(vo, na.rm=TRUE)[2] * 1.05), rect(xlim[1], yrange[1], xlim[2], yrange[2],col=theme$fill)),
           # add grid lines and left-side axis labels
           expression(segments(xlim[1], y_grid_lines(yrange), xlim[2], 
                               y_grid_lines(yrange), col = theme$grid, lwd = grid.ticks.lwd, 
                               lty = 3), 
                      text(xlim[1], y_grid_lines(yrange), y_grid_lines(range(TA.values)), 
                           col = theme$labels, srt = theme$srt, 
                           offset = 0.5, pos = 2, cex = theme$cex.axis, xpd = TRUE)),
           # add border of plotting area
           expression(rect(xlim[1], yrange[1], xlim[2], yrange[2],border=theme$labels)),exp)
  
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
  } else bar.col <- ifelse(rep(!is.null(lchob$Env$theme$Vo.bar.col), NROW(x)),
                           lchob$Env$theme$Vo.bar.col,lchob$Env$theme$border)
  border.col <- ifelse(rep(is.null(lchob$Env$theme$border),NROW(x)),
                       bar.col,lchob$Env$theme$border)

  bar.col <- bar.col[lchob$Env$xsubset]
  
  lchob$Env$theme$border.col <- border.col
  lchob$Env$theme$bar.col <- bar.col

  lchob$Env$vol.scale <- vol.scale
  lchob$Env$TA.values <- vo/vol.scale[[1]]

  lchob$Env$theme$thin <- ifelse(lchob$Env$type %in% c('bars','matchsticks'),TRUE,FALSE)
  
  lchob$add_frame(ylim=yrange, asp=1, fixed=TRUE)  # need to have a value set for ylim
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

