
# addVo {{{
`addVo` <- function(log.scale=FALSE) {
   lchob <- get.current.chob() 
  x <- as.matrix(lchob@xdata)
  if(!lchob@show.vol || !has.Vo(x))
    return(invisible(new('chobTA', new=FALSE, name="chartNULL", call=match.call())))
 
  Volumes <- Vo(x)
  max.vol <- max(Volumes,na.rm=TRUE)
  vol.scale <- list(100, "100s")
  if (max.vol > 10000) 
    vol.scale <- list(1000, "1000s")
  if (max.vol > 1e+05) 
    vol.scale <- list(10000, "10,000s")
  if (max.vol > 1e+06) 
    vol.scale <- list(1e+05, "100,000s")
  if (max.vol > 1e+07) 
    vol.scale <- list(1e+06, "millions")
  
  if(lchob@color.vol & is.OHLC(x)) {
    # calculate colors for bars, if applicable.
    Opens  <- Op(x)
    Closes <- Cl(x)
    if(lchob@multi.col) {
      # colored bars - 4 color
      last.Closes <- as.numeric(Lag(Closes))
      last.Closes[1] <- Closes[1]
      bar.col <- ifelse(Opens < Closes,
                        ifelse(Opens > last.Closes,
                               lchob@colors$dn.up.col,
                               lchob@colors$up.up.col),
                        ifelse(Opens < last.Closes,
                               lchob@colors$dn.dn.col,
                               lchob@colors$up.dn.col))
    } else {
      # colored bars - 2 color
      bar.col <- ifelse(Opens < Closes,
                        lchob@colors$up.col,
                        lchob@colors$dn.col)
    }
      # 1 color bars
  } else bar.col <- ifelse(!is.null(lchob@colors$Vo.bar.col),
                           lchob@colors$Vo.bar.col,lchob@colors$border)
  border.col <- ifelse(is.null(lchob@colors$border),
                       bar.col,lchob@colors$border)

  bar.col <- bar.col[lchob@xsubset]

  chobTA <- new("chobTA")
  chobTA@new <- TRUE

  chobTA@TA.values <- (Volumes/vol.scale[[1]])[lchob@xsubset]
  chobTA@name <- "chartVo"
  chobTA@call <- match.call()
  
  chobTA@params <- list(xrange=lchob@xrange,
                        colors=lchob@colors,
                        color.vol=lchob@color.vol,
                        multi.col=lchob@multi.col,
                        spacing=lchob@spacing,
                        width=lchob@width,
                        bp=lchob@bp,
                        vol.scale=vol.scale,
                        x.labels=lchob@x.labels,
                        log.scale=log.scale,
                        bar.col=bar.col,border.col=border.col,
                        time.scale=lchob@time.scale)

  chobTA@params$thin <- ifelse(lchob@type %in% c('bars','matchsticks'),TRUE,FALSE)

  if(is.null(sys.call(-1))) {
    TA <- lchob@passed.args$TA
    lchob@passed.args$TA <- c(TA,chobTA)
    lchob@windows <- lchob@windows + ifelse(chobTA@new,1,0)
    do.call('chartSeries.chob',list(lchob))
    invisible(chobTA)
  } else {
   return(chobTA)
  } 
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
           text.col=c(x@params$colors$fg.col, last(bar.col)), bty="n", y.inter=0.95)
#   text(0, max(Volumes,na.rm=TRUE) * .9, "Volume:",pos=4)

#   text(0, max(Volumes,na.rm=TRUE) * .9,
#        paste("\n\n\n",format(last(Volumes)*vol.scale[[1]],big.mark=','), sep = ""), 
#        pos = 4,col=last(bar.col))

    axis(2)
    box(col=x@params$colors$fg.col)
    invisible(vector('list',2))
} # }}}

