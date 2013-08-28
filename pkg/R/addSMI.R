
# addSMI {{{
`addSMI` <- function(n=13,slow=25,fast=2,signal=9,ma.type='EMA') {


  lchob <- get.current.chob()
  
  x <- as.matrix(lchob@xdata)

  chobTA <- new("chobTA")
  chobTA@new <- TRUE

  xx <- if(is.OHLC(x)) {
    cbind(Hi(x),Lo(x),Cl(x))
  } else if(is.null(dim(x))) {
    x
  } else {
    x[,1] 
  }

  smi <- SMI(xx, n=n, nFast=fast,
             nSlow=slow, nSig=signal, maType=ma.type)

# subset here
# smi <- smi[lchob@sindex]

  chobTA@TA.values <- smi[lchob@xsubset,]
  chobTA@name <- "chartSMI"
  chobTA@call <- match.call()
  chobTA@params <- list(xrange=lchob@xrange,
                        colors=lchob@colors,
                        color.vol=lchob@color.vol,
                        multi.col=lchob@multi.col,
                        spacing=lchob@spacing,
                        width=lchob@width,
                        bp=lchob@bp,
                        x.labels=lchob@x.labels,
                        time.scale=lchob@time.scale,
                        n=n,slow=slow,fast=fast,signal=signal,
                        ma.type=ma.type)
  #if(is.null(sys.call(-1))) {
  #  TA <- lchob@passed.args$TA
  #  lchob@passed.args$TA <- c(TA,chobTA)
  #  lchob@windows <- lchob@windows + ifelse(chobTA@new,1,0)
  #  do.call('chartSeries.chob',list(lchob))
  #  invisible(chobTA)
  #} else {
  return(chobTA)
  #} 
} #}}}
# chartSMI {{{
`chartSMI` <-
function(x) {
    spacing <- x@params$spacing
    width <- x@params$width

    x.range <- x@params$xrange
    x.range <- seq(x.range[1],x.range[2]*spacing)

    multi.col <- x@params$multi.col
    color.vol <- x@params$color.vol

    smi <- x@TA.values

    y.range <- seq(-max(abs(smi[,1]), na.rm = TRUE), max(abs(smi[,1]), 
                   na.rm = TRUE), length.out = length(x.range)) * 1.05

    if(x@new) {
      plot(x.range,y.range,type='n',axes=FALSE,ann=FALSE)
  
      coords <- par('usr')
      rect(coords[1],coords[3],coords[2],coords[4],col=x@params$colors$area)
      grid(NA,NULL,col=x@params$colors$grid.col)
    } 
    COLOR <- "#0033CC"
    SIGNAL <- "#BFCFFF"

    lines(seq(1,length(x.range),by=spacing),
          smi[,1],col=COLOR,lwd=1,type='l')
    lines(seq(1,length(x.range),by=spacing),
          smi[,2],col=SIGNAL,lwd=1,lty='dotted',type='l')

    text(0, last(y.range) * .9,
         paste("Stochastic Momentum Index (",
         paste(x@params$n,x@params$fast,x@params$slow,x@params$signal,sep=','),
         "):", sep = ""), 
         pos = 4)

    text(0, last(y.range)*.9,
        paste("\n\n\nSMI: ",sprintf("%.3f",last(smi[,1])), sep = ""), col = COLOR, 
        pos = 4)

    text(0, last(y.range)*.9,
        paste("\n\n\n\n\nSignal: ",
              sprintf("%.3f",last(smi[,2])), sep = ""), col = SIGNAL, 
        pos = 4)

    axis(2)
    box(col=x@params$colors$fg.col)
} # }}}

