
# addWPR {{{
`addWPR` <- function(n=14) {


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


  wpr <- WPR(xx,n=n)

  chobTA@TA.values <- as.numeric(wpr)[lchob@xsubset]
  chobTA@name <- "chartWPR"
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
                        n=n)
  if(is.null(sys.call(-1))) {
    TA <- lchob@passed.args$TA
    lchob@passed.args$TA <- c(TA,chobTA)
    lchob@windows <- lchob@windows + ifelse(chobTA@new,1,0)
    do.call('chartSeries.chob',list(lchob))
    invisible(chobTA)
  } else {
   return(chobTA)
  } 
} #}}}
# chartWPR {{{
`chartWPR` <-
function(x) {
    spacing <- x@params$spacing
    width <- x@params$width

    x.range <- x@params$xrange
    x.range <- seq(x.range[1],x.range[2]*spacing)

    multi.col <- x@params$multi.col
    color.vol <- x@params$color.vol

    n <- x@params$n
    wpr <- x@TA.values

    y.range <- seq(-0.1, max(abs(wpr), 
                   na.rm = TRUE), length.out = length(x.range)) * 1.05

    # create appropriately scaled empty plot area
    plot(x.range,y.range,type='n',axes=FALSE,ann=FALSE)

    coords <- par('usr')
    rect(coords[1],coords[3],coords[2],coords[4],col=x@params$colors$area)
    grid(NA,NULL,col=x@params$colors$grid.col)

    COLOR <- "#0033CC"

    lines(seq(1,length(x.range),by=spacing),wpr,col=COLOR,lwd=1,type='l')

    text(0, last(y.range)*.9,
         paste("Williams %R (", x@params$n,"):", sep = ""), 
        pos = 4)

    text(0, last(y.range)*.9,
        paste("\n\n\n",sprintf("%.3f",last(wpr)), sep = ""), col = COLOR, 
        pos = 4)

    axis(2)
    box(col=x@params$colors$fg.col)
} # }}}

