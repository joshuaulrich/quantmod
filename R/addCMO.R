
# addCMO {{{
`addCMO` <- function(n=14) {


  lchob <- get.current.chob()

  x <- as.matrix(lchob@xdata)

  chobTA <- new("chobTA")
  chobTA@new <- TRUE

  #  needs to accept any arguments for x, not just close

  xx <- if(has.Cl(x)) {
    Cl(x)
  } else if(is.null(dim(x))) {
    x
  } else {
    x[,1] 
  }

  cmo <- CMO(xx,n=n)

  chobTA@TA.values <- cmo[lchob@xsubset]
  chobTA@name <- "chartCMO"
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
# chartCMO {{{
`chartCMO` <-
function(x) {
    spacing <- x@params$spacing
    width <- x@params$width

    x.range <- x@params$xrange
    x.range <- seq(x.range[1],x.range[2]*spacing)

    multi.col <- x@params$multi.col
    color.vol <- x@params$color.vol

    n <- x@params$n
    cmo <- x@TA.values

    y.range <- seq(-max(abs(cmo), na.rm = TRUE), max(abs(cmo), 
                   na.rm = TRUE), length.out = length(x.range)) * 1.05

    plot(x.range,y.range,
         type='n',axes=FALSE,ann=FALSE)
    coords <- par('usr')
    rect(coords[1],coords[3],coords[2],coords[4],col=x@params$colors$area)
    grid(NA,NULL,col=x@params$colors$grid.col)

    COLOR="#0033CC"

    abline(h=0,col="#666666",lwd=1,lty='dotted')
    lines(seq(1,length(x.range),by=spacing),cmo,col=COLOR,lwd=1,type='l')

    text(0, last(y.range)*.9,
         paste("Chande Momentum Oscillator (", x@params$n,"):", sep = ""), 
        pos = 4)

    text(0, last(y.range)*.9,
        paste("\n\n\n",sprintf("%.3f",last(cmo)), sep = ""), col = COLOR, 
        pos = 4)

    axis(2)
    box(col=x@params$colors$fg.col)
} # }}}

