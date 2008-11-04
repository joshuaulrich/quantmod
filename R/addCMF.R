
# addCMF {{{
`addCMF` <- function(n=20) {

  stopifnot("package:TTR" %in% search() || require("TTR",quietly=TRUE))

  lchob <- get.current.chob()

  x <- as.matrix(lchob@xdata)

  chobTA <- new("chobTA")
  chobTA@new <- TRUE

  xx <- if(is.OHLC(x)) {
    cbind(Hi(x),Lo(x),Cl(x))
  } else stop("CMF only applicaple to HLC series")

  cmf <- CMF(xx,Vo(x),n=n)

  chobTA@TA.values <- cmf[lchob@xsubset]
  chobTA@name <- "chartCMF"
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
# chartCMF {{{
`chartCMF` <-
function(x) {
    spacing <- x@params$spacing
    width <- x@params$width

    x.range <- x@params$xrange
    x.range <- seq(x.range[1],x.range[2]*spacing)

    multi.col <- x@params$multi.col
    color.vol <- x@params$color.vol

    n <- x@params$n
    cmf <- x@TA.values

    y.range <- seq(-max(abs(cmf), na.rm = TRUE), max(abs(cmf), 
                   na.rm = TRUE), length.out = length(x.range)) * 1.05

    plot(x.range,y.range,
         type='n',axes=FALSE,ann=FALSE)
    coords <- par('usr')
    rect(coords[1],coords[3],coords[2],coords[4],col=x@params$colors$area)
    grid(NA,NULL,col=x@params$colors$grid.col)

    xx <- seq(1,length(x.range),by=spacing)
    cmf.positive <- ifelse(cmf >= 0,cmf,0)
    cmf.negative <- ifelse(cmf <  0,cmf,0)

    polygon(c(xx,rev(xx)),c(cmf.positive,rep(0,length(cmf))),col=x@params$colors$up.col)
    polygon(c(xx,rev(xx)),c(cmf.negative,rep(0,length(cmf))),col=x@params$colors$dn.col)

    abline(h=0,col="#999999")

    text(0, last(y.range)*.9,
         paste("Chaikin Money Flow (", x@params$n,"):", sep = ""), 
        pos = 4)

    text(0, last(y.range)*.9,
        paste("\n\n\n",sprintf("%.3f",last(cmf)), sep = ""), 
        col = ifelse(last(cmf) > 0,x@params$colors$up.col,x@params$colors$dn.col), 
        pos = 4)

    axis(2)
    box(col=x@params$colors$fg.col)
} # }}}

