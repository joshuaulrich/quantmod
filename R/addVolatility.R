# volatility from TTR by Josh Ulrich
#
# chartSeries interface by Jeffrey A. Ryan 2008
#
#  addVolatility

`addVolatility` <-
function (n = 10, calc = "close", N = 260, ..., on = NA, legend = "auto") 
{
    lchob <- get.current.chob()
    x <- as.matrix(lchob@xdata)
    x <- OHLC(x)
    x <- volatility(OHLC = x, n = n, calc = calc, N = N)
    yrange <- NULL
    chobTA <- new("chobTA")
    if (NCOL(x) == 1) {
        chobTA@TA.values <- x[lchob@xsubset]
    }
    else chobTA@TA.values <- x[lchob@xsubset, ]
    chobTA@name <- "chartTA"
    if (any(is.na(on))) {
        chobTA@new <- TRUE
    }
    else {
        chobTA@new <- FALSE
        chobTA@on <- on
    }
    chobTA@call <- match.call()
    legend.name <- gsub("^add", "", deparse(match.call()))
    gpars <- c(list(...), list(col = 8))[unique(names(c(list(col = 8), 
        list(...))))]
    chobTA@params <- list(xrange = lchob@xrange, yrange = yrange, 
        colors = lchob@colors, color.vol = lchob@color.vol, multi.col = lchob@multi.col, 
        spacing = lchob@spacing, width = lchob@width, bp = lchob@bp, 
        x.labels = lchob@x.labels, time.scale = lchob@time.scale, 
        isLogical = is.logical(x), legend = legend, legend.name = legend.name, 
        pars = list(gpars))
    if (is.null(sys.call(-1))) {
        TA <- lchob@passed.args$TA
        lchob@passed.args$TA <- c(TA, chobTA)
        lchob@windows <- lchob@windows + ifelse(chobTA@new, 1, 
            0)
        do.call("chartSeries.chob", list(lchob))
        invisible(chobTA)
    }
    else {
        return(chobTA)
    }
}
