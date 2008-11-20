# ZigZag from TTR by Josh Ulrich
#
# chartSeries interface by Jeffrey A. Ryan 2008
#
#   addZigZag

`addZigZag` <-
function (change = 10, percent = TRUE, retrace = FALSE, lastExtreme = TRUE, 
    ..., on = -1, legend = "auto") 
{
    lchob <- quantmod:::get.current.chob()
    x <- as.matrix(lchob@xdata)
    x <- cbind(Hi(x),Lo(x))
    x <- ZigZag(HL = x, change = change, percent = percent, retrace = retrace, 
        lastExtreme = lastExtreme)
    yrange <- NULL
    chobTA <- new("chobTA")
    if (NCOL(x) == 1) {
        chobTA@TA.values <- x[lchob@xsubset]
    }
    else chobTA@TA.values <- x[lchob@xsubset, ]
    chobTA@name <- "chartTA"
    if (is.na(on)) {
        chobTA@new <- TRUE
    }
    else {
        chobTA@new <- FALSE
        chobTA@on <- on
    }
    chobTA@call <- match.call()
    legend.name <- gsub("^add", "", deparse(match.call()))
    gpars <- c(list(...), list(col = 4, lwd = 3))[unique(names(c(list(col = 4, 
        lwd = 3), list(...))))]
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
        chartSeries.chob <- quantmod:::chartSeries.chob
        do.call("chartSeries.chob", list(lchob))
        invisible(chobTA)
    }
    else {
        return(chobTA)
    }
}
