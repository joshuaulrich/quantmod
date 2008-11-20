# aroon from TTR
# 
# chartSeries interface by Jeffrey A. Ryan 2008
#
#  addAroon
#  addAroonOsc

`addAroon` <-
function (n = 20, ..., on = NA, legend = "auto") 
{
    stopifnot("package:TTR" %in% search() || require("TTR", quietly = TRUE))
    lchob <- quantmod:::get.current.chob()
    x <- as.matrix(lchob@xdata)
    x <- cbind(Hi(x),Lo(x))
    x <- aroon(HL = x, n = n)[,-3]
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
    gpars <- c(list(...), list(col = 3:4))[unique(names(c(list(col = 3:4), 
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
        chartSeries.chob <- quantmod:::chartSeries.chob
        do.call("chartSeries.chob", list(lchob))
        invisible(chobTA)
    }
    else {
        return(chobTA)
    }
}

`addAroonOsc` <-
function (n = 20, ..., on = NA, legend = "auto") 
{
    stopifnot("package:TTR" %in% search() || require("TTR", quietly = TRUE))
    lchob <- quantmod:::get.current.chob()
    x <- as.matrix(lchob@xdata)
    x <- cbind(Hi(x),Lo(x))
    x <- aroon(HL = x, n = n)[,3]
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
    legend.name <- gsub("^addAroonOsc", "Aroon Oscillator ", deparse(match.call()))
    gpars <- c(list(...), list(col = 3:4))[unique(names(c(list(col = 3:4), 
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
        chartSeries.chob <- quantmod:::chartSeries.chob
        do.call("chartSeries.chob", list(lchob))
        invisible(chobTA)
    }
    else {
        return(chobTA)
    }
}

