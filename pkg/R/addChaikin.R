# Chaikin Functions
# chaikinAD and chaikinVolatility by Josh Ulrich from TTR
#
# chartSeries implementation by Jeffrey A. Ryan 2008
#  
#   addChAD
#   addChVol

`addChAD` <-
function (..., on = NA, legend = "auto") 
{
    stopifnot("package:TTR" %in% search() || require("TTR", quietly = TRUE))
    lchob <- quantmod:::get.current.chob()
    x <- as.matrix(lchob@xdata)
    x <- chaikinAD(HLC = HLC(x), volume = Vo(x))
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
    legend.name <- gsub("^.*[(]", " Chaikin Acc/Dist (", deparse(match.call()), 
        extended = TRUE)
    gpars <- c(list(...), list(col = 11))[unique(names(c(list(col = 11), 
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

`addChVol` <-
function (n = 10, maType, ..., on = NA, legend = "auto") 
{
    stopifnot("package:TTR" %in% search() || require("TTR", quietly = TRUE))
    lchob <- quantmod:::get.current.chob()
    x <- as.matrix(lchob@xdata)
    x <- chaikinVolatility(HL = HLC(x)[,-3], n = n, maType = maType)
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
    legend.name <- gsub("^.*[(]", " Chaikin Volatility (", deparse(match.call()), 
        extended = TRUE)
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
        chartSeries.chob <- quantmod:::chartSeries.chob
        do.call("chartSeries.chob", list(lchob))
        invisible(chobTA)
    }
    else {
        return(chobTA)
    }
}
