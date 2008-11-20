# Arm's Ease of Movement Index by Josh Ulrich from TTR
#
# chartSeries implementation by Jeffrey A. Ryan 2008
#
#  addEMV

`addEMV` <-
function (volume, n = 9, maType, vol.divisor = 10000, ..., on = NA, 
    legend = "auto") 
{
    lchob <- quantmod:::get.current.chob()
    x <- as.matrix(lchob@xdata)
    x <- EMV(HL = HLC(x)[,-3], volume = Vo(x), n = n, maType = maType, 
        vol.divisor = vol.divisor)
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
    legend.name <- gsub("^.*[(]", " Ease of Movement (", deparse(match.call()), 
        extended = TRUE)
    gpars <- c(list(...), list(col = 6:7))[unique(names(c(list(col = 6:7), 
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
