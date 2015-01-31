# Know Sure Thing from TTR by Josh Ulrich
#
# chartSeries interface by Jeffrey A. Ryan 2008
#
#   addKST
#

`addKST` <-
function (n = c(10, 10, 10, 15), nROC = c(10, 15, 20, 30), nSig = 9, 
    maType, wts = 1:NROW(n), ..., on = NA, legend = "auto") 
{
    lchob <- get.current.chob()
    x <- as.matrix(lchob@xdata)
    x <- coredata(Cl(x))
    x <- KST(price = x, n = n, nROC = nROC, nSig = nSig, maType = maType, 
        wts = wts)
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
    legend.name <- gsub("^addKST", "Know Sure Thing ", deparse(match.call()))
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
        chartSeries.chob <- chartSeries.chob
        do.call("chartSeries.chob", list(lchob))
        invisible(chobTA)
    }
    else {
        return(chobTA)
    }
}

