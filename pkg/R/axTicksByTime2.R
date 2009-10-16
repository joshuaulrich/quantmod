axTicksByTime2 <-
function (x, ticks.on = "auto", k = 1, labels = TRUE, format.labels = TRUE, 
    ends = TRUE, gt = 2, lt = 25) 
{
    if (timeBased(x)) 
        x <- xts(rep(1, length(x)), x)
    #tick.opts <- c("years", "months", "days", "hours", 
    #    "minutes", "seconds")
    tick.opts <- c("years", "months", "weeks", "days")
    tick.k.opts <- c(1,1,1,1)
    if (ticks.on %in% tick.opts) {
        cl <- ticks.on[1]
        ck <- k
    }
    else {
        tick.opts <- paste(tick.opts, tick.k.opts)
        is <- structure(rep(0, length(tick.opts)), .Names = tick.opts)
        for (i in 1:length(tick.opts)) {
            y <- strsplit(tick.opts[i], " ")[[1]]
            ep <- endpoints(x, y[1], as.numeric(y[2]))
            if(i>1 && is[i-1] == length(ep)-1)
              break
            is[i] <- length(ep) - 1
            if (is[i] > lt)
                break
        }
        nms <- rev(names(is)[which(is > gt & is < lt)])[1]
        cl <- strsplit(nms, " ")[[1]][1]
        ck <- as.numeric(strsplit(nms, " ")[[1]][2])
    }
    if (is.na(cl) || is.na(ck) || is.null(cl)) {
        return(c(1,NROW(x)))
        #ep <- NULL
    }
    else ep <- endpoints(x, cl, ck)
    if (ends) 
        ep <- ep + c(rep(1, length(ep) - 1), 0)
    if (labels) {
        if (is.logical(format.labels) || is.character(format.labels)) {
            unix <- ifelse(.Platform$OS.type == "unix", TRUE, 
                FALSE)
            #time.scale <- periodicity(x)$scale
            #fmt <- ifelse(unix, "%n%b%n%Y", "%b %Y")
            fmt <- switch(cl,
                          "years"="%Y",
                          "months"="%b",
                          "days"="%d",
                          "weeks"="W%W",
                          "hours"="%H:%M",
                          "minutes"="%H:%M:%S",
                          "seconds"="%H:%M:%S")
            if(ndays(x) > 1 && cl %in% c("hours","minutes","seconds")) {
              fmt <- paste("%b-%d",fmt)
            }
            names(ep) <- format(index(x)[ep], fmt)
        }
        else names(ep) <- as.character(index(x)[ep])
    }
    ep
}

