`zoomChart` <- 
function(subset, yrange=NULL) {
  if(missing(subset) || is.null(subset)) #{
    subset <- '::'
#  } else {
#      if (!is.character(subset))                        
#          subset <- deparse(match.call()$subset)             
#      subset <- gsub("::", "/", subset, perl = TRUE)         
#      subset <- gsub("[-:]", "", subset, perl = TRUE)        
#      subset <- gsub("[ ]", "", subset, perl = TRUE)  
#  }
  reChart(subset=subset, yrange=yrange)
}

`zooom` <-
function (n = 1, eps = 2) 
{
    for (i in 1:n) {
        cat("select left and right extremes by clicking the chart\n")
        points <- locator(2)
        if (abs(diff(points$x)) < eps) {
            zoomChart()
        }
        else {
            usr <- par("usr")
            xdata <- get.chob()[[2]]@xdata
            xsubset <- get.chob()[[2]]@xsubset
            sq <- floor(seq(usr[1], usr[2], 1))
            st <- which(floor(points$x[1]) == sq)/length(sq) * 
                NROW(xdata[xsubset])
            en <- which(floor(points$x[2]) == sq)/length(sq) * 
                NROW(xdata[xsubset])
            sorted <- sort(c(st, en))
            st <- sorted[1]
            en <- sorted[2] * 1.05
            zoomChart(paste(index(xdata[xsubset])[max(1, floor(st), 
                na.rm = TRUE)], index(xdata[xsubset])[min(ceiling(en), 
                NROW(xdata[xsubset]), na.rm = TRUE)], sep = "::"))
        }
    }
    cat("done\n")
}
