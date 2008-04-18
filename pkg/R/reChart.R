`reChart` <-
function (type = c("auto", "candlesticks", "matchsticks", 
    "bars", "line"), subset = NULL, show.grid = TRUE, name = NULL, 
    time.scale = NULL, line.type = "l", bar.type = "ohlc", 
    theme = chartTheme("black"), major.ticks = "auto", minor.ticks = TRUE, 
    up.col, dn.col, color.vol = TRUE, multi.col = FALSE, ...) 
{
  chob <- quantmod:::get.current.chob()

  sys.TZ <- Sys.getenv('TZ')
  Sys.setenv(TZ='GMT')
  on.exit(Sys.setenv(TZ=sys.TZ))

  x <- chob@xdata
  ########### name ###########
  if(!missing(name)) chob@name <- name
  ########### end name ###########

  ########### type ###########
  if(!missing(type)) {
    chart.options <- c("auto","candlesticks","matchsticks","line","bars")
    chart <- chart.options[pmatch(type,chart.options)]
    if(chart[1]=="auto") {
      chart <- ifelse(NROW(x) > 300,"matchsticks","candlesticks")
    }
    if(chart[1]=="candlesticks") {
      spacing <- 3
      width <- 3
    } else
    if(chart[1]=="matchsticks" || chart[1]=='line') {
      spacing <- 1
      width <- 1
    } else
    if(chart[1]=="bars") {
      spacing <- 4
      width <- 3
      if(NROW(x) > 60) width <- 1
    }
    chob@spacing <- spacing
    chob@width <- width
    chob@type <- chart[1]
  }
  ########### end type ###########

  ########### subset ##########
  if(!missing(subset)) {
    if (!is.null(subset) & is.character(subset)) {
        if (strsplit(subset, " ")[[1]][1] %in% c("first", "last")) {
            subsetvec <- strsplit(subset, " ")[[1]]
            if (length(subsetvec) < 3) {
                subset.n <- ifelse(length(subsetvec) == 1, 1L, 
                  as.numeric(subsetvec[2]))
            }   
            else {
                subset.n <- paste(subsetvec[2:3], collapse = " ")
            }   
            sub.index <- index(do.call(subsetvec[1], list(x, 
                subset.n)))
            xsubset <- which(index(x) %in% sub.index)
        }   
        else xsubset <- which(index(x) %in% index(x[subset]))
    }
    else xsubset <- 1:NROW(x)
  
    if(!is.null(subset)) {
      chob@xsubset <- xsubset
      x <- x[xsubset,]
      chob@xrange <- c(1, NROW(x))
      if (is.OHLC(x)) {
        chob@yrange <- c(min(Lo(x), na.rm = TRUE), max(Hi(x), 
            na.rm = TRUE))
      }   
      else chob@yrange <- range(x[, 1], na.rm = TRUE)
    }

    chob@xsubset <- xsubset
    chob@bp <- axTicksByTime(x,'auto')
    chob@x.labels <- names(chob@bp)
    chob@length <- NROW(x)
  }
  ########### end subset ##########

  
  ########### chartTheme ##########
  if(!missing(theme)) {
    if(inherits(theme,'chart.theme')) {
      chob@colors <- theme
    } else chob@colors <- chartTheme(theme)
  }
  ########### end chartTheme ##########

  ########### multi.col ##########
  if(missing(theme) & !missing(multi.col) ) 
    stop(paste(sQuote('theme'),'must be specified in conjunction with',
         sQuote('multi.col')))
  theme <- chob@colors
  if(missing(multi.col)) multi.col <- chob@multi.col

    if(is.OHLC(x)) {
      Opens <- as.numeric(Op(x))
      Highs <- as.numeric(Hi(x))
      Lows <- as.numeric(Lo(x))
      Closes <- as.numeric(Cl(x))
    } else {
      Lows <- min(x[,1])
      Highs <- max(x[,1])
      Closes <- as.numeric(x[,1])
      type <- "line"
      color.vol <- FALSE
    }
    if(has.Vo(x)) {
      Volumes <- as.numeric(Vo(x))
      show.vol <- TRUE
    } else show.vol <- FALSE
  
    if(missing(time.scale)) {
      time.scale <- chob@time.scale
    }
  
    if(!missing(up.col)) theme$up.col <- up.col
    if(!missing(dn.col)) theme$dn.col <- dn.col
    if(!multi.col) { # interpret as FALSE
      theme$dn.up.col <- theme$up.col
      theme$up.up.col <- theme$up.col
      theme$dn.dn.col <- theme$dn.col
      theme$up.dn.col <- theme$dn.col
    } else {
      if(is.character(multi.col)) {
        # add some check for length 4 colors
        theme$dn.up.col <- multi.col[1]
        theme$up.up.col <- multi.col[2]
        theme$dn.dn.col <- multi.col[3]
        theme$up.dn.col <- multi.col[4]
      }
      theme$up.col <- theme$up.up.col
      theme$dn.col <- theme$dn.dn.col
      multi.col <- TRUE
    }
  chob@colors <- theme
  chob@multi.col <- multi.col
  chob@color.vol <- color.vol
  ########### end multi.col ##########

  chob@passed.args$TA <- sapply(chob@passed.args$TA, 
    function(x) eval(x@call)
  )   

  quantmod:::chartSeries.chob(chob)

  chob@device <- as.numeric(dev.cur())

  write.chob(chob,chob@device)
  invisible(chob)

}
