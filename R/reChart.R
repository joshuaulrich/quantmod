`reChart` <-
function (type = c("auto", "candlesticks", "matchsticks", 
    "bars", "line"), subset = NULL, show.grid = TRUE, name = NULL, 
    time.scale = NULL, line.type = "l", bar.type = "ohlc", 
    theme = chartTheme("black"),
    major.ticks = "auto", minor.ticks = TRUE, 
    yrange=NULL,
    up.col, dn.col, color.vol = TRUE, multi.col = FALSE, ...) 
{
  # refactor xts:::chart.lines to make subset functionality work
  chart.lines <- function (x, type = "l", lty = 1, lwd = 2, lend = 1, col = 1:10, 
                           up.col = NULL, dn.col = NULL, legend.loc = NULL, ...) 
  {
    if (is.null(up.col)) 
      up.col <- "green"
    if (is.null(dn.col)) 
      dn.col <- "red"
    xx <- xts:::current.xts_chob()
    switch(type, h = {
      colors <- ifelse(x[, 1] < 0, dn.col, up.col)
      lines(xx$Env$xycoords$x[match(index(x), index(xx$Env$xdata))], x[, 1], lwd = 2, col = colors, 
            lend = lend, lty = 1, type = "h", ...)
    }, p = , l = , b = , c = , o = , s = , S = , n = {
      if (length(lty) < NCOL(x)) lty <- rep(lty, length.out = NCOL(x))
      if (length(lwd) < NCOL(x)) lwd <- rep(lwd, length.out = NCOL(x))
      if (length(col) < NCOL(x)) col <- rep(col, length.out = NCOL(x))
      for (i in NCOL(x):1) {
        lines(xx$Env$xycoords$x[match(index(x), index(xx$Env$xdata))], x[, i], type = type, lend = lend, 
              col = col[i], lty = lty[i], lwd = lwd[i], ...)
      }
    }, {
      warning(paste(type, "not recognized. Type must be one of\n                         'p', 'l', 'b, 'c', 'o', 'h', 's', 'S', 'n'.\n                         plot.xts supports the same types as plot.default,\n                         see ?plot for valid arguments for type"))
    })
    if (!is.null(legend.loc)) {
      lc <- legend.coords(legend.loc, xx$Env$xlim, range(x, 
                                                         na.rm = TRUE))
      legend(x = lc$x, y = lc$y, legend = colnames(x), xjust = lc$xjust, 
             yjust = lc$yjust, fill = col[1:NCOL(x)], bty = "n")
    }
  }
  
  
  chob <- current.chob()
  chob$Env$chart.lines <- chart.lines
  #sys.TZ <- Sys.getenv('TZ')
  #Sys.setenv(TZ='GMT')
  #on.exit(Sys.setenv(TZ=sys.TZ))

  x <- chob$Env$xdata
  ########### name ###########
  if(!missing(name)) chob$Env$main <- name
  ########### end name ###########

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
      chob$Env$xsubset <- xsubset
      x <- x[xsubset,]
      xlim <- c(1, NROW(x))
      chob$set_xlim(c(xlim[1]-xlim[2]*0.04,xlim[2]+xlim[2]*0.04))
      if (is.OHLC(x)) {
        chob$Env$ylim[[2]] <- structure(c(min(Lo(x), na.rm = TRUE), max(Hi(x), 
            na.rm = TRUE)), fixed = TRUE)
      }   
      else chob$Env$ylim[[2]] <- structure(range(x[, 1], na.rm = TRUE), fixed = TRUE)
      if(!is.null(yrange) && length(yrange)==2) chob$Env$ylim[[2]] <- structure(yrange, fixed = TRUE)
    }

    chob$Env$xsubset <- xsubset
    if(missing(major.ticks)) {
      majorticks <- chob$Env$major.ticks
    } else majorticks <- major.ticks
    chob$Env$bp <- axTicksByTime(x,majorticks)
#    chob$Env$x.labels <- names(chob$Env$bp)
    chob$Env$length <- NROW(x)
  }
  xsubset <- chob$Env$xsubset
  ########### end subset ##########
  
  ########### type ###########
  #  if(!missing(type)) {
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
  #    chob@spacing <- spacing
  chob$Env$theme$width <- width
  chob$Env$range.bars.type <- chart[1]
  #  }
  ########### end type ###########

  if(!missing(major.ticks)) {
    chob$Env$bp <- axTicksByTime(chob$Env$xdata[chob$Env$xsubset],major.ticks)
#    chob@x.labels <- names(chob@bp)
    chob$Env$major.ticks <- major.ticks
  }
  if(!missing(minor.ticks))
    chob$Env$minor.ticks <- minor.ticks
  ########### chartTheme ##########
  if(!missing(theme)) {
    if(inherits(theme,'chart.theme')) {
      theme <- theme
    } else theme <- chartTheme(theme)
    
    chob$Env$theme$bg <- theme$bg.col
    chob$Env$theme$fg <- theme$fg.col
    chob$Env$theme$labels <- theme$major.tick
    # deprecated arguments(?
    chob$Env$theme$border <- theme$border
    #chob$Env$theme$minor.tick
    #chob$Env$theme$main.color
    #chob$Env$theme$sub.col
    chob$Env$theme$fill <- theme$area
    
    chob$Env$color.vol <- color.vol
    chob$Env$multi.col <- multi.col
    chob$Env$show.vol <- show.vol
    chob$Env$bar.type <- bar.type
    chob$Env$line.type <- line.type
    #chob$Env$theme$spacing <- spacing
    chob$Env$theme$Expiry <- theme$Expiry
    chob$Env$theme$width <- width
    chob$Env$layout <- layout
    chob$Env$minor.ticks <- minor.ticks
    chob$Env$major.ticks <- major.ticks
    if(!show.grid){
      chob$Env$theme$grid <- NULL
      chob$Env$theme$grid2 <- NULL
    } else {
      chob$Env$theme$grid <- theme$grid.col
      chob$Env$theme$grid2 <- theme$grid.col
    }
    
    chob$Env$theme$BBands$col$fill <- theme$BBands$col$fill
    chob$Env$theme$BBands$col$upper <- theme$BBands$col$upper
    chob$Env$theme$BBands$col$lower <- theme$BBands$col$lower
    chob$Env$theme$BBands$col$ma <- theme$BBands$col$ma
  }
  ########### end chartTheme ##########

  ########### multi.col ##########
  if(missing(theme) & !missing(multi.col) ) 
    stop(paste(sQuote('theme'),'must be specified in conjunction with',
         sQuote('multi.col')))
  theme <- chob$Env$theme
  if(missing(multi.col)) multi.col <- chob$Env$multi.col

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
    if(!is.null(chob$Env$vo)) {
      Volumes <- chob$Env$vo[xsubset]
      show.vol <- TRUE
    } else show.vol <- FALSE
  
    if(missing(time.scale)) {
      time.scale <- chob$Env$time.scale
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
  # set bar color
  chob$Env$theme$dn.up.col <- theme$dn.up.col
  chob$Env$theme$up.up.col <- theme$up.up.col
  chob$Env$theme$up.dn.col <- theme$up.dn.col
  chob$Env$theme$dn.dn.col <- theme$dn.dn.col
  
  # set border color
  chob$Env$theme$dn.up.border <- theme$dn.up.border
  chob$Env$theme$up.up.border <- theme$up.up.border
  chob$Env$theme$up.dn.border <- theme$up.dn.border
  chob$Env$theme$dn.dn.border <- theme$dn.dn.border
  
  chob$Env$multi.col <- multi.col
  chob$Env$color.vol <- color.vol
  ########### end multi.col ##########

  chob
}
