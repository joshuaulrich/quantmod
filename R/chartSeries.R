# chartSeries0 {{{
`chartSeries0` <-
function(x,
         type=c("auto","candlesticks","matchsticks","bars","line"),
         subset=NULL,
         show.grid=TRUE,name=NULL,
         time.scale=NULL,
         TA=c(addVo()),
         line.type="l",
         bar.type="ohlc",
         theme=chartTheme("black"),
         major.ticks='auto',minor.ticks=TRUE,
         up.col,dn.col,color.vol=TRUE,multi.col=FALSE,...
         ) {
  sys.TZ <- Sys.getenv('TZ')
  Sys.setenv(TZ='GMT')
  on.exit(Sys.setenv(TZ=sys.TZ))

  if(!is.xts(x)) x <- as.xts(x)

  indexClass(x) <- "POSIXct"

  if(!is.null(subset) & is.character(subset)) {
    if(strsplit(subset,' ')[[1]][1] %in% c('first','last')) {
      subsetvec <- strsplit(subset,' ')[[1]]
      if(length(subsetvec) < 3) {
        subset.n <- ifelse(length(subsetvec)==1,1L,as.numeric(subsetvec[2]))
      } else {
        subset.n <- paste(subsetvec[2:3],collapse=' ')
      }
      sub.index <- index(do.call(subsetvec[1],list(x,subset.n)))
      xsubset <- which(index(x) %in% sub.index)
    } else xsubset <- which(index(x) %in% index(x[subset]))  
  } else xsubset <- 1:NROW(x)

  xdata <- x
  x <- x[xsubset]

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
  
  if(is.null(time.scale)) {
    time.scale <- periodicity(x)$scale
  }

  if(is.character(theme)) theme <- chartTheme(theme)
  if(!missing(up.col)) theme$up.col <- up.col 
  if(!missing(dn.col)) theme$dn.col <- dn.col 
  if(missing(multi.col) | !multi.col) { # interpret as FALSE
    multi.col <- FALSE
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

  # spacing requirements for chart type
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
  ep <- axTicksByTime(x, major.ticks)
  ep <- c(rev(rev(ep)[-1]),rev(ep)[1]-1)

  x.labels <- format(index(x)[ep + 1], "%n%b%n%Y")
  if (time.scale == "weekly" | time.scale == "daily") 
      x.labels <- format(index(x)[ep + 1], "%b %d%n%Y")
  if (time.scale == "minute" | time.scale == "hourly") 
      x.labels <- format(index(x)[ep + 1], "%b %d%n%H:%M")

  chob <- new("chob")
  chob@call <- match.call(expand.dots=TRUE)
  if(is.null(name)) name <- as.character(match.call()$x)

  chob@xdata <- xdata
  chob@xsubset <- xsubset
  chob@name <- name
  chob@type <- chart[1]

  chob@xrange <- c(1,NROW(x))
  if(is.OHLC(x)) {
    chob@yrange <- c(min(Lo(x),na.rm=TRUE),max(Hi(x),na.rm=TRUE))
  } else chob@yrange <- range(x[,1],na.rm=TRUE)
  

  chob@color.vol <- color.vol
  chob@multi.col <- multi.col
  chob@show.vol <- show.vol
  chob@bar.type <- bar.type
  chob@line.type <- line.type
  chob@spacing <- spacing
  chob@width <- width
  chob@bp <- ep
  chob@x.labels <- x.labels
  chob@colors <- theme
  chob@time.scale <- time.scale
  chob@minor.ticks <- minor.ticks

  chob@length <- NROW(x)

  chob@passed.args <- as.list(match.call(expand.dots=TRUE)[-1])
  if(!is.null(TA)) {

    # important to force eval of _current_ chob, not saved chob
    thisEnv <- environment()
    if(is.character(TA)) TA <- as.list(TA)
    chob@passed.args$TA <- list()
    for(ta in 1:length(TA)) {
      if(is.character(TA[[ta]])) {
        chob@passed.args$TA[[ta]] <- eval(parse(text=TA[[ta]]),envir=thisEnv)
      } else chob@passed.args$TA[[ta]] <- eval(TA[[ta]],envir=thisEnv)
    }
    chob@windows <- length(which(sapply(chob@passed.args$TA,function(x) x@new)))+1
    chob@passed.args$show.vol <- any(sapply(chob@passed.args$TA,function(x) x@name=="chartVo"))
  } else chob@windows <- 1
  
  #if(debug) return(str(chob))
  # re-evaluate the TA list, as it will be using stale data,
  chob@passed.args$TA <- sapply(chob@passed.args$TA, function(x) { eval(x@call) } )

  # draw the chart
  do.call('chartSeries.chob',list(chob))

  chob@device <- as.numeric(dev.cur())

  write.chob(chob,chob@device)
  invisible(chob)
} #}}}

# candleChart {{{
`candleChart` <-
function(x,
         subset = NULL,
         type="candlesticks",
         show.grid=TRUE,name=deparse(substitute(x)),
         time.scale=NULL,log.scale=FALSE,
         TA="addVo()",
         theme=chartTheme("black"),
         major.ticks='auto', minor.ticks = TRUE,
         color.vol=TRUE,multi.col=FALSE,...
         ) {
  do.call('chartSeries',list(x,subset=subset,
                             name=name,type='candlesticks',show.grid=show.grid,
                             time.scale=time.scale,log.scale=log.scale,TA=substitute(TA),
                             theme=theme,major.ticks=major.ticks,minor.ticks=minor.ticks,
                             color.vol=color.vol,
                             multi.col=multi.col,...))
} #}}}
# matchChart {{{
`matchChart` <-
function(x,
         subset = NULL,
         type="matchsticks",
         show.grid=TRUE,name=deparse(substitute(x)),
         time.scale=NULL,log.scale=FALSE,
         TA="addVo()",
         theme=chartTheme("black"),
         major.ticks='auto', minor.ticks = TRUE,
         color.vol=TRUE,multi.col=FALSE,...
         ) {
  do.call('chartSeries',list(x,subset=subset,
                             name=name,type='matchsticks',show.grid=show.grid,
                             time.scale=time.scale,log.scale=log.scale,TA=substitute(TA),
                             theme=theme,major.ticks=major.ticks,minor.ticks=minor.ticks,
                             color.vol=color.vol,
                             multi.col=multi.col,...))
} #}}}
# barChart {{{
`barChart` <-
function(x,
         subset = NULL,
         type="bars",
         show.grid=TRUE,name=deparse(substitute(x)),
         time.scale=NULL,log.scale=FALSE,
         TA="addVo()",
         bar.type="ohlc",
         theme=chartTheme("black"),
         major.ticks='auto', minor.ticks = TRUE,
         color.vol=TRUE,multi.col=FALSE,...
         ) {
  do.call('chartSeries',list(x,subset=subset,
                             name=name,type='bars',show.grid=show.grid,
                             time.scale=time.scale,log.scale=log.scale,TA=substitute(TA),bar.type=bar.type,
                             theme=theme,major.ticks=major.ticks,minor.ticks=minor.ticks,
                             color.vol=color.vol,
                             multi.col=multi.col,...))
} #}}}
# lineChart {{{
`lineChart` <-
function(x,subset = NULL,
         type="line",
         show.grid=TRUE,name=deparse(substitute(x)),
         time.scale=NULL,log.scale=FALSE,
         TA="addVo()",
         line.type="l",
         theme=chartTheme("black"),
         major.ticks='auto', minor.ticks = TRUE,
         color.vol=TRUE,multi.col=FALSE,...
         ) {
  do.call('chartSeries',list(x,subset=subset,
                             name=name,type='line',show.grid=show.grid,
                             time.scale=time.scale,log.scale=log.scale,TA=substitute(TA),line.type=line.type,
                             theme=theme,major.ticks=major.ticks,minor.ticks=minor.ticks,
                             color.vol=color.vol,
                             multi.col=multi.col,...))
} #}}}

# .chart.theme {{{
`.chart.theme` <- structure(list(
                      'white'=
                           list(fg.col="#000000",bg.col="#F0F0F0",
                                grid.col="#CCCCCC",border="#444444",
                                minor.tick="#888888",major.tick="#000000",
                                up.col="#00CC00",dn.col="#FF7700",
                                dn.up.col="#888888",up.up.col="#FFFFFF",
                                dn.dn.col="#FF0000",up.dn.col="#000000",
                                up.border="#444444",dn.border="#444444",
                                dn.up.border="#444444",up.up.border="#444444",
                                dn.dn.border="#444444",up.dn.border="#444444",
                                main.col="#555555",sub.col="#555555",
                                area="#FFFFFF",
                                fill="#F7F7F7",
                                Expiry='#C9C9C9',
                                theme.name='white'
                                ),
                      'white.mono'=
                           list(fg.col="#666666",bg.col="#FFFFFF",
                                grid.col="#CCCCCC",border="#666666",
                                minor.tick="#CCCCCC",major.tick="#888888",
                                up.col="#000000",dn.col="#000000",
                                dn.up.col="#888888",up.up.col="#FFFFFF",
                                dn.dn.col="#4D4D4D",up.dn.col="#000000",
                                up.border="#666666",dn.border="#666666",
                                dn.up.border="#666666",up.up.border="#666666",
                                dn.dn.border="#666666",up.dn.border="#666666",
                                main.col="#555555",sub.col="#555555",
                                fill="#F7F7F7",
                                Expiry='#C9C9C9',
                                BBands.col='#666666',BBands.fill="#F7F7F7",
                                BBands=list(col=list(upper='#666666',
                                                     lower='#666666',
                                                     fill='#F7F7F7',
                                                     ma='#D5D5D5')),
                                theme.name='white.mono'
                                ),
                      'black'=
                           list(fg.col="#666666",bg.col="#222222",
                                grid.col="#303030",border="#666666",
                                minor.tick="#303030",major.tick="#AAAAAA",
                                up.col="#00FF00",dn.col="#FF9900",
                                dn.up.col="#888888",up.up.col="#FFFFFF",
                                dn.dn.col="#FF0000",up.dn.col="#000000",
                                up.border="#666666",dn.border="#666666",
                                dn.up.border="#666666",up.up.border="#666666",
                                dn.dn.border="#666666",up.dn.border="#666666",
                                main.col="#999999",sub.col="#999999",
                                area="#252525",
                                fill="#282828",
                                Expiry='#383838',
                                BBands.col='red',BBands.fill="#282828",
                                BBands=list(col=list(upper='red',
                                                     lower='red',
                                                     fill='#282828',
                                                     ma='#D5D5D5')),
                                theme.name='black'
                                ),
                      'black.mono'=
                           list(fg.col="#666666",bg.col="#222222",
                                grid.col="#303030",border="#666666",
                                minor.tick="#303030",major.tick="#AAAAAA",
                                up.col="#FFFFFF",dn.col="#FFFFFF",
                                dn.up.col="#888888",up.up.col="#FFFFFF",
                                dn.dn.col="#4D4D4D",up.dn.col="#000000",
                                up.border="#666666",dn.border="#666666",
                                dn.up.border="#666666",up.up.border="#666666",
                                dn.dn.border="#666666",up.dn.border="#666666",
                                main.col="#999999",sub.col="#999999",
                                fill="#777777",
                                Expiry='#383838',
                                BBands=list(col=list(upper='#DDDDDD',
                                                     lower='#DDDDDD',
                                                     fill='#777777',
                                                     ma='#D5D5D5')),
                                BBands.col='#DDDDDD',BBands.fill="#777777",
                                theme.name='black.mono'
                                ),
                      'beige'=
                           list(fg.col="#888888",bg.col="#F5F5D0",
                                grid.col="#CCCCCC",border="#666666",
                                minor.tick="#CCCCCC",major.tick="#AAAAAA",
                                up.col="#00FF00",dn.col="#AA0000",
                                dn.up.col="#888888",up.up.col="#FFFFFF",
                                dn.dn.col="#FF0000",up.dn.col="#000000",
                                up.border="#666666",dn.border="#666666",
                                dn.up.border="#666666",up.up.border="#666666",
                                dn.dn.border="#666666",up.dn.border="#666666",
                                main.col="#555555",sub.col="#555555",
                                fill="#F5F5F5",
                                Expiry='#C9C9C9',
                                BBands.col='orange',BBands.fill='#F5F5DF',
                                BBands=list(col=list(upper='orange',
                                                     lower='orange',
                                                     fill='#F5F5DF',
                                                     ma='#D5D5D5')),
                                theme.name='beige'
                                ),
                       'wsj'= 
                            list(fg.col="#000000",bg.col="#F0F0F0",
                                 grid.col="#ffffff",border="#444444",
                                 minor.tick="#888888",major.tick="#000000", 
                                 up.col="#FFFFFF",dn.col="#666666",
                                 dn.up.col="#888888",up.up.col="#FFFFFF",
                                 dn.dn.col="#FF0000",up.dn.col="#000000", 
                                 up.border="#444444",dn.border="#666666",
                                 dn.up.border="#444444",up.up.border="#444444",
                                 dn.dn.border="#444444",up.dn.border="#444444", 
                                 main.col = "#555555", sub.col = "#555555", area = "#d3d0af", 
                                 fill = "#F7F7F7", Expiry = "#C9C9C9", 
                                 theme.name = "wsj")
                     ), class='chart.theme')
# }}}

`print.chart.theme` <- function(x,...) {
  str(x)
}

# chartTheme {{{
`chartTheme` <- function(theme='black',...) {
  ctheme <- get(".chart.theme",as.environment("package:quantmod"))
  attr(ctheme,".Environment") <- NULL
  current.theme <- ctheme[[theme]]
  ll <- list(...)
  for(i in names(ll)) {
    current.theme[[i]] <- ll[[i]]
  }
  return(structure(current.theme,class='chart.theme'))
}#}}}

# chartSeries {{{
`chartSeries` <-
function(x,
         type=c("auto","candlesticks","matchsticks","bars","line"),
         subset=NULL,
         show.grid=TRUE,name=NULL,
         time.scale=NULL,log.scale=FALSE,
         TA='addVo()',TAsep=';',
         line.type="l",
         bar.type="ohlc",
         theme=chartTheme("black"),
         layout=NA,
         major.ticks='auto',minor.ticks=TRUE,
         yrange=NULL,
         plot=TRUE,
         up.col,dn.col,color.vol=TRUE,multi.col=FALSE,...
         ) {
  #sys.TZ <- Sys.getenv('TZ')
  #Sys.setenv(TZ='GMT')
  #on.exit(Sys.setenv(TZ=sys.TZ))

  #if(!is.xts(x)) x <- as.xts(x)
  x <- try.xts(x, error='chartSeries requires an xtsible object')

  x <- na.omit(x)

  indexClass(x) <- "POSIXct"

  if(!is.null(subset) & is.character(subset)) {
    if(strsplit(subset,' ')[[1]][1] %in% c('first','last')) {
      subsetvec <- strsplit(subset,' ')[[1]]
      if(length(subsetvec) < 3) {
        subset.n <- ifelse(length(subsetvec)==1,1L,as.numeric(subsetvec[2]))
      } else {
        subset.n <- paste(subsetvec[2:3],collapse=' ')
      }
      sub.index <- index(do.call(subsetvec[1],list(x,subset.n)))
      xsubset <- which(index(x) %in% sub.index)
    } else xsubset <- which(index(x) %in% index(x[subset]))  
  } else xsubset <- 1:NROW(x)

  xdata <- x
  x <- x[xsubset]

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
  
  if(is.null(time.scale)) {
    time.scale <- periodicity(x)$scale
  }

  if(is.character(theme)) theme <- chartTheme(theme)
  if(!missing(up.col)) theme$up.col <- up.col 
  if(!missing(dn.col)) theme$dn.col <- dn.col 
  if(missing(multi.col) | !multi.col) { # interpret as FALSE
    multi.col <- FALSE
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

  # spacing requirements for chart type
  chart.options <- c("auto","candlesticks","matchsticks","line","bars")
  chart <- chart.options[pmatch(type,chart.options)]
  if(chart[1]=="auto") {
    chart <- ifelse(NROW(x) > 300,"matchsticks","candlesticks")
  }
  if(chart[1]=="candlesticks") {
    #spacing <- 3
    width <- 3 
  } else
  if(chart[1]=="matchsticks" || chart[1]=='line') {
    #spacing <- 1
    width <- 1
  } else 
  if(chart[1]=="bars") {
    #spacing <- 4
    width <- 3
    if(NROW(x) > 60) width <- 1
  }

  if(is.null(name)) name <- as.character(match.call()$x)
  cs <- chart_Series(x = xdata, name = name, type = chart[1],
                     subset = xsubset, yaxis.left = FALSE, ...)
  # set xlim to reserve space
  xlim <- cs$get_xlim()
  cs$set_xlim(c(xlim[1]-xlim[2]*0.04,xlim[2]+xlim[2]*0.04))
  # remove x-axis grid line
  cs$Env$actions[[1]] <- NULL

  if(is.OHLC(x)) {
    cs$Env$ylim[[2]] <- structure(c(min(Lo(x),na.rm=TRUE),max(Hi(x),na.rm=TRUE)), fixed = TRUE)
  } else cs$Env$ylim[[2]] <- structure(range(x[,1],na.rm=TRUE), fixed = TRUE)
  
  if(!is.null(yrange) && length(yrange)==2)
    cs$Env$ylim[[2]] <- structure(yrange, fixed = TRUE)

  cs$Env$log.scale <- log.scale # special handling needed
  
  cs$Env$theme$up.col <- theme$up.col
  cs$Env$theme$dn.col <- theme$dn.col
  
  # set bar color
  cs$Env$theme$dn.up.col <- theme$dn.up.col
  cs$Env$theme$up.up.col <- theme$up.up.col
  cs$Env$theme$up.dn.col <- theme$up.dn.col
  cs$Env$theme$dn.dn.col <- theme$dn.dn.col
  
  # set border color
  cs$Env$theme$dn.up.border <- theme$dn.up.border
  cs$Env$theme$up.up.border <- theme$up.up.border
  cs$Env$theme$up.dn.border <- theme$up.dn.border
  cs$Env$theme$dn.dn.border <- theme$dn.dn.border

  cs$Env$theme$bg <- theme$bg.col
  cs$Env$theme$fg <- theme$fg.col
  cs$Env$theme$labels <- theme$major.tick
  # deprecated arguments(?
  cs$Env$theme$border <- theme$border
  #cs$Env$theme$minor.tick
  #cs$Env$theme$main.color
  #cs$Env$theme$sub.col
  cs$Env$theme$fill <- theme$area
  
  cs$Env$color.vol <- color.vol
  cs$Env$multi.col <- multi.col
  cs$Env$show.vol <- show.vol
  cs$Env$bar.type <- bar.type
  cs$Env$line.type <- line.type
  #cs$Env$theme$spacing <- spacing
  cs$Env$theme$Expiry <- theme$Expiry
  cs$Env$theme$width <- width
  cs$Env$layout <- layout
  cs$Env$time.scale <- time.scale
  cs$Env$minor.ticks <- minor.ticks
  cs$Env$major.ticks <- major.ticks
  if(!show.grid){
    cs$Env$theme$grid <- NULL
    cs$Env$theme$grid2 <- NULL
  } else {
    cs$Env$theme$grid <- theme$grid.col
    cs$Env$theme$grid2 <- theme$grid.col
  }

  cs$Env$length <- NROW(x)
  cs$Env$theme$BBands$col$fill <- theme$BBands$col$fill
  cs$Env$theme$BBands$col$upper <- theme$BBands$col$upper
  cs$Env$theme$BBands$col$lower <- theme$BBands$col$lower
  cs$Env$theme$BBands$col$ma <- theme$BBands$col$ma
  
  # allow custom settings to TAs color
  # use chartTheme() to enter
  which.TA <- grep("add", names(theme))
  names(theme)[which.TA] <- gsub("^add", "", names(theme)[which.TA])
  cs$Env$theme <- append(cs$Env$theme, theme[which.TA])
  
  
  # change minor ticks to be downward
  exp <- expression(if (NROW(xdata[xsubset]) < 400) { 
    axis(1, at = xycoords$x[1:NROW(xsubset)], labels = FALSE, col = theme$grid2, 
         col.axis = theme$grid2, tcl = -0.4)
  })
  exp <- structure(exp, frame = 1)
  exp <- structure(exp, clip = TRUE)
  exp <- structure(exp, env = cs$Env)
  cs$Env$actions[[1]] <- exp
  
  # add border
  exp.border <- expression(segments(xlim[1], y_grid_lines(get_ylim()[[2]]), xlim[2], 
                                    y_grid_lines(get_ylim()[[2]]), col = theme$grid, lwd = grid.ticks.lwd, 
                                    lty = grid.ticks.lty), text(xlim[2] + xstep * 2/3, y_grid_lines(get_ylim()[[2]]), 
                                                                noquote(format(y_grid_lines(get_ylim()[[2]]), justify = "right")), 
                                                                col = theme$labels, srt = theme$srt, offset = 0, pos = 4, 
                                                                cex = theme$cex.axis, xpd = TRUE),
                           rect(xlim[1], get_ylim()[[2]][1], xlim[2], get_ylim()[[2]][2],border=theme$labels))
  exp.border <- structure(exp.border, frame = 2)
  exp.border <- structure(exp.border, clip = TRUE)
  exp.border <- structure(exp.border, env = cs$Env)
  cs$Env$actions[[4]] <- exp.border

  # add inbox color
  exp.area <- expression(rect(xlim[1], get_ylim()[[2]][1], xlim[2], get_ylim()[[2]][2],col=theme$fill))
  cs$set_frame(-2)
  cs$add(exp.area, env=cs$Env, expr=TRUE)
  
  # add legend
  text.exp <- expression(
    Closes <- Cl(xdata[xsubset]),
    lc <- xts:::legend.coords("topleft", xlim, get_ylim()[[2]]),
    legend(x = lc$x, y = lc$y,
           legend = paste("Last", sprintf("%.3f", last(Closes))), 
           text.col = theme$up.col, 
           bty='n', 
           y.intersp=0.95))
  cs$set_frame(2)
  cs$add(text.exp, env=cs$Env, expr=TRUE)
  
  # handle TA="addVo()" as we would interactively FIXME: allow TA=NULL to work
  TA <- unlist(strsplit(TA, TAsep))
  if(!show.vol) {
    which.vo <- match("addVo()", TA)
    if(!is.na(which.vo)) TA <- TA[-which.vo]
  }
  if(!is.null(TA) && length(TA) > 0) {
    TA <- parse(text=TA, srcfile=NULL)
    for(ta in seq_along(TA)) {
      if(length(TA[ta][[1]][-1]) > 0) {
        cs <- eval(TA[ta])
      } else {
        cs <- eval(TA[ta])
      }
    }
  }
  # Pass chart.layout settings
  cs$Env$chart.layout <- chart.layout
  if(!inherits(layout, "chart.layout")) {
    cl <- chart.layout(length(cs$Env$ylim)-1)
  } else
    cl <- layout
  # since xts::plot.xts is applied, chartSeries should now be layout free
  # layout(cl$mat, cl$width, cl$height, respect=FALSE)
  cs$Env$mar <- cl$par.list[[3]]$mar
  
  assign(".xts_chob", cs, xts:::.plotxtsEnv)
  if(plot) # draw the chart
    cs
} #}}}
