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
  ep <- xts:::axTicksByTime0(x, major.ticks)
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
                                BBands=list(col='#666666',fill='#F7F7F7'),
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
                                BBands=list(col='red',fill='#282828'),
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
                                BBands=list(col='#DDDDDD',fill='#777777'),
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
                                BBands=list(col='orange',fill='#F5F5DF'),
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
  ep <- axTicksByTime(x,major.ticks)

  x.labels <- names(ep)

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
  
  if(!is.null(yrange) && length(yrange)==2)
    chob@yrange <- yrange

  chob@log.scale <- log.scale

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
  chob@layout <- layout
  chob@time.scale <- time.scale
  chob@minor.ticks <- minor.ticks
  chob@major.ticks <- major.ticks

  chob@length <- NROW(x)

  chob@passed.args <- as.list(match.call(expand.dots=TRUE)[-1])
  if(!is.null(TA)) {

    # important to force eval of _current_ chob, not saved chob
    thisEnv <- environment()
    if(is.character(TA)) TA <- as.list(strsplit(TA,TAsep)[[1]])
    #if(!has.Vo(x)) TA <- TA[-which(TA=='addVo()')] # remove addVo if no volume
    chob@passed.args$TA <- list()
    #if(length(TA) > 0) {
      for(ta in 1:length(TA)) {
        if(is.character(TA[[ta]])) {
          chob@passed.args$TA[[ta]] <- eval(parse(text=TA[[ta]]),envir=thisEnv)
        } else chob@passed.args$TA[[ta]] <- eval(TA[[ta]],envir=thisEnv)
      }
      # check if all args are indeed chobTA
      poss.new <- sapply(chob@passed.args$TA, function(x) 
                          {
                            if(isS4(x) && is(x, 'chobTA')) 
                              return(x@new) 
                            stop('improper TA argument/call in chartSeries', call.=FALSE)
                          } )
      if(length(poss.new) > 0)
        poss.new <- which(poss.new)
      chob@windows <- length(poss.new) + 1
      #chob@windows <- length(which(sapply(chob@passed.args$TA,
      #                           function(x) ifelse(is.null(x),FALSE,x@new))))+1
      chob@passed.args$show.vol <- any(sapply(chob@passed.args$TA,
                                     function(x) x@name=="chartVo"))
    #} else {
    #  chob@windows <- 1
    #  chob@passed.args$TA <- NULL
    #}
  } else chob@windows <- 1
  
  #if(debug) return(str(chob))
  # re-evaluate the TA list, as it will be using stale data,
  chob@passed.args$TA <- sapply(chob@passed.args$TA, function(x) { eval(x@call) } )

  if(plot) # draw the chart
    do.call('chartSeries.chob',list(chob))

  chob@device <- as.numeric(dev.cur())

  write.chob(chob,chob@device)
  invisible(chob)
} #}}}
