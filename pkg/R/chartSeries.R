# chartSeries {{{
`chartSeries` <-
function(x,
         type=c("auto","candlesticks","matchsticks","bars","line"),
         subset=NULL,
         show.grid=TRUE,name=NULL,
         time.scale=NULL,
         TA=c(addVo()),
         line.type="l",
         bar.type="ohlc",
         xlab="time",ylab="price",theme=chartTheme("black"),
         major.ticks='auto',minor.ticks=TRUE,
         up.col,dn.col,color.vol=TRUE,multi.col=FALSE,...
         ) {
  sys.TZ <- Sys.getenv('TZ')
  Sys.setenv(TZ='GMT')
  on.exit(Sys.setenv(TZ=sys.TZ))

  if(!is.xts(x)) x <- as.xts(x)

  indexClass(x) <- "POSIXct"

  if(!is.null(subset)) {
    xsubset <- which(index(x) %in% index(x[subset]))  
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

  ticks <- function(ival, major.ticks, gt = 2, lt = 30) {
      tick.opts <- c("years", "months", "weeks", "days", "hours", 
          "minutes", "seconds")
      if (major.ticks %in% tick.opts) {
          cl <- major.ticks[1]
      }
      else {
          is <- sapply(tick.opts, function(y) {
              length(endpoints(ival, y, 1)) - 1
          })
          cl <- names(is)[which(is > gt & is < lt)][1]
      }
      ep <- endpoints(ival, cl)
      ep
  }
  ep <- ticks(x, major.ticks)
  x.labels <- format(index(x)[ep + 1], "%n%b%n%Y")
  if (time.scale == "weekly" | time.scale == "daily") 
      x.labels <- format(index(x)[ep + 1], "%b %d%n%Y")
  if (time.scale == "minute") 
      x.labels <- format(index(x)[ep + 1], "%b %d%n%H:%M")

  chob <- new("chob")
  chob@call <- match.call(expand=TRUE)
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

  chob@passed.args <- as.list(match.call(expand=TRUE)[-1])
  if(!is.null(TA)) {

    # important to force eval of _current_ chob, not saved chob
    thisEnv <- environment()
    if(is.character(TA)) TA <- as.list(TA)
    chob@passed.args$TA <- list()
    for(ta in 1:length(TA)) {
      if(is.character(TA[[ta]])) {
        chob@passed.args$TA[[ta]] <- eval(parse(text=TA[[ta]]),env=thisEnv)
      } else chob@passed.args$TA[[ta]] <- eval(TA[[ta]],env=thisEnv)
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
# zoomChart {{{
`zoomChart` <-
function(subset=NULL) {
  sys.TZ <- Sys.getenv('TZ')
  Sys.setenv(TZ='GMT')
  on.exit(Sys.setenv(TZ=sys.TZ))

  lchob <- get.current.chob()

  x <- lchob@xdata

  indexClass(x) <- "POSIXct"

  if(!is.null(subset)) {
    xsubset <- which(index(x) %in% index(x[subset]))  
  } else xsubset <- 1:NROW(x)

  xdata <- x
  x <- x[xsubset]

  color.vol <- lchob@color.vol

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
  
  time.scale <- lchob@time.scale

  theme <- lchob@colors

  multi.col <- lchob@multi.col
  show.vol <- lchob@show.vol
  bar.type <- lchob@bar.type
  line.type <- lchob@line.type
  spacing <- lchob@spacing
  width <- lchob@width
  minor.ticks <- lchob@minor.ticks 
  chart <- lchob@type
  

  major.ticks <- lchob@passed.args$major.ticks
  if(is.null(major.ticks)) major.ticks <- 'auto'

  ticks <- function(ival, major.ticks, gt = 2, lt = 30) {
      tick.opts <- c("years", "months", "weeks", "days", "hours", 
          "minutes", "seconds")
      if (major.ticks %in% tick.opts) {
          cl <- major.ticks[1]
      }
      else {
          is <- sapply(tick.opts, function(y) {
              length(endpoints(ival, y, 1)) - 1
          })
          cl <- names(is)[which(is > gt & is < lt)][1]
      }
      ep <- endpoints(ival, cl)
      ep
  }
  ep <- ticks(x, major.ticks)
  x.labels <- format(index(x)[ep + 1], "%n%b%n%Y")
  if (time.scale == "weekly" | time.scale == "daily") 
      x.labels <- format(index(x)[ep + 1], "%b %d%n%Y")
  if (time.scale == "minute") 
      x.labels <- format(index(x)[ep + 1], "%b %d%n%H:%M")

  chob <- new("chob")
  chob@call <- lchob@call
  chob@name <- lchob@name

  chob@xdata <- xdata
  chob@xsubset <- xsubset
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

  chob@passed.args <- lchob@passed.args
  TA <- chob@passed.args$TA
  if(!is.null(TA)) {

    # important to force eval of _current_ chob, not saved chob
    thisEnv <- environment()
    if(is.character(TA)) TA <- as.list(TA)
    chob@passed.args$TA <- list()
    for(ta in 1:length(TA)) {
      if(is.character(TA[[ta]])) {
        chob@passed.args$TA[[ta]] <- eval(parse(text=TA[[ta]]),env=thisEnv)
      } else chob@passed.args$TA[[ta]] <- eval(TA[[ta]],env=thisEnv)
    }
    chob@windows <- length(which(sapply(chob@passed.args$TA,function(x) x@new)))+1
    chob@passed.args$show.vol <- any(sapply(chob@passed.args$TA,function(x) x@name=="chartVo"))
  } else chob@windows <- 1
  
  #if(debug) return(str(chob))
  # re-evaluate the TA list, as it will be using stale data,
  chob@passed.args$TA <- sapply(chob@passed.args$TA, function(x) { eval(x@call) } )

  # draw the chart
  do.call('chartSeries.chob',list(chob))

  chob@device <- lchob@device

  write.chob(chob,chob@device)
  invisible(chob)
} #}}}


# candleChart {{{
`candleChart` <-
function(x,
         type="candlesticks",
         show.grid=TRUE,name=deparse(substitute(x)),
         time.scale=NULL,
         TA=c(addVo()),
         xlab="time",ylab="price",theme=chartTheme("black"),
         up.col,dn.col,color.vol=TRUE,multi.col=FALSE,...
         ) {
  if(is(x,'timeSeries')) x <- zoo(x@Data,as.POSIXct(x@positions))
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
  chart <- 'bars'
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
  ticks <- function(x,gt=2,lt=30) {
      nminutes15 <- function(x) {
          length(breakpoints(x,minutes15,TRUE))-1
        }
      FUNS <-c('nseconds','nminutes','nminutes15','nhours',
            'ndays','nweeks','nmonths',
            'nyears')
      is <-sapply(FUNS[8:1],
                  function(y) { do.call(y,list(x)) })
      cl <- substring(names(is)[which(is > gt & is < lt)],2)[1]
      bp <- breakpoints(x,cl,TRUE)
      bp
    }
  bp <- ticks(x)
  # format the scale
  x.labels <- format(index(x)[bp+1],"%n%b%n%Y")
  if(time.scale=='weekly' | time.scale=='daily')
    x.labels <- format(index(x)[bp+1],"%b %d%n%Y")
  if(time.scale=='minute')
    x.labels <- format(index(x)[bp+1],"%b %d%n%H:%M")
 
  chob <- new("chob")
  chob@call <- match.call(expand=TRUE)
  chob@name <- name
  chob@type <- chart[1]

  chob@xrange <- c(1,NROW(x))
  if(is.OHLC(x)) {
    chob@yrange <- c(min(Lo(x)),max(Hi(x)))
  } else chob@yrange <- range(x)
  


  chob@color.vol <- color.vol
  chob@multi.col <- multi.col
  chob@show.vol <- show.vol
 # chob@bar.type <- bar.type
 # chob@line.type <- line.type
  chob@spacing <- spacing
  chob@width <- width
  chob@bp <- bp
  chob@x.labels <- x.labels
  chob@colors <- theme
  chob@time.scale <- time.scale

  chob@length <- NROW(x)

  chob@passed.args <- as.list(match.call(expand=TRUE)[-1])
  if(!is.null(TA)) {

    # important to force eval of _current_ chob, not saved chob
    thisEnv <- environment()
    if(is.character(TA)) TA <- as.list(TA)
    chob@passed.args$TA <- list()
    for(ta in 1:length(TA)) {
      if(is.character(TA[[ta]])) {
        chob@passed.args$TA[[ta]] <- eval(parse(text=TA[[ta]]),env=thisEnv)
      } else chob@passed.args$TA[[ta]] <- eval(TA[[ta]],env=thisEnv)
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
# barChart {{{
`barChart` <-
function(x,
         type="bars",
         show.grid=TRUE,name=deparse(substitute(x)),
         time.scale=NULL,
         TA=c(addVo()),
         bar.type="ohlc",
         xlab="time",ylab="price",theme=chartTheme("black"),
         up.col,dn.col,color.vol=TRUE,multi.col=FALSE,...
         ) {
  if(is(x,'timeSeries')) x <- zoo(x@Data,as.POSIXct(x@positions))
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
  ticks <- function(x,gt=2,lt=30) {
      nminutes15 <- function(x) {
          length(breakpoints(x,minutes15,TRUE))-1
        }
      FUNS <-c('nseconds','nminutes','nminutes15','nhours',
            'ndays','nweeks','nmonths',
            'nyears')
      is <-sapply(FUNS[8:1],
                  function(y) { do.call(y,list(x)) })
      cl <- substring(names(is)[which(is > gt & is < lt)],2)[1]
      bp <- breakpoints(x,cl,TRUE)
      bp
    }
  bp <- ticks(x)
  # format the scale
  x.labels <- format(index(x)[bp+1],"%n%b%n%Y")
  if(time.scale=='weekly' | time.scale=='daily')
    x.labels <- format(index(x)[bp+1],"%b %d%n%Y")
  if(time.scale=='minute')
    x.labels <- format(index(x)[bp+1],"%b %d%n%H:%M")
 
  chob <- new("chob")
  chob@call <- match.call(expand=TRUE)
  chob@name <- name
  chob@type <- chart[1]

  chob@xrange <- c(1,NROW(x))
  if(is.OHLC(x)) {
    chob@yrange <- c(min(Lo(x)),max(Hi(x)))
  } else chob@yrange <- range(x)
  


  chob@color.vol <- color.vol
  chob@multi.col <- multi.col
  chob@show.vol <- show.vol
  chob@bar.type <- bar.type
  #chob@line.type <- line.type
  chob@spacing <- spacing
  chob@width <- width
  chob@bp <- bp
  chob@x.labels <- x.labels
  chob@colors <- theme
  chob@time.scale <- time.scale

  chob@length <- NROW(x)

  chob@passed.args <- as.list(match.call(expand=TRUE)[-1])
  if(!is.null(TA)) {

    # important to force eval of _current_ chob, not saved chob
    thisEnv <- environment()
    if(is.character(TA)) TA <- as.list(TA)
    chob@passed.args$TA <- list()
    for(ta in 1:length(TA)) {
      if(is.character(TA[[ta]])) {
        chob@passed.args$TA[[ta]] <- eval(parse(text=TA[[ta]]),env=thisEnv)
      } else chob@passed.args$TA[[ta]] <- eval(TA[[ta]],env=thisEnv)
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
# lineChart {{{
`lineChart` <-
function(x,
         type="line",
         show.grid=TRUE,name=deparse(substitute(x)),
         time.scale=NULL,
         TA=c(addVo()),
         line.type="l",
         xlab="time",ylab="price",theme=chartTheme("black"),
         up.col,dn.col,color.vol=TRUE,multi.col=FALSE,...
         ) {
  if(is(x,'timeSeries')) x <- zoo(x@Data,as.POSIXct(x@positions))
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
  ticks <- function(x,gt=2,lt=30) {
      nminutes15 <- function(x) {
          length(breakpoints(x,minutes15,TRUE))-1
        }
      FUNS <-c('nseconds','nminutes','nminutes15','nhours',
            'ndays','nweeks','nmonths',
            'nyears')
      is <-sapply(FUNS[8:1],
                  function(y) { do.call(y,list(x)) })
      cl <- substring(names(is)[which(is > gt & is < lt)],2)[1]
      bp <- breakpoints(x,cl,TRUE)
      bp
    }
  bp <- ticks(x)
  # format the scale
  x.labels <- format(index(x)[bp+1],"%n%b%n%Y")
  if(time.scale=='weekly' | time.scale=='daily')
    x.labels <- format(index(x)[bp+1],"%b %d%n%Y")
  if(time.scale=='minute')
    x.labels <- format(index(x)[bp+1],"%b %d%n%H:%M")
 
  chob <- new("chob")
  chob@call <- match.call(expand=TRUE)
  chob@name <- name
  chob@type <- chart[1]

  chob@xrange <- c(1,NROW(x))
  if(is.OHLC(x)) {
    chob@yrange <- c(min(Lo(x)),max(Hi(x)))
  } else chob@yrange <- range(x)
  


  chob@color.vol <- color.vol
  chob@multi.col <- multi.col
  chob@show.vol <- show.vol
  #chob@bar.type <- bar.type
  chob@line.type <- line.type
  chob@spacing <- spacing
  chob@width <- width
  chob@bp <- bp
  chob@x.labels <- x.labels
  chob@colors <- theme
  chob@time.scale <- time.scale

  chob@length <- NROW(x)

  chob@passed.args <- as.list(match.call(expand=TRUE)[-1])
  if(!is.null(TA)) {

    # important to force eval of _current_ chob, not saved chob
    thisEnv <- environment()
    if(is.character(TA)) TA <- as.list(TA)
    chob@passed.args$TA <- list()
    for(ta in 1:length(TA)) {
      if(is.character(TA[[ta]])) {
        chob@passed.args$TA[[ta]] <- eval(parse(text=TA[[ta]]),env=thisEnv)
      } else chob@passed.args$TA[[ta]] <- eval(TA[[ta]],env=thisEnv)
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

# .chart.theme {{{
`.chart.theme` <- structure(list('white'=
                           list(fg.col="#888888",bg.col="#FFFFFF",
                                grid.col="#CCCCCC",border="#666666",
                                minor.tick="#CCCCCC",major.tick="#888888",
                                up.col="#00CC00",dn.col="#FF7700",
                                dn.up.col="#888888",up.up.col="#FFFFFF",
                                dn.dn.col="#FF0000",up.dn.col="#000000",
                                up.border="#666666",dn.border="#666666",
                                dn.up.border="#666666",up.up.border="#666666",
                                dn.dn.border="#666666",up.dn.border="#666666",
                                main.col="#555555",sub.col="#555555"
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
                                main.col="#999999",sub.col="#999999"
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
                                main.col="#555555",sub.col="#555555"
                                )
                     ), class='chart.theme')
# }}}

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

