# chartSeries {{{
`chartSeries` <-
function(x,
         type=c("auto","candlesticks","matchsticks","bars","line"),
         show.grid=TRUE,name=deparse(substitute(x)),
         time.scale=NULL,
         TA=c(addVo()),
         line.type="l",
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
  if(!missing(up.col)) theme$colors$up.col <- up.col 
  if(!missing(dn.col)) theme$colors$dn.col <- dn.col 
  if(missing(multi.col) | !multi.col) { # interpret as FALSE
    multi.col <- FALSE
    theme$colors$dn.up.col <- theme$colors$up.col
    theme$colors$up.up.col <- theme$colors$up.col
    theme$colors$dn.dn.col <- theme$colors$dn.col
    theme$colors$up.dn.col <- theme$colors$dn.col
  } else {
    if(is.character(multi.col)) {
      # add some check for length 4 colors
      theme$colors$dn.up.col <- multi.col[1]
      theme$colors$up.up.col <- multi.col[2]
      theme$colors$dn.dn.col <- multi.col[3]
      theme$colors$up.dn.col <- multi.col[4]
    }
    theme$colors$up.col <- theme$colors$up.up.col
    theme$colors$dn.col <- theme$colors$dn.dn.col
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
  chob@line.type <- line.type
  chob@spacing <- spacing
  chob@width <- width
  chob@bp <- bp
  chob@x.labels <- x.labels
  chob@colors <- theme$colors
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
         show.grid=TRUE,name=deparse(substitute(x)),
         time.scale=NULL,
         TA=c(addVo()),
         line.type="l",
         bar.type="ohlc",
         xlab="time",ylab="price",theme="black",
         up.col,dn.col,color.vol=TRUE,multi.col=FALSE,...
         ) {
  type <- 'bars'
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

  if(theme=="black") {
    bg.col <- "#222222"
    fg.col <- "#666666"
    grid.col <- "#303030"
    up.col <- ifelse(missing(up.col),"#00FF00",up.col)
    dn.col <- ifelse(missing(dn.col),"#FF9900",dn.col)
  }
  if(theme=="white") {
    bg.col <- "#FFFFFF"
    fg.col <- "#444444"
    grid.col <- "#D0D0D0"
    up.col <- ifelse(missing(up.col),"#00CC00",up.col)
    dn.col <- ifelse(missing(dn.col),"#FF7700",dn.col)
  }
  if(theme=="grey") {
    bg.col <- "#FFFFFF"
    fg.col <- "#444444"
    grid.col <- "#D0D0D0"
    up.col <- ifelse(missing(up.col),"#FFFFFF",up.col)
    dn.col <- ifelse(missing(dn.col),"#000000",dn.col)
  }
  #if(is.logical(multi.col) | length(multi.col)==4) {
  if(missing(multi.col)) { # interpret as FALSE
    multi.col <- FALSE
    dn.up.col <- up.col
    up.up.col <- up.col
    dn.dn.col <- dn.col
    up.dn.col <- dn.col
  } else {
    if(is.logical(multi.col) && multi.col==TRUE) {
      multi.col <- c("#888888","#FFFFFF",
                     "#FF0000","#000000") 
    }
    # add some check for length 4 colors
    dn.up.col <- multi.col[1]
    up.up.col <- multi.col[2]
    dn.dn.col <- multi.col[3]
    up.dn.col <- multi.col[4]
    up.col <- up.up.col
    dn.col <- dn.dn.col
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
  chob@line.type <- line.type
  chob@spacing <- spacing
  chob@width <- width
  chob@bp <- bp
  chob@x.labels <- x.labels
  chob@colors <- list(fg.col=fg.col,bg.col=bg.col,grid.col=grid.col,
                      up.col=up.col,dn.col=dn.col,
                      dn.up.col=dn.up.col,up.up.col=up.up.col,
                      dn.dn.col=dn.dn.col,up.dn.col=up.dn.col)
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
# candleChart {{{
`candleChart` <-
function(x,
         show.grid=TRUE,name=deparse(substitute(x)),
         time.scale=NULL,
         TA=c(addVo()),
         line.type="l",
         bar.type="ohlc",
         xlab="time",ylab="price",theme="black",
         up.col,dn.col,color.vol=TRUE,multi.col=FALSE,...
         ) {
  type <- 'candlesticks'
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

  if(theme=="black") {
    bg.col <- "#222222"
    fg.col <- "#666666"
    grid.col <- "#303030"
    up.col <- ifelse(missing(up.col),"#00FF00",up.col)
    dn.col <- ifelse(missing(dn.col),"#FF9900",dn.col)
  }
  if(theme=="white") {
    bg.col <- "#FFFFFF"
    fg.col <- "#444444"
    grid.col <- "#D0D0D0"
    up.col <- ifelse(missing(up.col),"#00CC00",up.col)
    dn.col <- ifelse(missing(dn.col),"#FF7700",dn.col)
  }
  if(theme=="grey") {
    bg.col <- "#FFFFFF"
    fg.col <- "#444444"
    grid.col <- "#D0D0D0"
    up.col <- ifelse(missing(up.col),"#FFFFFF",up.col)
    dn.col <- ifelse(missing(dn.col),"#000000",dn.col)
  }
  #if(is.logical(multi.col) | length(multi.col)==4) {
  if(missing(multi.col)) { # interpret as FALSE
    multi.col <- FALSE
    dn.up.col <- up.col
    up.up.col <- up.col
    dn.dn.col <- dn.col
    up.dn.col <- dn.col
  } else {
    if(is.logical(multi.col) && multi.col==TRUE) {
      multi.col <- c("#888888","#FFFFFF",
                     "#FF0000","#000000") 
    }
    # add some check for length 4 colors
    dn.up.col <- multi.col[1]
    up.up.col <- multi.col[2]
    dn.dn.col <- multi.col[3]
    up.dn.col <- multi.col[4]
    up.col <- up.up.col
    dn.col <- dn.dn.col
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
  chob@line.type <- line.type
  chob@spacing <- spacing
  chob@width <- width
  chob@bp <- bp
  chob@x.labels <- x.labels
  chob@colors <- list(fg.col=fg.col,bg.col=bg.col,grid.col=grid.col,
                      up.col=up.col,dn.col=dn.col,
                      dn.up.col=dn.up.col,up.up.col=up.up.col,
                      dn.dn.col=dn.dn.col,up.dn.col=up.dn.col)
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
                      list(colors=
                           list(fg.col="#888888",bg.col="#FFFFFF",
                                grid.col="#CCCCCC",border="#666666",
                                minor.tick="#CCCCCC",major.tick="#888888",
                                up.col="#00CC00",dn.col="#FF7700",
                                dn.up.col="#888888",up.up.col="#FFFFFF",
                                dn.dn.col="#FF0000",up.dn.col="#000000",
                                up.border="#666666",dn.border="#666666",
                                dn.up.border="#666666",up.up.border="#666666",
                                dn.dn.border="#666666",up.dn.border="#666666"
                                ),
                           TA=list()
                          ),
                      'black'=
                      list(colors=
                           list(fg.col="#666666",bg.col="#222222",
                                grid.col="#303030",border="#666666",
                                minor.tick="#CCCCCC",major.tick="#AAAAAA",
                                up.col="#00FF00",dn.col="#FF9900",
                                dn.up.col="#888888",up.up.col="#FFFFFF",
                                dn.dn.col="#FF0000",up.dn.col="#000000",
                                up.border="#666666",dn.border="#666666",
                                dn.up.border="#666666",up.up.border="#666666",
                                dn.dn.border="#666666",up.dn.border="#666666"
                                ),
                           TA=list()
                          ),
                      'beige'=
                      list(colors=
                           list(fg.col="#888888",bg.col="#F5F5D0",
                                grid.col="#CCCCCC",border="#666666",
                                minor.tick="#CCCCCC",major.tick="#AAAAAA",
                                up.col="#00FF00",dn.col="#AA0000",
                                dn.up.col="#888888",up.up.col="#FFFFFF",
                                dn.dn.col="#FF0000",up.dn.col="#000000",
                                up.border="#666666",dn.border="#666666",
                                dn.up.border="#666666",up.up.border="#666666",
                                dn.dn.border="#666666",up.dn.border="#666666"
                                ),
                           TA=list()
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
    if(i %in% names(current.theme$colors)) current.theme$colors[[i]] <- ll[[i]]
  }
  return(structure(current.theme,class='chart.series'))
}#}}}
