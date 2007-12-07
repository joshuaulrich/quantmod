`setTA` <- function(...) {
  as.character(match.call()[-1])
}

# chartSeries generic {{{
`chartSeries` <- 
function(x,
         type=c("auto","candlesticks","matchsticks","bars","line"),
         show.vol=TRUE,
         show.grid=TRUE,name=deparse(substitute(x)),
         time.scale=NULL,
         technicals=NULL,
         line.type="l",
         bar.type='ohlc',
         xlab="time",ylab="price",theme="black",
         up.col,dn.col,color.vol=TRUE,multi.col=FALSE,...
         ) {
  UseMethod('chartSeries')
} # }}}
# chartSeries0 generic WORKING {{{
`chartSeries0` <- 
function(x,
         type=c("auto","candlesticks","matchsticks","bars","line"),
         show.vol=TRUE,
         show.grid=TRUE,name=deparse(substitute(x)),
         time.scale=NULL,
         technicals=NULL,
         line.type="l",
         bar.type='ohlc',
         xlab="time",ylab="price",theme="black",
         up.col,dn.col,color.vol=TRUE,multi.col=FALSE,...
         ) {
  UseMethod('chartSeries0')
} # }}}

# chartSeries.zoo {{{
`chartSeries.zoo` <-
function(x,
         type=c("auto","candlesticks","matchsticks","bars","line"),
         show.vol=TRUE,
         show.grid=TRUE,name=deparse(substitute(x)),
         time.scale=NULL,
         technicals=NULL,
         line.type="l",
         bar.type="ohlc",
         xlab="time",ylab="price",theme="black",
         up.col,dn.col,color.vol=TRUE,multi.col=FALSE,...
         ) {
#  if(class(x)[1]=='timeSeries')
#    x <- zoo(seriesData(x),as.Date(as.character(rownames(x))))
#  if(class(x)[1]=="quantmod.OHLC" || NCOL(x) >= 5) {
  if(is.OHLC(x)) {
#     || (is.zoo(x) &
#     all(suppressWarnings(
#        grep("(Op)|(Hi)|(Lo)|(Cl)",colnames(x),ignore.case=TRUE)==1:4)))) {
    Opens <- as.numeric(Op(x))
    Highs <- as.numeric(Hi(x))
    Lows <- as.numeric(Lo(x))
    Closes <- as.numeric(Cl(x))
  } else {
    Lows <- min(x[,1])
    Highs <- max(x[,1])
    Closes <- as.numeric(x[,1])
    type <- "line"
  } 
  if(has.Vo(x)) {
    Volumes <- as.numeric(Vo(x))
  } else show.vol <- FALSE
  
  #if(identical(Volumes,numeric(0))) show.vol <- FALSE
  if(is.null(time.scale)) {
    time.scale <- periodicity(x)$scale
  }
  
  # before messing with graphics, save...
  old <- par(c('pty','mar','xpd','bg','xaxs','las','col.axis','fg','fig'))
  on.exit(par(old))

  if(theme=="black") {
    bg.col <- "#222222"
    fg.col <- "#666666"
    up.col <- ifelse(missing(up.col),"#00FF00",up.col)
    dn.col <- ifelse(missing(dn.col),"#FF9900",dn.col)
  }
  if(theme=="white") {
    bg.col <- "#FFFFFF"
    fg.col <- "#444444"
    up.col <- ifelse(missing(up.col),"#00CC00",up.col)
    dn.col <- ifelse(missing(dn.col),"#FF7700",dn.col)
  }
  if(theme=="grey") {
    bg.col <- "#FFFFFF"
    fg.col <- "#444444"
    up.col <- ifelse(missing(up.col),"#FFFFFF",up.col)
    dn.col <- ifelse(missing(dn.col),"#000000",dn.col)
  }
  #if(is.logical(multi.col) | length(multi.col)==4) {
  if(missing(multi.col)) { # interpret as TRUE
    multi.col <- FALSE
  } else {
    if(is.logical(multi.col) && multi.col==TRUE) {
      multi.col <- c("#666666","#FFFFFF",
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
    spacing <- 4
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

  # determine formatting for time-axis
  for(time.period in c('years','months','weeks')) {
    bp <- breakpoints(x,by=time.period,TRUE)
    if(length(bp) > 3) {
      x.labels <- format(index(x)[bp+1],"%b %y")
      if(time.period=='weeks')
        x.labels <- format(index(x)[bp+1],"%b %d %Y")
      break
    }
  }

  x.range <- 1:(NROW(x)*spacing)
  y.range <- seq(min(Lows),max(Highs),length.out=length(x.range))

  # determine type of technicals to draw
  # if they need there own plot area, or
  # are overlays.
  # default choice is based upon 
  
  if(show.vol) {
    layout(matrix(c(1,2),2,1,byrow=TRUE),1,c(2,1),respect=FALSE)
    par(mar=c(1,4,3,3))
  }
  par(bg=bg.col,col.axis=fg.col,xaxs='r',las=2,fg='#bbbbbb')
  plot(x.range,y.range,type='n',axes=FALSE,ann=FALSE,
       ylim=c(min(Lows),max(Highs)))
  if(show.vol) par(new=TRUE)
  if(show.grid) grid(NA,NULL,col=fg.col)
  if(chart[1]=='line') {
    # plot line
    lines(1:length(Closes),Closes,col='blue',lwd=2,type=line.type)
  } else 
  if(chart[1]=='bars') {
    # plot bars
    tick.size <- ifelse(NROW(x) > 100, 1, 0.5)
    for(i in 1:NROW(x)) {
      O.to.C <- c(Opens[i],Closes[i])
      L.to.H <- c(Lows[i],Highs[i])
      if(i > 1 & multi.col) {
        if(Opens[i] < Closes[i] & Opens[i] < Closes[i-1]) bar.col <- dn.up.col
        if(Opens[i] < Closes[i] & Opens[i] > Closes[i-1]) bar.col <- up.up.col
        if(Opens[i] > Closes[i] & Opens[i] < Closes[i-1]) bar.col <- dn.dn.col
        if(Opens[i] > Closes[i] & Opens[i] > Closes[i-1]) bar.col <- up.dn.col
      } else {
        bar.col <- ifelse(O.to.C[1] > O.to.C[2],dn.col,up.col)
      }
      x.pos <- 1+spacing*(i-1)
      # create full bar
      lines(c(x.pos,x.pos),L.to.H,
            lwd=width,col=bar.col)
      # create open tick
      if(bar.type=="ohlc") {
        segments(x.pos-tick.size,Opens[i],x.pos,Opens[i],
                 lwd=width,col=bar.col)
      }
      # create close tick
      segments(x.pos,Closes[i],x.pos+tick.size,Closes[i],
               lwd=width,col=bar.col)
      if(bar.type=="hlc") {
        segments(x.pos-tick.size,Closes[i],x.pos,Closes[i],
                 lwd=width,col=bar.col)
      }
    }
  } else {
    # plot candlesticks or matchsticks
    for(i in 1:NROW(x)) {
      O.to.C <- c(Opens[i],Closes[i])
      L.to.H <- c(Lows[i],Highs[i])
      if(i > 1 & multi.col) {
        if(Opens[i] < Closes[i] & Opens[i] < Closes[i-1]) bar.col <- dn.up.col
        if(Opens[i] < Closes[i] & Opens[i] > Closes[i-1]) bar.col <- up.up.col
        if(Opens[i] > Closes[i] & Opens[i] < Closes[i-1]) bar.col <- dn.dn.col
        if(Opens[i] > Closes[i] & Opens[i] > Closes[i-1]) bar.col <- up.dn.col
        border.col <- "#444444"
      } else {
        bar.col <- ifelse(O.to.C[1] > O.to.C[2],dn.col,up.col)
        border.col <- ifelse(multi.col,"#444444",bar.col)
      }
      x.pos <- 1+spacing*(i-1)
      lines(c(x.pos,x.pos),L.to.H,lwd=1,col="#666666") # full range grey line
      if(chart[1]=='matchsticks') {
        lines(c(x.pos,x.pos),O.to.C,lwd=width,col=bar.col)
      } else {
        rect(x.pos-spacing/5,O.to.C[1],x.pos+spacing/5,
             O.to.C[2],col=bar.col,border=border.col)
      }
    }
  }
  title(ylab=ylab,col.lab=fg.col)
  title(main=paste(name),col.main=fg.col,font.main=4)
  if(!show.vol) {
    axis(1,at=1:NROW(x)*spacing+1,labels=FALSE,col="#444444")
    axis(1,at=bp*spacing+1,labels=x.labels,las=1)
  }
  axis(2)
  box(col=fg.col)
  
  if(show.vol) {
  # if volume is to be plotted, do so here
    # scale volume - vol.divisor
    max.vol <- max(Volumes)
    vol.scale <- list(100,'100s')
    if(max.vol > 10000) vol.scale <- list(1000,'1000s') 
    if(max.vol > 100000) vol.scale <- list(10000,'10,000s')
    if(max.vol > 1000000) vol.scale <- list(100000,'100,000s')
    if(max.vol > 10000000) vol.scale <- list(1000000,'millions')
    par(mar=c(3,4,0,3))
    plot(x.range,seq(min(Vo(x))/vol.scale[[1]],max(Vo(x))/vol.scale[[1]],
         length.out=length(x.range)),
         type='n',axes=FALSE,ann=FALSE)
    bar.col <- fg.col
    for(i in 1:NROW(x)) {
      Vols <- c(0,Volumes[i]/vol.scale[[1]])
      x.pos <- 1+spacing*(i-1)
      if(color.vol) {
        if(i > 1 & multi.col & color.vol) {
          if(Opens[i] < Closes[i] & Opens[i] < Closes[i-1]) bar.col <- dn.up.col
          if(Opens[i] < Closes[i] & Opens[i] > Closes[i-1]) bar.col <- up.up.col
          if(Opens[i] > Closes[i] & Opens[i] < Closes[i-1]) bar.col <- dn.dn.col
          if(Opens[i] > Closes[i] & Opens[i] > Closes[i-1]) bar.col <- up.dn.col
        } else {
          bar.col <- ifelse(Opens[i] > Closes[i],dn.col,up.col)
        }
      }
      #if(color.vol) 
      #  bar.col <- ifelse(Opens[i] > Closes[i],dn.col,up.col)
      #lines(c(x.pos,x.pos),Vols,lwd=width,col=bar.col)
      border.col <- ifelse(multi.col,"#000000",bar.col)
      rect(x.pos-spacing/4,0,x.pos+spacing/4,Vols[2],col=bar.col,border=border.col)
    }
    title(ylab=paste("volume (",vol.scale[[2]],")"),
          xlab=time.scale,col.lab=fg.col)
    axis(1,at=1:NROW(x)*spacing+1,labels=FALSE,col="#444444")
    axis(1,at=bp*spacing+1,labels=x.labels,las=1)
    axis(2)
    box(col=fg.col)
  } else {
    # add axes label
    title(xlab=time.scale,col.lab=fg.col)
  }
  # eventually return a chart object (a chob, in the spirit of grob)
  # for use in subsequent calls to modify params, add indicators etc...
  layout(matrix(1))
  invisible(1)
} #}}}

# cS2 {{{
`cS2` <-
function(x,debug=FALSE,
         type=c("auto","candlesticks","matchsticks","bars","line"),
         show.vol=FALSE,
         show.grid=TRUE,name=deparse(substitute(x)),
         time.scale=NULL,
         TA=c(addVo()),
         line.type="l",
         bar.type="ohlc",
         xlab="time",ylab="price",theme="black",
         up.col,dn.col,color.vol=TRUE,multi.col=FALSE,...
         ) {
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
  } 
  if(has.Vo(x)) {
    Volumes <- as.numeric(Vo(x))
  } else show.vol <- FALSE
  
  if(is.null(time.scale)) {
    time.scale <- periodicity(x)$scale
  }

  if(theme=="black") {
    bg.col <- "#222222"
    fg.col <- "#666666"
    up.col <- ifelse(missing(up.col),"#00FF00",up.col)
    dn.col <- ifelse(missing(dn.col),"#FF9900",dn.col)
  }
  if(theme=="white") {
    bg.col <- "#FFFFFF"
    fg.col <- "#444444"
    up.col <- ifelse(missing(up.col),"#00CC00",up.col)
    dn.col <- ifelse(missing(dn.col),"#FF7700",dn.col)
  }
  if(theme=="grey") {
    bg.col <- "#FFFFFF"
    fg.col <- "#444444"
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
      multi.col <- c("#666666","#FFFFFF",
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
    spacing <- 4
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

  chob@xrange <- c(1,NROW(x))
  if(is.OHLC(x)) {
    chob@yrange <- c(min(Lo(x)),max(Hi(x)))
  } else chob@yrange <- range(x)
  


  chob@color.vol <- color.vol
  chob@multi.col <- multi.col
  chob@spacing <- spacing
  chob@width <- width
  chob@bp <- bp
  chob@x.labels <- x.labels
  chob@colors <- list(fg.col=fg.col,bg.col=bg.col,up.col=up.col,dn.col=dn.col,
                      dn.up.col=dn.up.col,up.up.col=up.up.col,
                      dn.dn.col=dn.dn.col,up.dn.col=up.dn.col)
  chob@time.scale <- time.scale

  chob@length <- NROW(x)

  chob@passed.args <- as.list(match.call(expand=TRUE)[-1])
  #if(show.vol) TA <- c(addVo(),TA)
  if(!is.null(TA)) {
    if(is.character(TA)) TA <- as.list(TA)
    chob@passed.args$TA <- list()
    for(ta in 1:length(TA)) {
      if(is.character(TA[[ta]])) {
        chob@passed.args$TA[[ta]] <- eval(parse(text=TA[[ta]]))
      } else chob@passed.args$TA[[ta]] <- eval(TA[[ta]])
    }
#  }

#  if(!is.null(chob@passed.args$TA)) {
    chob@windows <- length(which(sapply(chob@passed.args$TA,function(x) x@new)))+1
    chob@passed.args$show.vol <- any(sapply(chob@passed.args$TA,function(x) x@name=="chartVo"))
  } else chob@windows <- 1
  
if(debug) return(str(chob))
  # re-evaluate the TA list, as it will be using stale data,
  chob@passed.args$TA <- sapply(chob@passed.args$TA, function(x) { eval(x@call) } )

  # draw the chart
  do.call('chartSeries.chob',list(chob))

  chob@device <- as.numeric(dev.cur())

  write.chob(chob,chob@device)
  invisible(chob)
} #}}}

# chartSeries.timeSeries {{{
`chartSeries.timeSeries` <-
function(x,
         type=c("auto","candlesticks","matchsticks","bars","line"),
         show.vol=TRUE,
         show.grid=TRUE,name=deparse(substitute(x)),
         time.scale=NULL,
         technicals=NULL,
         line.type="l",
         bar.type='ohlc',
         xlab="time",ylab="price",theme="black",
         up.col,dn.col,color.vol=TRUE,multi.col,...
         ) {
  force(name) # weird scoping issue - I'll never realy know it all
  x <- zoo(seriesData(x),as.Date(as.character(rownames(x))))
  chartSeries(x=x,type=type,show.vol=show.vol,
                             show.grid=show.grid,name=name,
                             time.scale=time.scale,technicals=technicals,
                             line.type=line.type,xlab=xlab,ylab=ylab,
                             theme=theme)
} # }}}

# barchart {{{
`barChart` <- function(x,name=deparse(substitute(x)),type='bars',...)
{
  chartSeries(x=x,type=type,name=name,...)
} # }}}

# candleChart {{{
`candleChart` <- function(x,name=deparse(substitute(x)),type="candlesticks",...)
{
  chartSeries(x=x,type=type,name=name,...)
} # }}}

# matchChart {{{
`matchChart` <- function(x,name=deparse(substitute(x)),type="matchsticks",...)
{
  chartSeries(x=x,type=type,name=name,...)
} #}}}

# lineChart {{{
`lineChart` <- function(x,name=deparse(substitute(x)),type="line",color.vol=FALSE,...)
{
  chartSeries(x=x,type=type,name=name,color.vol=color.vol,...)
} # }}}

# chartSeries0.zoo WORKING {{{
`chartSeries0.zoo` <-
function(x,
         type=c("auto","candlesticks","matchsticks","bars","line"),
         show.vol=TRUE,
         show.grid=TRUE,name=deparse(substitute(x)),
         time.scale=NULL,
         technicals=NULL,
         line.type="l",
         bar.type="ohlc",
         xlab="time",ylab="price",theme="black",
         up.col,dn.col,color.vol=TRUE,multi.col=FALSE,...
         ) {
#  if(class(x)[1]=='timeSeries')
#    x <- zoo(seriesData(x),as.Date(as.character(rownames(x))))
  if(class(x)[1]=="quantmod.OHLC" || NCOL(x) >= 5) {
#     || (is.zoo(x) &
#     all(suppressWarnings(
#        grep("(Op)|(Hi)|(Lo)|(Cl)",colnames(x),ignore.case=TRUE)==1:4)))) {
    Opens <- as.numeric(Op(x))
    Highs <- as.numeric(Hi(x))
    Lows <- as.numeric(Lo(x))
    Closes <- as.numeric(Cl(x))
  } else {
    Lows <- min(x[,1])
    Highs <- max(x[,1])
    Closes <- as.numeric(x[,1])
    type <- "line"
  } 
  Volumes <- as.numeric(Vo(x))
  if(identical(Volumes,numeric(0))) show.vol <- FALSE
  if(is.null(time.scale)) {
    time.scale <- periodicity(x)$scale
  }
  
  # before messing with graphics, save...
  old <- par(c('pty','mar','xpd','bg','xaxs','las','col.axis','fg','fig'))
  on.exit(par(old))

  if(theme=="black") {
    bg.col <- "#222222"
    fg.col <- "#666666"
    up.col <- ifelse(missing(up.col),"#00FF00",up.col)
    dn.col <- ifelse(missing(dn.col),"#FF9900",dn.col)
  }
  if(theme=="white") {
    bg.col <- "#FFFFFF"
    fg.col <- "#444444"
    up.col <- ifelse(missing(up.col),"#00CC00",up.col)
    dn.col <- ifelse(missing(dn.col),"#FF7700",dn.col)
  }
  if(theme=="grey") {
    bg.col <- "#FFFFFF"
    fg.col <- "#444444"
    up.col <- ifelse(missing(up.col),"#FFFFFF",up.col)
    dn.col <- ifelse(missing(dn.col),"#000000",dn.col)
  }
  #if(is.logical(multi.col) | length(multi.col)==4) {
  if(missing(multi.col)) { # interpret as TRUE
    multi.col <- FALSE
  } else {
    if(is.logical(multi.col) && multi.col==TRUE) {
      multi.col <- c("#666666","#FFFFFF",
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
    spacing <- 4
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

  # determine formatting for time-axis
  for(time.period in c('years','months','weeks')) {
    bp <- breakpoints(x,by=time.period,TRUE)
    if(length(bp) > 3) {
      x.labels <- format(index(x)[bp+1],"%b %y")
      if(time.period=='weeks')
        x.labels <- format(index(x)[bp+1],"%b %d %Y")
      break
    }
  }

  x.range <- 1:(NROW(x)*spacing)
  y.range <- seq(min(Lows),max(Highs),length.out=length(x.range))

  # determine type of technicals to draw
  # if they need there own plot area, or
  # are overlays.
  # default choice is based upon 
  
  if(show.vol) {
    par(fig=c(0,1,0.30,1))
    par(mar=c(0,4,4,2))
  }
  par(bg=bg.col,col.axis=fg.col,xaxs='r',las=2,fg='#bbbbbb')
  plot(x.range,y.range,type='n',axes=FALSE,ann=FALSE,
       ylim=c(min(Lows),max(Highs)))
  if(show.vol) par(new=TRUE)
  if(show.grid) grid(NA,NULL,col=fg.col)
  if(chart[1]=='line') {
    # plot line
    lines(1:length(Closes),Closes,col='blue',lwd=2,type=line.type)
  } else 
  if(chart[1]=='bars') {
    # plot bars
    tick.size <- ifelse(NROW(x) > 100, 1, 0.5)
    for(i in 1:NROW(x)) {
      O.to.C <- c(Opens[i],Closes[i])
      L.to.H <- c(Lows[i],Highs[i])
      if(i > 1 & multi.col) {
        if(Opens[i] < Closes[i] & Opens[i] < Closes[i-1]) bar.col <- dn.up.col
        if(Opens[i] < Closes[i] & Opens[i] > Closes[i-1]) bar.col <- up.up.col
        if(Opens[i] > Closes[i] & Opens[i] < Closes[i-1]) bar.col <- dn.dn.col
        if(Opens[i] > Closes[i] & Opens[i] > Closes[i-1]) bar.col <- up.dn.col
      } else {
        bar.col <- ifelse(O.to.C[1] > O.to.C[2],dn.col,up.col)
      }
      x.pos <- 1+spacing*(i-1)
      # create full bar
      lines(c(x.pos,x.pos),L.to.H,
            lwd=width,col=bar.col)
      # create open tick
      if(bar.type=="ohlc") {
        segments(x.pos-tick.size,Opens[i],x.pos,Opens[i],
                 lwd=width,col=bar.col)
      }
      # create close tick
      segments(x.pos,Closes[i],x.pos+tick.size,Closes[i],
               lwd=width,col=bar.col)
      if(bar.type=="hlc") {
        segments(x.pos-tick.size,Closes[i],x.pos,Closes[i],
                 lwd=width,col=bar.col)
      }
    }
  } else {
    # plot candlesticks or matchsticks
    for(i in 1:NROW(x)) {
      O.to.C <- c(Opens[i],Closes[i])
      L.to.H <- c(Lows[i],Highs[i])
      if(i > 1 & multi.col) {
        if(Opens[i] < Closes[i] & Opens[i] < Closes[i-1]) bar.col <- dn.up.col
        if(Opens[i] < Closes[i] & Opens[i] > Closes[i-1]) bar.col <- up.up.col
        if(Opens[i] > Closes[i] & Opens[i] < Closes[i-1]) bar.col <- dn.dn.col
        if(Opens[i] > Closes[i] & Opens[i] > Closes[i-1]) bar.col <- up.dn.col
        border.col <- "#444444"
      } else {
        bar.col <- ifelse(O.to.C[1] > O.to.C[2],dn.col,up.col)
        border.col <- ifelse(multi.col,"#444444",bar.col)
      }
      x.pos <- 1+spacing*(i-1)
      lines(c(x.pos,x.pos),L.to.H,lwd=1,col="#666666") # full range grey line
      #lines(c(x.pos,x.pos),O.to.C,lwd=width,col=bar.col)
      rect(x.pos-spacing/5,O.to.C[1],x.pos+spacing/5,
          O.to.C[2],col=bar.col,border=border.col)
    }
  }
  title(ylab=ylab,col.lab=fg.col)
  title(main=paste(name),col.main=fg.col,font.main=4)
  if(!show.vol) {
    axis(1,at=bp*spacing+1,labels=x.labels,las=1)
  }
  axis(2)
  box(col=fg.col)
  
  if(show.vol) {
  # if volume is to be plotted, do so here
    # scale volume - vol.divisor
    max.vol <- max(Volumes)
    vol.scale <- list(100,'100s')
    if(max.vol > 10000) vol.scale <- list(1000,'1000s') 
    if(max.vol > 100000) vol.scale <- list(10000,'10,000s')
    if(max.vol > 1000000) vol.scale <- list(100000,'100,000s')
    if(max.vol > 10000000) vol.scale <- list(1000000,'millions')
    par(new=TRUE)
    par(fig=c(0,1,0,0.35))
    par(mar=c(5,4,3,2))
    plot(x.range,seq(min(Vo(x))/vol.scale[[1]],max(Vo(x))/vol.scale[[1]],
         length.out=length(x.range)),
         type='n',axes=FALSE,ann=FALSE)
    bar.col <- fg.col
    for(i in 1:NROW(x)) {
      Vols <- c(0,Volumes[i]/vol.scale[[1]])
      x.pos <- 1+spacing*(i-1)
      if(color.vol) {
        if(i > 1 & multi.col & color.vol) {
          if(Opens[i] < Closes[i] & Opens[i] < Closes[i-1]) bar.col <- dn.up.col
          if(Opens[i] < Closes[i] & Opens[i] > Closes[i-1]) bar.col <- up.up.col
          if(Opens[i] > Closes[i] & Opens[i] < Closes[i-1]) bar.col <- dn.dn.col
          if(Opens[i] > Closes[i] & Opens[i] > Closes[i-1]) bar.col <- up.dn.col
        } else {
          bar.col <- ifelse(Opens[i] > Closes[i],dn.col,up.col)
        }
      }
      #if(color.vol) 
      #  bar.col <- ifelse(Opens[i] > Closes[i],dn.col,up.col)
      #lines(c(x.pos,x.pos),Vols,lwd=width,col=bar.col)
      border.col <- ifelse(chart[1]=="candlesticks","#000000",bar.col)
      rect(x.pos-spacing/4,0,x.pos+spacing/4,Vols[2],col=bar.col,border=border.col)
    }
    title(ylab=paste("volume (",vol.scale[[2]],")"),
          xlab=time.scale,col.lab=fg.col)
    axis(1,at=bp*spacing+1,labels=x.labels,las=1)
    axis(2)
    box(col=fg.col)
  } else {
    # add axes label
    title(xlab=time.scale,col.lab=fg.col)
  }
  # eventually return a chart object (a chob, in the spirit of grob)
  # for use in subsequent calls to modify params, add indicators etc...
  invisible(1)
} #}}}
