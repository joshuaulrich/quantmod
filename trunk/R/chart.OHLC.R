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
         up.col,dn.col,color.vol=TRUE,col.candles=FALSE
         ) {
  UseMethod('chartSeries')
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
         up.col,dn.col,color.vol=TRUE,col.candles=FALSE
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
    period <- periodicity(x)
    time.scale <- "days"
    if(period > 2) time.scale <- "weeks" 
    if(period > 7) time.scale <- "months" 
    if(period > 31) time.scale <- "years" 
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
  if(col.candles) {
    up.col <- "#666666"
    dn.col <- "#FFFFFF"
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
      if(i > 1 & col.candles) {
        if(Opens[i] < Closes[i] & Opens[i] < Closes[i-1]) bar.col <- "#666666"
        if(Opens[i] < Closes[i] & Opens[i] > Closes[i-1]) bar.col <- "#FFFFFF"
        if(Opens[i] > Closes[i] & Opens[i] < Closes[i-1]) bar.col <- "#FF0000"
        if(Opens[i] > Closes[i] & Opens[i] > Closes[i-1]) bar.col <- "#000000"
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
      if(i > 1 & col.candles) {
        if(Opens[i] < Closes[i] & Opens[i] < Closes[i-1]) bar.col <- "#666666"
        if(Opens[i] < Closes[i] & Opens[i] > Closes[i-1]) bar.col <- "#FFFFFF"
        if(Opens[i] > Closes[i] & Opens[i] < Closes[i-1]) bar.col <- "#FF0000"
        if(Opens[i] > Closes[i] & Opens[i] > Closes[i-1]) bar.col <- "#000000"
        border.col <- "#444444"
      } else {
        bar.col <- ifelse(O.to.C[1] > O.to.C[2],dn.col,up.col)
        border.col <- bar.col
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
    vol.scale <- 100
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
        if(i > 1 & col.candles & color.vol) {
          if(Opens[i] < Closes[i] & Opens[i] < Closes[i-1]) bar.col <- "#666666"
          if(Opens[i] < Closes[i] & Opens[i] > Closes[i-1]) bar.col <- "#FFFFFF"
          if(Opens[i] > Closes[i] & Opens[i] < Closes[i-1]) bar.col <- "#FF0000"
          if(Opens[i] > Closes[i] & Opens[i] > Closes[i-1]) bar.col <- "#000000"
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

# chartSeries.timeSeries {{{
`chartSeries.timeSeries` <-
function(x,
         type=c("auto","candlesticks","matchsticks","bars","line"),
         show.vol=TRUE,
         show.grid=TRUE,name=deparse(substitute(x)),
         time.scale=NULL,
         technicals=NULL,
         line.type="l",
         xlab="time",ylab="price",theme="black",
         up.col,dn.col,color.vol=TRUE
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
`barChart` <- function(x,type='bars',name=deparse(substitute(x)),...)
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
