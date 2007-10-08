# chartSeries {{{
`chartSeries` <-
function(x,
         type=c("auto","candlesticks","matchsticks","bars","line"),
         show.vol=TRUE,
         show.grid=TRUE,name=deparse(substitute(x)),
         time.scale=NULL,
         technicals=NULL,
         line.type="l",
         xlab="time",ylab="price",theme="black"
         ) {
  if(class(x)[1]=="quantmod.OHLC") {
    Highs <- as.numeric(Hi(x))
    Lows <- as.numeric(Lo(x))
    Opens <- as.numeric(Op(x))
    Closes <- as.numeric(Cl(x))
  } else {
    Lows <- min(x)
    Highs <- max(x)
    Closes <- as.numeric(x)
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
  old <- par(c('mar','xpd','bg','col.axis','fg','fig'))
  on.exit(par(old))

  if(theme=="black") {
    bg.col <- "#222222"
    fg.col <- "#AAAAAA"
  }
  if(theme=="white") {
    bg.col <- "#EEEEEE"
    fg.col <- "#444444"
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
    if(NROW(x) > 150) width <- 1
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
  y.range <- seq(min(Lows)*.70,max(Highs)*1.20,length.out=length(x.range))
  if(show.vol) {
    par(fig=c(0,1,0.25,1))
    par(mar=c(1,4,4,2))
  }
  par(bg=bg.col,col.axis=fg.col,fg='#bbbbbb')
  plot(x.range,y.range,type='n',axes=FALSE,ann=FALSE)
  if(show.vol) par(new=TRUE)
  if(show.grid) grid(NA,NULL,col=fg.col)
  if(chart[1]=='line') {
    lines(1:length(Closes),Closes,col='blue',lwd=2,type=line.type)
  } else 
  if(chart[1]=='bars') {
    tick.size <- ifelse(NROW(x) > 100, 1, 0.5)
    for(i in 1:NROW(x)) {
      O.to.C <- c(Opens[i],Closes[i])
      L.to.H <- c(Lows[i],Highs[i])
      x.pos <- 1+spacing*(i-1)
      # create full bar
      lines(c(x.pos,x.pos),L.to.H,
            lwd=width,col=ifelse(O.to.C[1] > O.to.C[2],"#ff9900","green"))
      # create open tick
      segments(x.pos-tick.size,Opens[i],x.pos,Opens[i],
               lwd=width,col=ifelse(O.to.C[1] > O.to.C[2],"#ff9900","green"))
      # create close tick
      segments(x.pos,Closes[i],x.pos+tick.size,Closes[i],
               lwd=width,col=ifelse(O.to.C[1] > O.to.C[2],"#ff9900","green"))
    }
  } else {
    for(i in 1:NROW(x)) {
      O.to.C <- c(Opens[i],Closes[i])
      L.to.H <- c(Lows[i],Highs[i])
      x.pos <- 1+spacing*(i-1)
      lines(c(x.pos,x.pos),L.to.H,lwd=1,col="#666666") # full range grey line
      lines(c(x.pos,x.pos),O.to.C,lwd=width,col=ifelse(O.to.C[1] > O.to.C[2],"#ff9900","green"))
    }
  }
  title(ylab=ylab,col.lab=fg.col)
  title(main=paste(name),col.main=fg.col,font.main=4)
  if(!show.vol) {
    axis(1,at=bp*spacing+1,labels=x.labels)
  }
  axis(2)
  
  if(show.vol) {
  # if volume is to be plotted, do so here
    par(new=TRUE)
    par(fig=c(0,1,0,0.3))
    par(mar=c(5,4,3,2))
    plot(x.range,seq(min(Vo(x))/1000000,max(Vo(x))/1000000,length.out=length(x.range)),
         type='n',axes=FALSE,ann=FALSE)
    for(i in 1:NROW(x)) {
      Vols <- c(0,Volumes[i]/1000000)
      x.pos <- 1+spacing*(i-1)
      lines(c(x.pos,x.pos),Vols,lwd=width,col=fg.col)
    }
    title(ylab="volume (1,000,000's)",
          xlab=time.scale,col.lab=fg.col)
    axis(1,at=bp*spacing+1,labels=x.labels)
    axis(2)
  } else {
    # add axes label
    title(xlab=time.scale,col.lab=fg.col)
  }
  # eventually return a chart object (a chob, in the spirit of grob)
  # for use in subsequent calls to modify params, add indicators etc...
  invisible(1)
} #}}}

# barchart {{{
`barChart` <- function(x,name=deparse(substitute(x)),...)
{
  chartSeries(x=x,type="bars",name=name,...)
} # }}}

# candleChart {{{
`candleChart` <- function(x,name=deparse(substitute(x)),...)
{
  chartSeries(x=x,type="candlesticks",name=name,...)
} # }}}

# matchChart {{{
`matchChart` <- function(x,name=deparse(substitute(x)),...)
{
  chartSeries(x=x,type="matchsticks",name=name,...)
} #}}}

# lineChart {{{
`lineChart` <- function(x,name=deparse(substitute(x)),...)
{
  chartSeries(x=x,type="line",name=name,...)
} # }}}
