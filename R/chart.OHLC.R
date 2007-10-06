`chart.OHLC` <-
function(x,
         type=c("auto","candlesticks","matchsticks","bar","line"),
         plot.volume=TRUE,
         grid.lines=TRUE,name=deparse(substitute(x)),
         xlab="time",ylab="price",bg.col="black"
         ) {
  Highs <- as.numeric(Hi(x))
  Lows <- as.numeric(Lo(x))
  Opens <- as.numeric(Op(x))
  Closes <- as.numeric(Cl(x))
  Volumes <- as.numeric(Vo(x))
  if(identical(Volumes,numeric(0))) plot.volume <- FALSE
  period <- periodicity(x)
  time.scale <- "daily"
  if(period > 2) time.scale <- "weekly" 
  if(period > 7) time.scale <- "monthly" 
  if(period > 31) time.scale <- "yearly" 
  if(bg.col=="black") {
    bg.col <- "#333333"
    fg.col <- "#888888"
  }
  if(bg.col=="white") {
    bg.col <- "#eeeeee"
    fg.col <- "#666666"
  }
  chart.options <- c("auto","candlesticks","matchsticks","line","bar")
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
  } else {
    stop(paste("method",chart,"is currently unimplemented"))
  }
  # calculate x values - years?
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
  old <- par(c('mar','xpd','bg','col.axis','fg','fig'))
  if(plot.volume) {
    par(fig=c(0,1,0.25,1))
    par(mar=c(1,4,4,2))
  }
  par(bg=bg.col,col.axis=fg.col,fg='#bbbbbb')
  plot(x.range,y.range,type='n',axes=FALSE,ann=FALSE)
  if(plot.volume) par(new=TRUE)
  if(grid.lines) grid(NA,NULL,col=fg.col)
  if(chart[1]=='line') {
    lines(1:length(Closes),Closes,col='blue',lwd=3)
  } else {
    for(i in 1:NROW(x)) {
      O.to.C <- c(Opens[i],Closes[i])
      L.to.H <- c(Lows[i],Highs[i])
      x.pos <- 1+spacing*(i-1)
      lines(c(x.pos,x.pos),L.to.H,lwd=1,col="#999999")
      lines(c(x.pos,x.pos),O.to.C,lwd=width,col=ifelse(O.to.C[1] > O.to.C[2],"#ff9900","green"))
    }
  }
  title(ylab=ylab,col.lab=fg.col)
  title(main=paste(name),col.main=fg.col,font.main=4)
  if(!plot.volume) {
    axis(1,at=bp*spacing+1,labels=x.labels)
  }
  axis(2)
  if(plot.volume) {
    par(new=TRUE)
    par(fig=c(0,1,0,0.3))
    par(mar=c(5,4,3,2))
    plot(x.range,seq(min(Vo(x))/1000000,max(Vo(x))/1000000,length.out=length(x.range)),
         type='n',axes=FALSE,ann=FALSE)
    for(i in 1:NROW(x)) {
      Vols <- c(0,Volumes[i]/1000000)
      x.pos <- 1+spacing*(i-1)
      lines(c(x.pos,x.pos),Vols,lwd=width,col="#999999")
    }
    title(ylab="volume (1,000,000's)",col.lab=fg.col)
    axis(1,at=bp*spacing+1,labels=x.labels)
    axis(2)
  }
  par(old)
}

