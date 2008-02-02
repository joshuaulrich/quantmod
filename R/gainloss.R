"plotGainLoss" <- function(x,last.n.days=60) {
  if(is.quantmodResults(x)) x <- modelSignal(x)
  ms <- x[(NROW(x)-last.n.days):NROW(x),]
  plwd <- 3
  val.range <- sd(ms[,1])*3
  ylim <- c(-val.range,val.range)
  oldbg <- par('bg')
  par(bg='#333333')
  par(new=FALSE)
  # plot all returns in bar style
  plot(ms[,1],type='h',ylim=ylim,col='#dddddd',lwd=plwd,ylab='', xlab='',bty='n')
  merged.d.f <- merge(abs(subset(ms,ms[,1]*ms[,2]<0)[,1])*-1,abs(subset(ms,ms[,1]*ms[,2]>0)[,1]))
  par(new=TRUE)
  plot(merged.d.f[,1],type='h',ylim=ylim,col='#ff0000',lwd=plwd,ylab='', xlab='')
  par(new=TRUE)
  plot(merged.d.f[,2],type='h',ylim=ylim,col='#00ff00',lwd=plwd,ylab='', xlab='')
  grid(NA,5,lwd=1,col="#aaaaaa",lty=3)
  par(bg=oldbg)
}
