"gainloss" <- function(tsn) {
  val.range <- sd(tsn[,1])*3
  ylim <- c(-val.range,val.range)
  oldbg <- par('bg')
  par(bg='#ffffff')
  plot(tsn[,1],type='h',ylim=ylim,col='#dddddd',lwd=2,ylab='', xlab='')
  par(new=T)
  merged.d.f <- merge(abs(subset(tsn,tsn[,1]*tsn[,2]<0)[,1])*-1,abs(subset(tsn,tsn[,1]*tsn[,2]>0)[,1]))
  plot(merged.d.f[,1],type='h',ylim=ylim,col='#ff0000',lwd=2,ylab='', xlab='')
  par(new=T)
  plot(merged.d.f[,2],type='h',ylim=ylim,col='#00ff00',lwd=2,ylab='', xlab='')
  par(bg=oldbg)
}
