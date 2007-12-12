`chartSeries.chob` <-
function(x)
{
  # layout page
  if(x@windows == 1) {
    layout(matrix(1))
  } else {
    layout(matrix(1:x@windows,x@windows,1,byrow=TRUE),1,c(3,rep(1,x@windows-2),1.45),respect=FALSE)
  }

  x.range <- 1:(x@xrange[2]*x@spacing)
  y.range <- seq(x@yrange[1],x@yrange[2],length.out=length(x.range))

  Opens <- as.numeric(Op(eval(x@passed.args$x)))
  Highs <- as.numeric(Hi(eval(x@passed.args$x)))
  Lows <- as.numeric(Lo(eval(x@passed.args$x)))
  Closes <- as.numeric(Cl(eval(x@passed.args$x)))

  old.par  <- par(c('pty','mar','xpd','bg','xaxs','las','col.axis','fg'))
  on.exit(par(old.par))

  par(bg=x@colors$bg.col,col.axis=x@colors$fg.col,
      xaxs='r',las=2,fg="#BBBBBB")
  if(x@windows > 1) par(mar=c(0,4,3,3))
  plot(x.range,y.range,type='n',axes=FALSE,ann=FALSE)

  chart = "candlesticks"
  grid(NA,NULL,col=x@colors$grid.col)

  # a vector of x positions
  x.pos <- 1+x@spacing*(1:x@length-1)


  # create a vector of colors
  if(x@multi.col) {
    last.Closes <- as.numeric(quantmod::Lag(Closes))
    last.Closes[1] <- Closes[1]
    bar.col <- ifelse(Opens < Closes,
                      ifelse(Opens > last.Closes,
                             x@colors$dn.up.col,
                             x@colors$up.up.col),
                      ifelse(Opens < last.Closes,
                             x@colors$dn.dn.col,
                             x@colors$up.dn.col))
  } else {
    bar.col <- ifelse(Opens < Closes,x@colors$up.col,x@colors$dn.col)
  }

  # draw HL lines
  rect(x.pos,Lows,x.pos,Highs,border="#666666")

  # draw OC candles
  rect(x.pos-x@spacing/5,Opens,x.pos+x@spacing/5,Closes,
       col=bar.col,border=bar.col)
  
  axis(2)
  box(col=x@colors$fg.col)

  # TA calculation and drawing loops
  if(x@windows > 1 | length(x@passed.args$TA) > 0) {
    for(i in 1:x@windows) {
      # draw all overlays needed for figure 'i' on plot
      overlay.TA <- which(sapply(x@passed.args$TA,function(x) identical(x@on,as.numeric(i))))
      for(j in overlay.TA) {
        do.call(x@passed.args$TA[[j]]@name,list(x@passed.args$TA[[j]]))
      }
      if(x@windows >= i+1) {
        # if there are more windows to draw...draw the next one
        next.new.TA <- which(sapply(x@passed.args$TA,function(x) x@new))[i]
        par(mar=c(0,4,0,3))
        if(x@windows == i+1) par(mar=c(4,4,0,3))
        do.call(x@passed.args$TA[[next.new.TA]]@name,list(x@passed.args$TA[[next.new.TA]]))
      }
    }
  }

  # draw the final x labels
  title(xlab=x@colors$time.scale,col.lab=x@colors$fg.col)
  axis(1,at=1:x@length*x@spacing+1,labels=FALSE,col="#333333")
  axis(1,at=x@bp*x@spacing+1,labels=x@x.labels,las=1,lwd=1,mgp=c(3,2,0))

  # resave new chob object - just in case of any changes
  write.chob(x,dev.cur())
  # reset layout of page
  layout(matrix(1))
}
