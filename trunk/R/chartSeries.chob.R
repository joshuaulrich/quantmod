# chartSeries.chob {{{
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
 
  xx <- eval(x@passed.args$x)
  
  if(is.OHLC(xx)) {
    Opens <- as.numeric(Op(xx))
    Highs <- as.numeric(Hi(xx))
    Lows <- as.numeric(Lo(xx))
    Closes <- as.numeric(Cl(xx))
  } else {
    Lows <- min(xx[,1])
    Highs <- max(xx[,1])
    Closes <- as.numeric(xx[,1])
  }

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

  if(x@type=='line') {
    lines(x.pos,Closes,col='blue',type=x@line.type)
  } else {
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

  
    if(x@type %in% c('candlesticks','matchsticks')) {
      # draw HL lines
      segments(x.pos,Lows,x.pos,Highs,col="#666666")
      # draw OC candles
      if(x@type=='candlesticks') {
        rect(x.pos-x@spacing/3,Opens,x.pos+x@spacing/3,Closes,
             col=bar.col,border="#666666")
      } else segments(x.pos,Opens,x.pos,Closes,col=bar.col)
    } else {  # bars
      # draw vertical HL
      segments(x.pos,Lows,x.pos,Highs,col=bar.col)
      # draw CLOSE notch
      segments(x.pos,Closes,x.pos+x@spacing/6,Closes,col=bar.col)
      # extend CLOSE to left side if HLC, else draw OPEN notch
      if(x@bar.type=='hlc') {
        segments(x.pos-x@spacing/6,Closes,x.pos,Closes,col=bar.col)
      } else segments(x.pos-x@spacing/6,Opens,x.pos,Opens,col=bar.col)
    }    
  }

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
}#}}}
