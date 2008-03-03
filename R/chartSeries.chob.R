# chartSeries.chob {{{
`chartSeries.chob` <-
function(x)
{
  old.par  <- par(c('pty','mar','xpd','bg','xaxs','las','col.axis','fg'))
  on.exit(par(old.par))

  # layout page
  if(x@windows == 1) {
    layout(matrix(1))
  } else {
    layout(matrix(1:x@windows,x@windows,1,byrow=TRUE),
                  1,c(3,rep(1,x@windows-2),1.65),
           respect=FALSE)
    if(x@windows > 1) par(mar=c(0,4,3,3))
  }

  x.range <- 1:(x@xrange[2]*x@spacing)
  y.range <- seq(x@yrange[1],x@yrange[2],length.out=length(x.range))
 
  # get current values of series to be charted
  xx <- eval(x@passed.args$x)
  xx <- x@xdata

#  if(!is.xts(xx)) xx <- as.xts(xx)
 
  xx <- xx[x@xsubset]

  if(is.OHLC(xx)) {
    Opens <- as.numeric(Op(xx))
    Highs <- as.numeric(Hi(xx))
    Lows <- as.numeric(Lo(xx))
    Closes <- as.numeric(Cl(xx))
  } else {
    # if not OHLC, assume univariate series
    Lows <- min(xx[,1])
    Highs <- max(xx[,1])
    Closes <- as.numeric(xx[,1])
  }

  par(bg=x@colors$bg.col,col.axis=x@colors$fg.col,
      xaxs='r',las=2,fg=x@colors$fg.col)

  # create scale of main plot window
  plot(x.range,y.range,type='n',axes=FALSE,ann=FALSE)

  # check for any underlay TA indicators that need to be drawn here:
  main.key <- list() # main.key stores text to be added after all drawing by text()
  if (length(x@passed.args$TA) > 0) {
    underlay.TA <- which(sapply(x@passed.args$TA,
                         function(x) identical(x@on, as.numeric(-1))))
    for (j in underlay.TA) {
      tmp.x <- x@passed.args$TA[[j]]
      main.key <- c(main.key,do.call(x@passed.args$TA[[j]]@name, list(tmp.x)))
    }
  }

  # add gridlines _under_ main series
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
      # create vector of appropriate bar colors
      bar.col <- ifelse(Opens < Closes,
                        ifelse(Opens > last.Closes,
                               x@colors$dn.up.col,
                               x@colors$up.up.col),
                        ifelse(Opens < last.Closes,
                               x@colors$dn.dn.col,
                               x@colors$up.dn.col))
      # create vector of appropriate border colors
      bar.border <- ifelse(Opens < Closes,
                           ifelse(Opens > last.Closes,
                                  x@colors$dn.up.border,
                                  x@colors$up.up.border),
                           ifelse(Opens < last.Closes,
                                  x@colors$dn.dn.border,
                                  x@colors$up.dn.border))
    } else {
      bar.col <- ifelse(Opens < Closes,x@colors$up.col,x@colors$dn.col)
      bar.border <- ifelse(Opens < Closes,x@colors$up.border,x@colors$dn.border)
    }
    if(x@type %in% c('candlesticks','matchsticks')) {
      # draw HL lines

# in progress thinking...
# if subsetting is allowed, keep x.pos based on length of output
#  _but_ subset the y-values with the index corresponding to the
# desired sub-range
#
# most likely found with something like:
#  sindex <- which(index(x) %in% index(x)[subset])
#
#  the default subset should be set to :: (the whole thing...)
#
#  so the following would read:
#     segments(x.pos,Lows[sindex],x.pos,Highs[sindex],col=bar.border)

      segments(x.pos,Lows,x.pos,Highs,col=bar.border)

      # draw OC candles
      if(x@type=='candlesticks') {
        rect(x.pos-x@spacing/3,Opens,x.pos+x@spacing/3,Closes,
             col=bar.col,border=bar.border)
      } else segments(x.pos,Opens,x.pos,Closes,col=bar.col)
    } else {  # draw HLC or OHLC bars
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
  do.call('title',list(x@name,col.main=x@colors$main.col))

  # TA calculation and drawing loops
  if(x@windows > 1 | length(x@passed.args$TA) > 0) {

    for(i in 1:x@windows) {
      # draw all overlays needed for figure 'i' on plot
      overlay.TA <- which(sapply(x@passed.args$TA,function(x) identical(x@on,as.numeric(i))))
      for(j in overlay.TA) {
        # call draws TA and returns the text to add to the chart
        overlay.text <- do.call(x@passed.args$TA[[j]]@name,list(x@passed.args$TA[[j]]))
        main.key <- c(main.key,overlay.text)
        #main.key <- lapply(list(main.key,overlay.text),unlist)
        #main.key[[length(main.key)+1]] <- overlay.text
      }

      if(i == 1) {
        # add indicator key to main chart
        if(length(main.key) > 0) {
          for(indicator in 1:length(main.key)) {
            text(0,max(Closes,na.rm=TRUE),
                 paste(paste(rep("\n\n\n",indicator),collapse=''),main.key[[indicator]][['text']],sep=''),
                 pos=4, col=main.key[[indicator]][['col']]
                )
          }
        }
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
  #title(xlab=x@colors$time.scale,col.lab=x@colors$fg.col)
  if(x@minor.ticks)
    axis(1,at=1:x@length*x@spacing+1,labels=FALSE,col=x@colors$minor.tick)
 
  axis(1,at=x@bp*x@spacing+1,labels=x@x.labels,las=1,lwd=1,mgp=c(3,2,0),
       col=x@colors$major.tick)


  # resave new chob object - just in case of any changes
  write.chob(x,dev.cur())
  # reset layout of page
  layout(matrix(1))
}#}}}
