# chartSeries.chob {{{
`chartSeries.chob` <-
function(x)
{
  old.par  <- par(c('pty','mar','xpd','bg','xaxs','las','col.axis','fg'))
  on.exit(par(old.par))

  LAYOUT <- ifelse(is.null(x@layout),FALSE,TRUE)

  par.list <- list(list(mar=c(  0,3.5,2,3)),
                   list(mar=c(  0,3.5,0,3)),
                   list(mar=c(3.5,3.5,0,3)))
 
  # layout page
  if(LAYOUT) {
    if(!inherits(x@layout,'chart.layout')) {
      cl <- chart.layout(x@windows)
    } else cl <- x@layout
    
    layout(cl$mat, cl$width, cl$height, respect=FALSE)
  }
  if(x@windows > 1) {
    do.call('par',par.list[[1]]) 
  } else par(mar=c(3.5,3.5,2,3)
  #layout(matrix(c(2,3,4,5,1,1,1,1),nc=4,byrow=TRUE),c(1,1,1,1),c(1,2),FALSE)
  #layout(matrix(c(1,2,1,3,1,4,1,5),nc=2,byrow=TRUE),c(5,1),c(1),FALSE)

  x.range <- 1:(x@xrange[2]*x@spacing)
  y.range <- seq(x@yrange[1],x@yrange[2],length.out=length(x.range))

  log.scale <- ifelse(x@log.scale, 'y', '')
 
  # get current values of series to be charted
  xx <- x@xdata
 
  xx <- xx[x@xsubset]

  if(is.OHLC(xx)) {
    Opens <- as.numeric(Op(xx))
    Highs <- as.numeric(Hi(xx))
    Lows <- as.numeric(Lo(xx))
    Closes <- as.numeric(Cl(xx))
  } else {
    # if not OHLC, assume univariate series
    Lows <- min(xx[,1],na.rm=TRUE)
    Highs <- max(xx[,1],na.rm=TRUE)
    Closes <- as.numeric(xx[,1])
  }

  par(bg=x@colors$bg.col,col.axis=x@colors$fg.col,
      xaxs='r',las=2,fg=x@colors$fg.col)

  # create scale of main plot window
 # plot(x.range,y.range,type='n',axes=FALSE,ann=FALSE)

  plot.new()
  plot.window(xlim=c(1,x@xrange[2]*x@spacing),
              ylim=c(x@yrange[1],x@yrange[2]),
              log=log.scale)

  coords <- par('usr')
  rect(coords[1],coords[3],coords[2],coords[4],col=x@colors$area)

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
  #grid(NA,NULL,col=x@colors$grid.col)
  abline(h=axTicks(2), col=x@colors$grid.col)

  # a vector of x positions
  x.pos <- 1+x@spacing*(1:x@length-1)


  if(x@type=='line') {
    lines(x.pos,Closes,col=x@colors$up.col,type=x@line.type)
    main.key <- c(list(list(legend=
                       paste('Last',last(Closes)),
                       text.col=x@colors$up.col)),main.key)
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
    main.key <- c(list(list(legend=
                       paste('Last',last(Closes)),
                       text.col=last(bar.col))),main.key)
  }

  axis(2)
  box(col=x@colors$fg.col)

  old.adj <- par('adj')
  par('adj'=0)
  do.call('title',list(x@name, col.main=x@colors$fg.col))
  par('adj'=1)
  do.call('title',list(paste('[',start(xx),'/',end(xx),']', sep='')
                      ,col.main=x@colors$main.col))
  par('adj'=old.adj)

  # TA calculation and drawing loops
  if(x@windows > 1 | length(x@passed.args$TA) > 0) {

    for(i in 1:x@windows) {
      # draw all overlays needed for figure 'i' on plot
      overlay.TA <- which(sapply(x@passed.args$TA,function(x) identical(x@on,as.numeric(i))))
      for(j in overlay.TA) {
        # call draws TA and returns the text to add to the chart
        overlay.text <- do.call(x@passed.args$TA[[j]]@name,list(x@passed.args$TA[[j]]))
        main.key <- c(main.key,overlay.text)
      }

      if(i == 1) {
        # add indicator key to main chart
        if(length(main.key) > 0) {
          for(indicator in 1:length(main.key)) {
            legend("topleft",
                   legend=c(rep('',indicator-1), paste(main.key[[indicator]][["legend"]],collapse="")),
                   text.col=rev(main.key[[indicator]][["text.col"]])[1], bty='n', y.inter=0.95)
          }
        }
      }

      if(x@windows >= i+1) {
        # if there are more windows to draw...draw the next one
        next.new.TA <- which(sapply(x@passed.args$TA,function(x) x@new))[i]
        do.call('par',par.list[[2]]) #par(mar=c(0,4,0,3))
        if(x@windows == i+1) do.call('par',par.list[[3]]) #par(mar=c(4,4,0,3))
#  coords <- par('usr')
#  rect(coords[1],coords[3],coords[2],coords[4],col=x@colors$area)
        do.call(x@passed.args$TA[[next.new.TA]]@name,list(x@passed.args$TA[[next.new.TA]]))
      }
    }

  }

  # draw the final x labels
  if(x@minor.ticks)
    axis(1,at=x.pos,labels=FALSE,col=x@colors$minor.tick)
 
  axis(1,at=1+x@bp*x@spacing-x@spacing,labels=x@x.labels,las=1,lwd=1,mgp=c(3,2,0),
       col=x@colors$major.tick)


  # resave new chob object - just in case of any changes
  write.chob(x,dev.cur())
  # reset layout of page
  if(LAYOUT) layout(matrix(1))
}#}}}

# chart.layout {{{
`chart.layout` <-
function(x) {
  if(x==1) {
    lyt <- 'layout(matrix(1))'
    mat <- matrix(1)
    wd  <- 1
    ht  <- 1
  } else {
    lyt <- paste('layout(matrix(c(1,1:',x,'),',x+1,',1,byrow=TRUE),',
                 '1,1,respect=FALSE)',sep='')
                 #'1,c(3,rep(1,',x-2,'),1.60),respect=FALSE)',sep='')
    mat <- matrix(1:x,x,1,byrow=TRUE)
    wd  <- 1
    ht  <- c(3,rep(1,x-2),1.60)
  }
  par.list <- list(list(mar=c(  0,3.5,2,3)),
                   list(mar=c(  0,3.5,0,3)),
                   list(mar=c(3.5,3.5,0,3)))
  structure(list(text=lyt,mat=mat,width=wd,height=ht,par.list=par.list), class='chart.layout')
}
#}}}

# experimental {{{
#`doCharts` <- function(W, TA, nc) {
#  chartLayout(W,TA,nc)
#  for(i in 1:x) barChart(GS, subset='2008', layout=NULL)
#}
#
#`chartLayout` <- function(W=1, TA=1, nc=1) {
# x <- matrix(rep(c(1,1,seq(2,length.out=TA)),W) +
#              rep(seq(0,by=TA+1, length.out=W), each=TA+2),
#              nc=nc, byrow=FALSE)
# layout(x,1,1,respect=FALSE)
#}
#
#`dozenCharts` <- function(W,TA , nc) {
#  getSymbols("GS")
#  chartLayout(W,TA,nc)
#  TAs <- paste('addVo();addMACD();addRSI();addSMI();addROC();addDPO()',
#               'addADX();addATR();addCMF();addCCI();addCMO();addWPR()',sep=';')
#  TAs <- unlist(strsplit(TAs,';'))
#  Overlays <- paste('addEMA();addBBands();addEnvelope()',
#                     'addExpiry();addSAR();addSMA()',sep=';')
#  Overlays <- rep(unlist(strsplit(Overlays,';')),2)
#   
#  for(i in 1:W) {
#    TA <- paste(TAs[i],Overlays[i],sep=';')
#    candleChart(GS, theme='white', subset='2008', type='b', layout=NULL, TA=TA)
#  }
#} #}}}
