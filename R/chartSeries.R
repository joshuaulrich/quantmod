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

  # determine formatting for time-axis
  #for(time.period in c('years','months','weeks','days','minutes')) {
#    time.choices <- list(yearly='years',monthly='months',
#                         weekly='weeks',daily='days',
#                         hourly='hours',minute='minutes')
#    time.period <- time.choices[[time.scale]]
#    bp <- breakpoints(x,by=time.period,TRUE)
#  #  if(length(bp) > 3) {
#      x.labels <- format(index(x)[bp+1],"%b %y")
#      if(time.period=='weeks' | time.period=='days')
#        x.labels <- format(index(x)[bp+1],"%b %d %Y")
#      if(time.period=='minutes')
#        x.labels <- format(index(x)[bp+1],"%H:%M")
#  #    break
#  #  }
#  #}
  ticks <- function(x,gt=2,lt=20) {
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

# chartVo {{{
`chartVo` <-
function(x) {
  # if volume is to be plotted, do so here
    # scale volume - vol.divisor
    if(class(x) != "chobTA") stop("chartVo requires a suitable chobTA object")
    Opens <- x@TA.values[,1]
    Closes <- x@TA.values[,2]
    Volumes <- x@TA.values[,3]

    spacing <- x@params$spacing
    width <- x@params$width

    x.range <- x@params$xrange
    x.range <- seq(x.range[1],x.range[2]*spacing)

    multi.col <- x@params$multi.col
    color.vol <- x@params$color.vol

    vol.scale <- x@params$vol.scale
    #par(mar=c(2,4,0,3))
    plot(x.range,seq(min(Volumes),max(Volumes),
         length.out=length(x.range)),
         type='n',axes=FALSE,ann=FALSE)
    grid(NA,NULL,col="#333333")
    bar.col <- x@params$colors$fg.col
    for(i in 1:length(Volumes)) {
      Vols <- c(0,Volumes[i])
      x.pos <- 1+spacing*(i-1)
      if(x@params$color.vol) {
        dn.up.col <- x@params$colors$dn.up.col
        up.up.col <- x@params$colors$up.up.col
        dn.dn.col <- x@params$colors$dn.dn.col
        up.dn.col <- x@params$colors$up.dn.col
        up.col <- x@params$colors$up.col
        dn.col <- x@params$colors$dn.col

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
      border.col <- ifelse(x@params$multi.col,"#000000",bar.col)
      rect(x.pos-spacing/4,0,x.pos+spacing/4,Vols[2],col=bar.col,border=border.col)
    }
    title(ylab=paste("volume (",vol.scale[[2]],")"))
    #title(ylab=paste("volume (",vol.scale[[2]],")"),
    #      xlab=x@params$colors$time.scale,col.lab=x@params$colors$fg.col)
    #axis(1,at=1:length(Volumes)*spacing+1,labels=FALSE,col="#444444")
    #axis(1,at=x@params$bp*spacing+1,labels=x@params$x.labels,las=1)
    axis(2)
    box(col=x@params$colors$fg.col)
} # }}}

# chartSMI {{{
`chartSMI` <-
function(x) {
  # if volume is to be plotted, do so here
    # scale volume - vol.divisor
    Highs <- x@TA.values[,1]
    Lows <- x@TA.values[,2]
    Closes <- x@TA.values[,3]

    spacing <- x@params$spacing
    width <- x@params$width

    x.range <- x@params$xrange
    x.range <- seq(x.range[1],x.range[2]*spacing)

    multi.col <- x@params$multi.col
    color.vol <- x@params$color.vol

    param <- x@params$param; ma.type <- x@params$ma.type
    #par(mar=c(2,4,0,3))
    smi <- SMI(cbind(Highs,Lows,Closes),n=param[1],ma.slow=list(ma.type[1],n=param[2]),
               ma.fast=list(ma.type[2],n=param[3]),ma.sig=list(ma.type[3],n=param[4]))
    plot(x.range,seq(min(smi[,1]*.975),max(smi[,1]*1.05),length.out=length(x.range)),
         type='n',axes=FALSE,ann=FALSE)
    grid(NA,NULL,col="#333333")
    lines(seq(1,length(x.range),by=spacing),smi[,1],col='#0033CC',lwd=2,type='l')
    lines(seq(1,length(x.range),by=spacing),smi[,2],col='#BFCFFF',lwd=1,lty='dotted',type='l')
    title(ylab=paste('SMI(',paste(param,collapse=','),')',sep=''))
    axis(2)
    box(col=x@params$colors$fg.col)
} # }}}

# addVo {{{
`addVo` <- function() {
  if(exists('chob',env=sys.frames()[[1]])) {
    if(identical(sys.frames()[[1]],.GlobalEnv)) stop('only chob in GlobalEnv!')
    lchob <- get('chob',env=sys.frames()[[1]])
  } else {
    gchob <- get.chob()
    #protect against NULL device or windows not drawn to yet
    if(dev.cur()==1 || length(gchob) < dev.cur()) {
      # create a default chob to work with: must match requirements of call
      return(invisible(NULL))
    } else {
    #which is the current device; is it in the chob list?
    #current.chob <- which(unlist(sapply(gchob,
    #                             function(x) {
    #                               if(!is.null(x)) x@device==as.numeric(dev.cur())
    #                             })))+1
    current.chob <- which(sapply(gchob,
                                 function(x) {
                                   ifelse(class(x)=="chob" &&
                                   x@device==as.numeric(dev.cur()),TRUE,FALSE)
                                 }))
    if(identical(current.chob,integer(0))) stop("no current plot")
    lchob <- gchob[[current.chob]]
    }
  }
  x <- as.matrix(eval(lchob@passed.args$x))
  Volumes <- x[,5]
  max.vol <- max(Volumes)
  vol.scale <- list(100, "100s")
  if (max.vol > 10000) 
    vol.scale <- list(1000, "1000s")
  if (max.vol > 1e+05) 
    vol.scale <- list(10000, "10,000s")
  if (max.vol > 1e+06) 
    vol.scale <- list(1e+05, "100,000s")
  if (max.vol > 1e+07) 
    vol.scale <- list(1e+06, "millions")

  chobTA <- new("chobTA")
  chobTA@new <- TRUE

  chobTA@TA.values <- cbind(x[,c(1,4)],Volumes/vol.scale[[1]])
  chobTA@name <- "chartVo"
  chobTA@call <- match.call()
  chobTA@params <- list(xrange=lchob@xrange,
                        colors=lchob@colors,
                        color.vol=lchob@color.vol,
                        multi.col=lchob@multi.col,
                        spacing=lchob@spacing,
                        width=lchob@width,
                        bp=lchob@bp,
                        vol.scale=vol.scale,
                        x.labels=lchob@x.labels,
                        time.scale=lchob@time.scale)
  return(chobTA)
} # }}}

# addSMI {{{
`addSMI` <- function(param=c(5,3,3,3),ma.type=c('EMA','EMA','EMA')) {
  if(exists('chob',env=sys.frames()[[1]])) {
    if(identical(sys.frames()[[1]],.GlobalEnv)) stop()
    lchob <- get('chob',env=sys.frames()[[1]])
  } else {
    gchob <- get.chob()
    #protect against NULL device or windows not drawn to yet
    if(dev.cur()==1 || length(gchob) < dev.cur()) stop()
    #which is the current device; is it in the chob list?
    #current.chob <- which(unlist(sapply(gchob,
    #                             function(x) {
    #                               if(!is.null(x)) x@device==as.numeric(dev.cur())
    #                             })))+1
    current.chob <- which(sapply(gchob,
                                 function(x) {
                                   ifelse(class(x)=="chob" &&
                                   x@device==as.numeric(dev.cur()),TRUE,FALSE)
                                 }))
    if(identical(current.chob,integer(0))) stop("no current plot")
    lchob <- gchob[[current.chob]]
  }
  x <- as.matrix(eval(lchob@passed.args$x))
  chobTA <- new("chobTA")
  chobTA@new <- TRUE

  chobTA@TA.values <- x[,2:4] # HLC
  chobTA@name <- "chartSMI"
  chobTA@call <- match.call()
  chobTA@params <- list(xrange=lchob@xrange,
                        colors=lchob@colors,
                        color.vol=lchob@color.vol,
                        multi.col=lchob@multi.col,
                        spacing=lchob@spacing,
                        width=lchob@width,
                        bp=lchob@bp,
                        x.labels=lchob@x.labels,
                        time.scale=lchob@time.scale,
                        param=param,ma.type=ma.type)
  return(chobTA)
} #}}}

# addMA {{{
`addMA` <- function(n=10,wilder=FALSE,from.fig=1,with.col=Cl,overlay=TRUE,col='blue') {
  if(exists('chob',env=sys.frames()[[1]])) {
    if(identical(sys.frames()[[1]],.GlobalEnv)) stop()
    lchob <- get('chob',env=sys.frames()[[1]])
  } else {
    gchob <- get.chob()
    #protect against NULL device or windows not drawn to yet
    if(dev.cur()==1 || length(gchob) < dev.cur()) {
      return(invisible(NULL))
    } else {
    #which is the current device; is it in the chob list?
    #current.chob <- which(unlist(sapply(gchob,
    #                             function(x) {
    #                               if(!is.null(x)) x@device==as.numeric(dev.cur())
    #                             })))+1
    current.chob <- which(sapply(gchob,
                                 function(x) {
                                   ifelse(class(x)=="chob" &&
                                   x@device==as.numeric(dev.cur()),TRUE,FALSE)
                                 }))
    if(identical(current.chob,integer(0))) stop("no current plot")
    lchob <- gchob[[current.chob]]
    }
  }
  chobTA <- new("chobTA")
  chobTA@new <- !overlay

  # get the appropriate data - from the approp. src
  if(from.fig==1) {
    x <- as.matrix(eval(lchob@passed.args$x))
    if(is.function(with.col)) {
      x.tmp <- do.call(with.col,list(x))
    } else x.tmp <- x[,with.col]
  } else {
    # get values from TA...
    which.TA <- which(sapply(lchob@passed.args$TA,function(x) x@new))
    target.TA <- eval(lchob@passed.args$TA[which.TA][from.fig-1])[[1]]
    x <- as.matrix(target.TA@TA.values)
    if(missing(with.col)) {
      warning('missing "with.col" argument')
      invisible(return())
    }
    if(is.function(with.col)) {
      x.tmp <- do.call(with.col,list(x))
    } else x.tmp <- x[,with.col]
  }

  chobTA@TA.values <- x.tmp # single numeric vector
  chobTA@name <- "chartMA"
  chobTA@call <- match.call()
  chobTA@on <- from.fig # used for deciding when to draw...
  chobTA@params <- list(xrange=lchob@xrange,
                        colors=lchob@colors,
                        color.vol=lchob@color.vol,
                        multi.col=lchob@multi.col,
                        spacing=lchob@spacing,
                        width=lchob@width,
                        bp=lchob@bp,
                        x.labels=lchob@x.labels,
                        time.scale=lchob@time.scale,
                        ma.col=col,n=n,wilder=wilder)
  return(chobTA)
} # }}}
# chartMA {{{
`chartMA` <-
function(x) {
  # if volume is to be plotted, do so here
    # scale volume - vol.divisor

    spacing <- x@params$spacing
    width <- x@params$width

    x.range <- x@params$xrange
    x.range <- seq(x.range[1],x.range[2]*spacing)

    multi.col <- x@params$multi.col
    color.vol <- x@params$color.vol

    ma <- EMA(x@TA.values,n=x@params$n,wilder=x@params$wilder)
    if(x@new) {
      par(new=TRUE)
      plot(x.range,seq(min(ma*.975),max(ma*1.05),length.out=length(x.range)),
           type='n',axes=FALSE,ann=FALSE)
      title(ylab=paste('EMA(',paste(x@params$n,collapse=','),')',sep=''))
      axis(2)
      box(col=x@params$colors$fg.col)
    }
    lines(seq(1,length(x.range),by=spacing),ma,col=x@params$ma.col,lwd=1,type='l')
} # }}}

# newChob {{{
`newChob` <-
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
  chob <- new("chob")

  chob@x <- x
  chob@name <- name
  chob@theme <- theme
  chob@type <- type
  chob@line.type <- line.type
  chob@bar.type <- bar.type
  chob@xlab <- xlab
  chob@ylab <- ylab

  if(is.OHLC(x)) {
#     || (is.zoo(x) &
#     all(suppressWarnings(
#        grep("(Op)|(Hi)|(Lo)|(Cl)",colnames(x),ignore.case=TRUE)==1:4)))) {
    chob@Opens <- as.numeric(Op(x))
    chob@Highs <- as.numeric(Hi(x))
    chob@Lows <- as.numeric(Lo(x))
    chob@Closes <- as.numeric(Cl(x))
  } else {
    chob@Lows <- min(x[,1])
    chob@Highs <- max(x[,1])
    chob@Closes <- as.numeric(x[,1])
    chob@type <- "line"
  } 
  if(has.Vo(x)) {
    chob@Volumes <- as.numeric(Vo(x))
    chob@show.vol <- show.vol
  } else chob@show.vol <- FALSE
  
  #if(identical(Volumes,numeric(0))) show.vol <- FALSE
  if(is.null(time.scale)) {
    chob@time.scale <- periodicity(x)$scale
  }
  
  # before messing with graphics, save...
  #old <- par(c('pty','mar','xpd','bg','xaxs','las','col.axis','fg','fig'))
  #on.exit(par(old))

  if(theme=="black") {
    chob@bg.col <- "#222222"
    chob@fg.col <- "#666666"
    chob@up.col <- ifelse(missing(up.col),"#00FF00",up.col)
    chob@dn.col <- ifelse(missing(dn.col),"#FF9900",dn.col)
  }
  if(theme=="white") {
    chob@bg.col <- "#FFFFFF"
    chob@fg.col <- "#444444"
    chob@up.col <- ifelse(missing(up.col),"#00CC00",up.col)
    chob@dn.col <- ifelse(missing(dn.col),"#FF7700",dn.col)
  }
  if(theme=="grey") {
    chob@bg.col <- "#FFFFFF"
    chob@fg.col <- "#444444"
    chob@up.col <- ifelse(missing(up.col),"#FFFFFF",up.col)
    chob@dn.col <- ifelse(missing(dn.col),"#000000",dn.col)
  }
  #if(is.logical(multi.col) | length(multi.col)==4) {
  if(missing(multi.col)) { # interpret as TRUE
    chob@multi.col <- FALSE
  } else {
    if(is.logical(multi.col) && multi.col==TRUE) {
      chob@multi.col <- c("#666666","#FFFFFF",
                     "#FF0000","#000000") 
    }
    # add some check for length 4 colors
    chob@dn.up.col <- multi.col[1]
    chob@up.up.col <- multi.col[2]
    chob@dn.dn.col <- multi.col[3]
    chob@up.dn.col <- multi.col[4]
    chob@up.col <- up.up.col
    chob@dn.col <- dn.dn.col
    chob@multi.col <- TRUE
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

  chob@chart <- chart[1]
  chob@spacing <- spacing
  chob@width <- width

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
  chob@bp <- bp
  chob@x.labels <- x.labels

  chob@xrange <- x.range <- 1:(NROW(x)*spacing)
  chob@yrange <- seq(min(chob@Lows),max(chob@Highs),length.out=length(x.range))

  # determine type of technicals to draw
  # if they need there own plot area, or
  # are overlays.
  # default choice is based upon 
  
  chob@device <- dev.cur()
  chob@call <- match.call(expand=TRUE)
  invisible(chob)
} #}}}

# plot.chob {{{
`plot.chob` <-
function(x,
         type=c("auto","candlesticks","matchsticks","bars","line"),
         show.vol=TRUE,
         show.grid=TRUE,name,
         time.scale=NULL,
         technicals=NULL,
         line.type="l",
         bar.type="ohlc",
         xlab="time",ylab="price",theme="black",
         up.col,dn.col,color.vol=TRUE,multi.col=FALSE,...
         ) {
    Opens <- x@Opens
    Highs <- x@Highs
    Lows <- x@Lows
    Closes <- x@Closes
    Volumes <- x@Volumes
    type <- if(missing(type)) x@type
    name <- if(missing(name)) x@name
    show.vol <- if(missing(show.vol)) x@show.vol
    #show.grid <- if(missing(show.grid)) x@show.grid
    line.type <- if(missing(line.type)) x@line.type
    bar.type <- if(missing(bar.type)) x@bar.type
    theme <- if(missing(theme)) x@theme
    x.labels <- x@x.labels
  #if(identical(Volumes,numeric(0))) show.vol <- FALSE
  if(is.null(time.scale)) {
    time.scale <- x@time.scale
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

  x.range <- x@xrange
  y.range <- x@yrange

  # determine type of technicals to draw
  # if they need there own plot area, or
  # are overlays.
  # default choice is based upon 
  
  if(show.vol) {
    layout(matrix(c(1,2),2,1,byrow=TRUE),1,c(2,1),respect=FALSE)
    par(mar=c(1,4,3,3))
  }
  par(bg=bg.col,col.axis=fg.col,xaxs='r',las=2,fg='#bbbbbb')
  plot(x.range,y.range,type='n',axes=FALSE,ann=FALSE
       #ylim=c(min(Lows),max(Highs)))
       )
  if(show.vol) par(new=TRUE)
  if(show.grid) grid(NA,NULL,col=fg.col)
  if(chart[1]=='line') {
    # plot line
    lines(1:length(Closes),Closes,col='blue',lwd=2,type=line.type)
  } else 
  if(chart[1]=='bars') {
    # plot bars
    tick.size <- ifelse(length(Closes) > 100, 1, 0.5)
    for(i in 1:length(Closes)) {
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
    for(i in 1:length(Closes)) {
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
    axis(1,at=1:length(Closes)*spacing+1,labels=FALSE,col="#444444")
    axis(1,at=x@bp*spacing+1,labels=x.labels,las=1)
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
    plot(x.range,seq(min(Volumes)/vol.scale[[1]],max(Volumes)/vol.scale[[1]],
         length.out=length(x.range)),
         type='n',axes=FALSE,ann=FALSE)
    bar.col <- fg.col
    for(i in 1:length(Volumes)) {
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
    axis(1,at=1:length(Volumes)*spacing+1,labels=FALSE,col="#444444")
    axis(1,at=x@bp*spacing+1,labels=x.labels,las=1)
    axis(2)
    box(col=fg.col)
  } else {
    # add axes label
    title(xlab=time.scale,col.lab=fg.col)
  }
  # eventually return a chart object (a chob, in the spirit of grob)
  # for use in subsequent calls to modify params, add indicators etc...
  layout(matrix(1))
  #chob <- new("chob")
  #chob@device <- dev.cur()
  #chob@call <- match.call(expand=TRUE)
  #chob@xrange <- x.range 
  #chob@yrange <- y.range
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
