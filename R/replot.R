findOHLC <- 
function() {
  chob <- current.chob()
  loc <- round(locator(1)$x)
  ohlc <- current.chob()$Env$xdata[current.chob()$Env$xsubset][loc]
  actions <- chob$Env$actions
  envs <- lapply(actions[which(!sapply(actions,attr,'frame')%%2)],attr,'env')
  values <- lapply(lapply(envs[sapply(envs,is.list)],`[[`,1),
                   function(x) x$xdata[chob$Env$xsubset][loc])
  do.call('cbind',c(list(ohlc),values))
}

getSubset <- 
function() {
  chob <- current.chob()
  from <- round(locator(1)$x)
  to <- round(locator(1)$x)
  ohlc <- current.chob()$Env$xdata[current.chob()$Env$xsubset][from:to]
  actions <- chob$Env$actions
  envs <- lapply(actions[which(!sapply(actions,attr,'frame')%%2)],attr,'env')
  values <- lapply(lapply(envs[sapply(envs,is.list)],`[[`,1),
                   function(x) x$xdata[chob$Env$xsubset][from:to])
  c(list(ohlc),values)
}

axTicksByValue <-
function(x,match.to=c(20,10,5,2,1), gt=3) {
  x <- na.omit(x)
  by <- match.to[which(diff(range(x %/% 1)) %/% match.to > gt)[1]]
  do.call('seq.int', as.list(c(range(x)[1]%/%by*by,range(x)[2]%/%by*by,by)))
}

new.replot <- function(frame=1,asp=1,xlim=c(1,10),ylim=list(structure(c(1,10),fixed=FALSE))) {
  # global variables
  Env <- new.env()
  Env$frame <- frame
  Env$asp   <- asp
  Env$usr   <- par("usr")
  Env$xlim  <- xlim
  Env$ylim  <- ylim
  if(length(asp) != length(ylim))
    stop("'ylim' and 'asp' must be the same length")


  # setters
  set_frame <- function(frame,clip=TRUE) { 
    Env$frame <<- frame; 
    set_window(clip); # change actual window
  }
  set_asp   <- function(asp) { Env$asp <<- asp }
  set_xlim  <- function(xlim) { Env$xlim <<- xlim }
  set_ylim  <- function(ylim) { Env$ylim <<- ylim }
  reset_ylim <- function() {
    ylim <- get_ylim()
    ylim <- rep(list(c(Inf,-Inf)),length(ylim))
    #ylim[[1]] <- range(OHLC(Env$xdata)[x]) # main data
    lapply(Env$actions,
           function(x) {
             frame <- attr(x, "frame")
             if(frame > 0) {
               lenv <- attr(x,"env")
               if(is.list(lenv)) lenv <- lenv[[1]]
               ylim[[frame]][1] <<- min(ylim[[frame]][1],range(na.omit(lenv$xdata[Env$xsubset]))[1],na.rm=TRUE)
               ylim[[frame]][2] <<- max(ylim[[frame]][2],range(na.omit(lenv$xdata[Env$xsubset]))[2],na.rm=TRUE)
             }
           })
    # reset all ylim values, by looking for range(env[[1]]$xdata)
    # xdata should be either coming from Env or if lenv lenv
    set_ylim(ylim)
  }

  # getters
  get_frame <- function(frame) { Env$frame }
  get_asp   <- function(asp) { Env$asp }
  get_xlim  <- function(xlim) { Env$xlim }
  get_ylim  <- function(ylim) { Env$ylim }

  # scale ylim based on current frame, and asp values
  scale_ranges <- function(frame, asp, ranges)
  {
    asp/asp[frame] * abs(diff(ranges[[frame]]))
  }
  # set_window prepares window for drawing
  set_window <- function(clip=TRUE,set=TRUE)
  {
    frame <- Env$frame
    frame <- abs(frame)
    asp   <- Env$asp
    xlim  <- Env$xlim
    ylim  <- Env$ylim
    sr <- scale_ranges(frame, asp, ylim)
    if(frame == 1) {
      win <- list(xlim, c((ylim[[frame]][1] - sum(sr[-1])), ylim[[frame]][2]))
    } else
    if(frame == length(ylim)) {
      win <- list(xlim, c(ylim[[frame]][1], ylim[[frame]][2] + sum(sr[-length(sr)])))
    } else {
      win <- list(xlim, c(ylim[[frame]][1] - sum(sr[-(1:frame)]),
                          ylim[[frame]][2] + sum(sr[-(frame:length(sr))])))
    }
    if(!set) return(win)
    do.call("plot.window",win)
    if(clip) clip(par("usr")[1],par("usr")[2],ylim[[frame]][1],ylim[[frame]][2])
  }

  get_actions <- function(frame) {
    actions <- NULL
    for(i in 1:length(Env$actions)) {
      if(abs(attr(Env$actions[[i]],"frame"))==frame)
        actions <- c(actions, Env$actions[i])
    }
    actions
  }

  add_frame <- function(after, ylim=c(0,0), asp=0, fixed=FALSE) {
    if(missing(after))
      after <- max(abs(sapply(Env$actions, function(x) attr(x,"frame"))))
    for(i in 1:length(Env$actions)) {
      cframe <- attr(Env$actions[[i]],"frame")
      if(cframe > 0 && cframe > after)
        attr(Env$actions[[i]], "frame") <- cframe+1L
      if(cframe < 0 && cframe < -after)
        attr(Env$actions[[i]], "frame") <- cframe-1L
    }
    Env$ylim <- append(Env$ylim,list(structure(ylim,fixed=fixed)),after)
    Env$asp  <- append(Env$asp,asp,after)
  }
  remove_frame <- function(frame) {
    rm.frames <- NULL
    max.frame <- max(abs(sapply(Env$actions, function(x) attr(x,"frame"))))
    for(i in 1:length(Env$actions)) {
      cframe <- attr(Env$actions[[i]],"frame")
      if(abs(attr(Env$actions[[i]],"frame"))==frame)
        rm.frames <- c(rm.frames, i)
      if(cframe > 0 && cframe > frame) {
        attr(Env$actions[[i]], "frame") <- cframe-1L
      }
      if(cframe < 0 && cframe < -frame) {
        attr(Env$actions[[i]], "frame") <- cframe+1L
      }
    }
    if(frame > max.frame) {
      Env$frame <- max.frame
    } else Env$frame <- max.frame-1
    Env$ylim <- Env$ylim[-frame]
    Env$asp  <- Env$asp[-frame]
    if(!is.null(rm.frames))
      Env$actions <- Env$actions[-rm.frames]
  }
  next_frame <- function() {
    set_frame(max(abs(sapply(Env$actions,function(x) attr(x,"frame"))))+1L)
  }
  move_frame   <- function() {}

  # actions
  Env$actions <- list()

  # aplot
  add <- replot <- function(x,env=Env,expr=FALSE,clip=TRUE,...) {
    if(!expr) {
      x <- match.call()$x
    } 
    a <- structure(x,frame=Env$frame,clip=clip,env=env)
    Env$actions[[length(Env$actions)+1]] <<- a
  }

  # prepare window to draw
  set_window()
  # return
  structure(list(Env=Env, 
                 set_window=set_window,
                 add=add, replot=replot,
                 get_actions, subset=subset,
                 set_frame=set_frame, get_frame=get_frame, next_frame=next_frame,
                 add_frame=add_frame, remove_frame=remove_frame,
                 set_asp=set_asp, get_asp=get_asp,
                 set_xlim=set_xlim, get_xlim=get_xlim,
                 reset_ylim=reset_ylim, set_ylim=set_ylim, get_ylim=get_ylim),
            class="replot")
}

str.replot <- function(x, ...) {
  print(str(unclass(x)))
}

print.replot <- function(x, ...) plot(x,...)
plot.replot <- function(x, ...) {
  plot.new()
  assign(".chob",x,.GlobalEnv)
  cex <- par(cex=x$Env$cex)
  mar <- par(mar=x$Env$mar)
  if(.Device=="X11") # only reasonable way to fix X11/quartz issue
    par(cex=x$Env$cex * 1.5)
  oxpd <- par(xpd=FALSE)
  usr <- par("usr")
  # plot negative (underlay) actions
  last.frame <- x$get_frame()
  lapply(x$Env$actions,
    function(aob) {
      if(attr(aob,"frame") < 0) {
        x$set_frame(attr(aob,"frame"),attr(aob,"clip"))
        env <- attr(aob,"env")
        if(is.list(env)) {
          # if env is c(env, Env), convert to list
          env <- unlist(lapply(env, function(x) eapply(x, eval)),recursive=FALSE)
        }
        eval(aob, env)
      }
    }
  )
  # plot positive (overlay) actions
  lapply(x$Env$actions,
    function(aob) {
      if(attr(aob,"frame") > 0) {
        x$set_frame(attr(aob,"frame"),attr(aob,"clip"))
        env <- attr(aob,"env")
        if(is.list(env)) {
          env <- unlist(lapply(env, function(x) eapply(x, eval)),recursive=FALSE)
        }
        eval(aob, env)
      }
    }
  )
  for(frames in 1:length(x$get_ylim())) {
    x$set_frame(frames)
    abline(h=x$get_ylim()[[frames]][1], col=x$Env$theme$grid, lwd=1)
  }
  x$set_frame(abs(last.frame),clip=FALSE)
  do.call("clip",as.list(usr))
  par(xpd=oxpd,cex=cex$cex,mar=mar$mar)#,usr=usr)
  invisible(x$Env$actions)
}

scale.ranges <- function(frame, asp, ranges)
{
  asp/asp[frame] * abs(diff(ranges[[frame]]))
}

heikin.ashi.bars <- 
function(x, type="", spacing=1, up.col="green",dn.col="red",up.border="grey",dn.border=up.border) {
  if(is.OHLC(x)) {
    haCloses <- as.xts(apply(OHLC(x),1,sum))/4
    haOpens  <- Op(x)
    haOpens  <- (lag(haOpens) + lag(haCloses))/2
    haHighs  <- as.numeric(as.xts(apply(cbind(Hi(x),haOpens,haCloses),1,max)))
    haLows   <- as.numeric(as.xts(apply(cbind(Lo(x),haOpens,haCloses),1,min)))
    haOpens  <- as.numeric(haOpens)
    haCloses <- as.numeric(haCloses)
  }
  bar.col <- ifelse(haOpens < haCloses, up.col, dn.col)
  bar.border <- ifelse(haOpens < haCloses, up.border, dn.border)
  
  x.pos <- spacing*(1:NROW(x))
  segments(x.pos, haLows, x.pos, apply(cbind(haOpens,haCloses),1,min),col=bar.border)
  segments(x.pos, haHighs, x.pos, apply(cbind(haOpens,haCloses),1,max),col=bar.border)

  if (type == "candlesticks") {
     rect(x.pos - spacing/3, haOpens, x.pos + spacing/3, 
          haCloses, col = bar.col, border = bar.border)
  } else segments(x.pos, haOpens, x.pos, haCloses, col='blue')
  
}

bars <- function(x, spacing=1, bar.col="grey", border.col="darkgrey") {
  x.pos <- spacing*(1:NROW(x))
  rect(x.pos-spacing/3, x, x.pos+spacing/3, rep(0,NROW(x)), col=bar.col, border=border.col)
}


range.bars <-
function(x, type="", spacing=1, up.col="green",dn.col="red",up.border="grey",dn.border=up.border) {
  if(is.OHLC(x) && type != "line") {
    Opens <- as.numeric(Op(x))
    Highs <- as.numeric(Hi(x))
    Lows <- as.numeric(Lo(x))
    Closes <- as.numeric(Cl(x))
    if(type=="heikin.ashi") {
      Closes <- as.xts(apply(OHLC(x),1,sum))/4
      Opens  <- Op(x)
      Opens  <- (lag(Opens) + lag(Closes))/2
      Highs  <- as.numeric(as.xts(apply(cbind(Hi(x),Opens,Closes),1,max)))
      Lows   <- as.numeric(as.xts(apply(cbind(Lo(x),Opens,Closes),1,min)))
      Opens  <- as.numeric(Opens)
      Closes <- as.numeric(Closes)
      type <- "candlesticks"
    }
  } else {
    lines(1:NROW(x),x[,1],lwd=3,col=up.border,lend=3,lty=1)
    return(NULL)
  }
  bar.col <- ifelse(Opens < Closes, up.col, dn.col)
  bar.border <- ifelse(Opens < Closes, up.border, dn.border)
  
  x.pos <- spacing*(1:NROW(x))
  if( type %in% c("ohlc", "hlc")) {
    bar.border <- bar.col
    bar.border[is.na(bar.border)] <- up.border
  }

  segments(x.pos, Lows, x.pos, apply(cbind(Opens,Closes),1,min),col=bar.border,lwd=1.5,lend=3)
  segments(x.pos, Highs, x.pos, apply(cbind(Opens,Closes),1,max),col=bar.border,lwd=1.5,lend=3)

  if (type == "candlesticks") {
     rect(x.pos - spacing/3, Opens, x.pos + spacing/3, 
          Closes, col = bar.col, border = bar.border)
  } else 
  if (type == "matchsticks") {
    bar.col[is.na(bar.col)] <- up.col
    segments(x.pos, Opens, x.pos, Closes, col=bar.col,lwd=1.5,lend=3)
  } else
  if (type == "ohlc") {
    segments(x.pos, Opens, x.pos, Closes, col=bar.border,lwd=1.5,lend=3)
    segments(x.pos-1/3, Opens, x.pos, Opens, col=bar.border,lwd=1.5,lend=3) 
    segments(x.pos, Closes, x.pos+1/3, Closes, col=bar.border,lwd=1.5,lend=3) 
  } else
  if (type == "hlc") {
    segments(x.pos, Opens, x.pos, Closes, col=bar.border,lwd=1.5,lend=3)
    segments(x.pos, Closes, x.pos+1/3, Closes, col=bar.border,lwd=1.5,lend=3) 
  }
  
}

bars <- function(x, spacing=1, bar.col="grey", border.col="darkgrey") {
  x.pos <- spacing*(1:NROW(x))
  rect(x.pos-spacing/3, x, x.pos+spacing/3, rep(0,NROW(x)), col=bar.col, border=border.col)
}

chart_theme <- function() {
  list(dn.col="red",
       up.col=NA,
       dn.border="#333333",
       up.border="#333333",
       format.labels=TRUE,
       grid.ticks.on="months")
}
chart_pars <- function() {
  list(cex=0.6, mar=c(3,1,1,1))
}

chart_Series <- function(x, 
                         name=deparse(substitute(x)), 
                         type="candlesticks", 
                         subset=1:NROW(x), 
                         TA="",
                         pars=chart_pars(), theme=chart_theme()) {
  cs <- new.replot()

  cex <- pars$cex
  mar <- pars$mar

  up.col <- theme$up.col
  dn.col <- theme$dn.col
  up.border <- theme$up.border
  dn.border <- theme$dn.border
  format.labels <- theme$format.labels
  grid.ticks.on <- theme$grid.ticks.on
  
  cs$subset <- function(x) {
    Env$xsubset <<- x
    set_xlim(c(1,length(x)))
    ylim <- get_ylim()
    for(y in seq(2,length(ylim),by=2)) {
      if(!attr(ylim[[y]],'fixed'))
        ylim[[y]] <- structure(c(Inf,-Inf),fixed=FALSE)
    }
    lapply(Env$actions,
           function(x) {
             frame <- abs(attr(x, "frame"))
             fixed <- attr(ylim[[frame]],'fixed')
             #fixed <- attr(x, "fixed")
             if(frame %% 2 == 0 && !fixed) {
               lenv <- attr(x,"env")
               if(is.list(lenv)) lenv <- lenv[[1]]
               min.tmp <- min(ylim[[frame]][1],range(na.omit(lenv$xdata[Env$xsubset]))[1],na.rm=TRUE)
               max.tmp <- max(ylim[[frame]][2],range(na.omit(lenv$xdata[Env$xsubset]))[2],na.rm=TRUE)
               ylim[[frame]] <<- structure(c(min.tmp,max.tmp),fixed=fixed)
             }
           })
    # reset all ylim values, by looking for range(env[[1]]$xdata)
    # xdata should be either coming from Env or if lenv, lenv
    set_ylim(ylim)
  }
  environment(cs$subset) <- environment(cs$get_asp)
  if(is.OHLC(x)) {
    cs$Env$xdata <- OHLC(x)
    if(has.Vo(x))
      cs$Env$vo <- Vo(x)
  } else cs$Env$xdata <- x
  cs$Env$xsubset <- subset
  cs$Env$cex <- cex
  cs$Env$mar <- mar
  cs$set_asp(3)
  cs$set_xlim(c(1,NROW(subset)))
  cs$set_ylim(list(structure(range(cs$Env$xdata[subset]),fixed=FALSE)))
  cs$set_frame(1,FALSE)
  cs$Env$theme$up.col <- up.col
  cs$Env$theme$dn.col <- dn.col
  cs$Env$theme$up.border <- up.border
  cs$Env$theme$dn.border <- dn.border
  cs$Env$theme$bg <- "white"
  cs$Env$theme$grid <- "#d0d0d0"
  cs$Env$theme$grid2 <- "#f5f5f5"
  cs$Env$theme$labels <- "#333333"
  cs$Env$format.labels <- format.labels
  cs$Env$ticks.on <- grid.ticks.on
  cs$Env$type <- type
  #cs$add(box(col=theme$grid))
  #cs$add(abline(v=axTicksByTime(xdata[xsubset],ticks.on='months'), col=theme$grid),clip=FALSE)
  cs$add(segments(axTicksByTime(xdata[xsubset],ticks.on=ticks.on),
                  get_ylim()[[2]][1],
                  axTicksByTime(xdata[xsubset],ticks.on=ticks.on),
                  get_ylim()[[2]][2], col=theme$grid),clip=FALSE)
                  
  cs$set_frame(-1)
  # main title and date range
  cs$add(rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col=theme$bg,border=NA))
  cs$add_frame(0,ylim=c(0,1),asp=0.2)
  cs$set_frame(1)

  cs$add(rect(par("usr")[1],0,par("usr")[2],1,col=theme$grid,border=NA))
  cs$add(axis(1,at=1:length(xsubset),labels=FALSE,col=theme$grid))
  cs$Env$axis_ticks <- function(xdata,xsubset) {
    ticks <- diff(axTicksByTime(xdata[xsubset],"months",labels=FALSE))/2 + 
                  last(axTicksByTime(xdata[xsubset],"months",labels=T,format.labels="%b"),-1)
    if(length(ticks) > 20) ticks <- structure(unname(ticks),.Names=substring(names(ticks),1,1))
    ticks
  }
  cs$add(text(axis_ticks(xdata,xsubset),par('usr')[3]-0.5*min(strheight(axis_ticks(xdata,xsubset))),
              names(axis_ticks(xdata,xsubset)),cex=0.9,pos=3),clip=FALSE)
  cs$add(axis(1,at=axTicksByTime(xdata[xsubset]),
                labels=names(axTicksByTime(xdata[xsubset],format.labels=format.labels)),
                las=1,lwd.ticks=1,mgp=c(3,1.5,0),cex.axis=.9))
  cs$Env$name <- name
  text.exp <- c(expression(text(1-1/3,0.5,name,font=2,col='#444444',offset=0,cex=1.1,pos=4)),
                expression(text(length(xsubset),0.5,paste(start(xdata[xsubset]),end(xdata[xsubset]),sep="/"),col=1,adj=c(0,0),pos=2))
               )
  cs$add(text.exp, env=cs$Env, expr=TRUE)
  cs$set_frame(2)

  cs$Env$axis_labels <- function(xdata,xsubset,scale=5) {
    axTicksByValue(na.omit(xdata[xsubset]))
  }
  cs$add(assign("five",rnorm(10)))  # this gets re-evaled each update, though only to test

  # add $1 gridlines
  cs$set_frame(-2)
  cs$add(abline(h=seq(min(xdata[xsubset]%/%1,na.rm=TRUE),max(xdata[xsubset]%/%1,na.rm=TRUE),1),col=theme$grid2))
  cs$set_frame(2)
  # add $5 gridlines
  cs$add(abline(h=axis_labels(xdata,xsubset),col=theme$grid))
  # add $5 axis labels/boxes
  cs$add(text(1-1/3-max(strwidth(axis_labels(xdata,xsubset))),
              axis_labels(xdata,xsubset), 
              noquote(format(axis_labels(xdata,xsubset),justify="right")), 
              col=theme$labels,offset=0,cex=0.9,pos=4))
  cs$add(text(length(xsubset)+1/3,#-1/3-max(strwidth(axis_labels(xdata,xsubset))),
              axis_labels(xdata,xsubset), 
              noquote(format(axis_labels(xdata,xsubset),justify="right")),
              col=theme$labels,offset=0,cex=0.9,pos=4))
  #cs$add(text(length(xsubset),seq(min(xdata[xsubset]%/%5*5+5,na.rm=TRUE),max(xdata[xsubset]%/%5*5-5,na.rm=TRUE),5),
  #              seq(min(xdata[xsubset]%/%5*5+5,na.rm=TRUE),max(xdata[xsubset]%/%5*5-5,na.rm=TRUE),5),
  #            col=theme$labels,offset=0,cex=0.9,pos=4))
  # add main series
  cs$set_frame(2)
#  if(type=="heikin.ashi") {
#  cs$add(heikin.ashi.bars(xdata[xsubset], type="candlesticks",
#         1,theme$up.col,theme$dn.col,theme$up.border,theme$dn.border))
#  } else {
  cs$add(range.bars(xdata[xsubset], type,
         1,theme$up.col,theme$dn.col,theme$up.border,theme$dn.border))
#  }
  assign(".chob", cs, .GlobalEnv)

  # handle TA="add_Vo()" as we would interactively
  if(nchar(TA) > 0) {
  TA <- parse(text=TA, srcfile=NULL)
  for( ta in 1:length(TA)) {
    if(length(TA[ta][[1]][-1]) > 0) {
    cs <- eval(TA[ta])
    } else {
    cs <- eval(TA[ta])
    }
  }
  }
  assign(".chob", cs, .GlobalEnv)
  cs
}

current.chob <- function() get(".chob",.GlobalEnv)
use.chob <- function(use=TRUE) {
  options('global.chob'=use) 
}

new_ta <- function(FUN, preFUN, postFUN, on=NA, ...) {}

# add_Series {{{
add_Series <- function(x, type="candlesticks",order=NULL, on=NA, legend="auto", ...) { 
  lenv <- new.env()
  lenv$name <- deparse(substitute(x))
  lenv$plot_series <- function(x, series, type, ...) {
    series <- merge(series, x$Env$xdata, join="left",retside=c(TRUE,FALSE))[x$Env$xsubset]
    range.bars(series, type=type)
    #lines(x$Env$xsubset, series, ...)
  }
  lenv$xdata <- x
  # map all passed args (if any) to 'lenv' environment
  mapply(function(name,value) { assign(name,value,envir=lenv) }, 
        names(list(x=x,type=type,order=order,on=on,legend=legend,...)),
              list(x=x,type=type,order=order,on=on,legend=legend,...))
  exp <- parse(text=gsub("list","plot_series",
               as.expression(substitute(list(x=current.chob(),type=type,series=get("x"), ...)))),
               srcfile=NULL)
  plot_object <- current.chob()
  xdata <- plot_object$Env$xdata
  xsubset <- plot_object$Env$xsubset
  tav <- merge(x, xdata, join="left",retside=c(TRUE,FALSE))
  lenv$xdata <- x
  x <- x[xsubset]

  if(is.na(on)) {
    plot_object$add_frame(ylim=c(0,1),asp=0.15)
    plot_object$next_frame()
    text.exp <- expression(text(c(0,
                                  0+strwidth(paste(name,sep="")),
                                  0+strwidth(paste(name,sep=""))+strwidth("5.55555")+1),
                         0.5,
                         name,
                         col=c(1),adj=c(0,0),cex=0.9,offset=0,pos=4))
    plot_object$add(rect(par("usr")[1],0,par("usr")[2],1,col=theme$grid,border=NA))
    plot_object$add(text.exp, env=c(lenv,plot_object$Env), expr=TRUE)

    plot_object$add_frame(ylim=range(na.omit(OHLC(x))),asp=1)  # need to have a value set for ylim
    plot_object$next_frame()
    lenv$grid_lines <- function(xdata,x) { seq(-1,1) }
    # add grid lines
    exp <- c(expression(abline(h=grid_lines(xdata,xsubset),col=theme$grid)),
    # add axis labels/boxes
             expression(text(0,grid_lines(xdata,xsubset),
                        sprintf("%+d",grid_lines(xdata,xsubset)),
                        col=theme$labels,pos=2)),
             expression(text(length(xsubset),grid_lines(xdata,xsubset),
                        sprintf("%+d",grid_lines(xdata,xsubset)),
                        col=theme$labels,pos=4)),exp)
  } else { plot_object$set_frame(sign(on)*(abs(on)+1L)) }


  plot_object$add(exp,env=c(lenv, plot_object$Env),expr=TRUE)
  plot_object
} #}}}

# add_EMA {{{
add_EMA <- function(n=10, on=1, ...) {
  lenv <- new.env()
  lenv$add_ema <- function(x, n, ...) {
    xdata <- x$Env$xdata
    xsubset <- x$Env$xsubset
    ema <- EMA(Cl(xdata), n=n)[xsubset]
    lines(1:NROW(xdata[xsubset]), ema, ...)
  }
  mapply(function(name,value) { assign(name,value,envir=lenv) }, names(list(n=n,...)), list(n=n,...))
  exp <- parse(text=gsub("list","add_ema",as.expression(substitute(list(x=current.chob(),n=n,...)))),
               srcfile=NULL)
  plot_object <- current.chob()
  lenv$xdata <- EMA(Cl(plot_object$Env$xdata),n=n)
  plot_object$set_frame(sign(on)*abs(on)+1L)
  plot_object$add(exp,env=c(lenv, plot_object$Env),expr=TRUE)
  plot_object
} # }}}

# add_SMA {{{
add_SMA <- function(n=10, on=1, ...) {
  lenv <- new.env()
  lenv$add_sma <- function(x, n, ...) {
    xdata <- x$Env$xdata
    xsubset <- x$Env$xsubset
    ema <- SMA(Cl(xdata), n=n)[xsubset]
    lines(1:NROW(xdata[xsubset]), ema, ...)
  }
  mapply(function(name,value) { assign(name,value,envir=lenv) }, names(list(n=n,...)), list(n=n,...))
  exp <- parse(text=gsub("list","add_sma",as.expression(substitute(list(x=current.chob(),n=n,...)))),
               srcfile=NULL)
  plot_object <- current.chob()
  lenv$xdata <- SMA(Cl(plot_object$Env$xdata),n=n)
  plot_object$set_frame(sign(on)*abs(on)+1L)
  plot_object$add(exp,env=c(lenv, plot_object$Env),expr=TRUE)
  plot_object
} # }}}

# add_SMI {{{
add_SMI <- function (n=13, nFast=25, nSlow=2, nSig=9, maType="EMA", bounded=TRUE,...) {
  lenv <- new.env()
  lenv$plot_smi <- function(x, n, nFast, nSlow, nSig, maType, bounded, ...) {
    xdata <- x$Env$xdata
    xsubset <- x$Env$xsubset
    smi <- SMI(HLC(xdata),n=n,nFast=nFast,nSlow=nSlow,nSig=nSig,
               maType=maType,bounded=bounded)
    x.pos <- 1:length(xsubset)
    segments(axTicksByTime(xdata[xsubset],ticks.on=x$Env$ticks.on),
             range(na.omit(smi))[1], 
             axTicksByTime(xdata[xsubset],ticks.on=x$Env$ticks.on),
             range(na.omit(smi))[2], col=x$Env$theme$grid)
    lines(x.pos, smi[xsubset,1], col=x$Env$theme$smi$col$smi, lwd=2,...) 
    lines(x.pos, smi[xsubset,2], col=x$Env$theme$smi$col$signal,  ...) 
  }
  mapply(function(name,value) { assign(name,value,envir=lenv) }, 
        names(list(n=n,nFast=nFast,nSlow=nSlow,nSig=nSig,maType=maType,bounded=bounded,...)),
              list(n=n,nFast=nFast,nSlow=nSlow,nSig=nSig,maType=maType,bounded=bounded,...))
  exp <- parse(text=gsub("list","plot_smi",
               as.expression(substitute(list(x=current.chob(),
                                             n=n,nFast=nFast,
                                             nSlow=nSlow,nSig=nSig,
                                             maType=maType,bounded=bounded,...)))),
               srcfile=NULL)

  plot_object <- current.chob()
  if(is.null(plot_object$Env$theme$smi)) {
    plot_object$Env$theme$smi$col$smi   <- "orange"
    plot_object$Env$theme$smi$col$signal <- "darkgrey"
  }
  xsubset <- plot_object$Env$xsubset
  smi <- SMI(HLC(plot_object$Env$xdata),n=n,nFast=nFast,nSlow=nSlow,nSig=nSig,
                    maType=maType,bounded=bounded)
  plot_object$add_frame(ylim=c(0,1),asp=0.15)
  plot_object$next_frame()
  lenv$xdata <- structure(smi,.Dimnames=list(NULL, c("smi","signal")))
  text.exp <- expression(text(c(0,
                                0+strwidth(paste("SMI(",paste(n,nFast,nSlow,nSig,sep=","),"):",sep="")),
                                0+strwidth(paste("SMI(",paste(n,nFast,nSlow,nSig,sep=","),"):",sep=""))+strwidth("5.55555")+1),
                       0.5,
                       c(paste("SMI(",paste(n,nFast,nSlow,nSig,sep=","),"):",sep=""),
                         round(last(xdata[xsubset,1]),5),
                         round(last(xdata[xsubset,2]),5)),
                       col=c(1,theme$smi$col$smi,theme$smi$col$signal),adj=c(0,0),cex=0.9,offset=0,pos=4))
  plot_object$add(rect(par("usr")[1],0,par("usr")[2],1,col=theme$grid,border=NA))
  plot_object$add(text.exp, env=c(lenv,plot_object$Env), expr=TRUE)

  plot_object$add_frame(ylim=range(na.omit(smi)),fixed=TRUE ,asp=1)
  plot_object$next_frame()

  # add grid lines
  lenv$grid_lines <- function(xdata,x) { seq(-50,50,50) }
  exp <- c(expression(abline(h=grid_lines(xdata,xsubset),col=theme$grid)), exp,
  # add axis labels/boxes
           expression(text(1-1/3-max(strwidth(grid_lines(xdata,xsubset))),grid_lines(xdata,xsubset),
                      noquote(format(grid_lines(xdata,xsubset),justify="right")),
                      col=theme$labels,offset=0,pos=4,cex=0.9)),
           expression(text(length(xsubset)+1/3,grid_lines(xdata,xsubset),
                      noquote(format(grid_lines(xdata,xsubset),justify="right")),
                      col=theme$labels,offset=0,pos=4,cex=0.9)))
#  exp <- c(expression(abline(h=grid_lines(xdata,xsubset),col=theme$grid)),
#  # add axis labels/boxes
#           expression(text(0,grid_lines(xdata,xsubset),
#                      sprintf("%+d",grid_lines(xdata,xsubset)),
#                      col=theme$labels,offset=0,pos=2)),
#           expression(text(length(xsubset),grid_lines(xdata,xsubset),
#                      sprintf("%+d",grid_lines(xdata,xsubset)),
#                      col=theme$labels,offset=0,pos=4)),exp)
  plot_object$add(exp,env=c(lenv, plot_object$Env),expr=TRUE)
  plot_object
} # }}}

# add_RSI {{{
add_RSI <- function (n=14, maType="EMA", ...) {
  lenv <- new.env()
  lenv$plot_rsi <- function(x, n, maType, ...) {
    xdata <- x$Env$xdata
    xsubset <- x$Env$xsubset
    rsi <- RSI(Cl(xdata),n=n,maType=maType)[xsubset]
    x.pos <- 1:NROW(rsi)
    theme <- x$Env$theme$rsi
    segments(axTicksByTime(xdata[xsubset],ticks.on=x$Env$ticks.on),
             range(na.omit(rsi))[1], 
             axTicksByTime(xdata[xsubset],ticks.on=x$Env$ticks.on),
             range(na.omit(rsi))[2], col=x$Env$theme$grid)
    lines(x.pos, rep(30,length(x.pos)), col=theme$col$lines, lwd=2,lty=2,lend=2,...) 
    lines(x.pos, rep(70,length(x.pos)), col=theme$col$lines, lwd=2,lty=2,lend=2,...) 
    lines(x.pos, rsi[,1], col=x$Env$theme$rsi$col$rsi, lwd=2.5,...) 
  }
  mapply(function(name,value) { assign(name,value,envir=lenv) }, 
        names(list(n=n,maType=maType,...)),
              list(n=n,maType=maType,...))
  exp <- parse(text=gsub("list","plot_rsi",
               as.expression(substitute(list(x=current.chob(),
                                             n=n,maType=maType,...)))),
               srcfile=NULL)

  plot_object <- current.chob()
  if(is.null(plot_object$Env$theme$rsi)) {
    plot_object$Env$theme$rsi$col$rsi   <- "saddlebrown"
    plot_object$Env$theme$rsi$col$lines <- "orange2"
  }
  xsubset <- plot_object$Env$xsubset
  rsi <- RSI(Cl(plot_object$Env$xdata),n=n,maType=maType)
  plot_object$add_frame(ylim=c(0,1),asp=0.15)
  plot_object$next_frame()
  lenv$xdata <- structure(rsi,.Dimnames=list(NULL, "rsi"))
  text.exp <- expression(text(c(0,
                                0+strwidth(paste("RSI(",n,"):",sep=""))),
                       0.5,
                       c(paste("RSI(",n,"):",sep=""),
                         round(last(xdata[xsubset]),5)),
                       col=c(1,theme$rsi$col$rsi),adj=c(0,0),cex=0.9,offset=0,pos=4))
  plot_object$add(rect(par("usr")[1],0,par("usr")[2],1,col=theme$grid,border=NA))
  plot_object$add(text.exp, env=c(lenv,plot_object$Env), expr=TRUE)

  plot_object$add_frame(ylim=range(na.omit(rsi)),asp=1)
  plot_object$next_frame()

  # add grid lines
  lenv$grid_lines <- function(xdata,x) { c(30,70) }
  # add grid lines
  exp <- c(expression(abline(h=grid_lines(xdata,xsubset),col=theme$grid)), exp,
  # add axis labels/boxes
           expression(text(1-1/3-max(strwidth(grid_lines(xdata,xsubset))),grid_lines(xdata,xsubset),
                      noquote(format(grid_lines(xdata,xsubset),justify="right")),
                      col=theme$labels,offset=0,pos=4,cex=0.9)),
           expression(text(length(xsubset)+1/3,grid_lines(xdata,xsubset),
                      noquote(format(grid_lines(xdata,xsubset),justify="right")),
                      col=theme$labels,offset=0,pos=4,cex=0.9)))
#  exp <- c(expression(abline(h=grid_lines(xdata,xsubset),col=theme$grid,lty=1)),
#  # add axis labels/boxes
#           expression(text(0,grid_lines(xdata,xsubset),
#                      sprintf("%d",grid_lines(xdata,xsubset)),
#                      col=theme$labels,offset=0,pos=2)),
#           expression(text(length(xsubset),grid_lines(xdata,xsubset),
#                      sprintf("%d",grid_lines(xdata,xsubset)),
#                      col=theme$labels,offset=0,pos=4)),exp)
  plot_object$add(exp,env=c(lenv, plot_object$Env),expr=TRUE)
  plot_object
} # }}}

# add_MACD {{{
add_MACD <- function(fast=12,slow=26,signal=9,maType="EMA",histogram=TRUE,...) {
  lenv <- new.env()
  lenv$plot_macd <- function(x, fast, slow, signal, maType, histogram,...) {
    xdata <- x$Env$xdata
    xsubset <- x$Env$xsubset
    macd <- MACD(HLC(xdata),fast,slow,signal,maType)[xsubset]

    x.pos <- 1:NROW(macd)
    # histogram
    segments(axTicksByTime(xdata[xsubset],ticks.on=x$Env$ticks.on),
             range(na.omit(macd))[1], 
             axTicksByTime(xdata[xsubset],ticks.on=x$Env$ticks.on),
             range(na.omit(macd))[2], col=x$Env$theme$grid)
    if(histogram) {
      macd.hist <- macd[,1] - macd[,2]
      bar.col <- ifelse(macd.hist > 0, x$Env$theme$macd$up.col, x$Env$theme$macd$dn.col)
      rect(x.pos-1/3, 0, x.pos+1/3, macd.hist, col=bar.col, border="grey",...)  # base graphics call
    }
    lines(x.pos, macd[,1], col=x$Env$theme$macd$macd, lwd=2,,lty=1,...) 
    lines(x.pos, macd[,2], col=x$Env$theme$macd$signal, lty=3,...) 
  }
  # map all passed args (if any) to 'lenv' environment
  mapply(function(name,value) { assign(name,value,envir=lenv) }, 
        names(list(fast=fast,slow=slow,signal=signal,maType=maType,histogram=histogram,...)),
              list(fast=fast,slow=slow,signal=signal,maType=maType,histogram=histogram,...))
  exp <- parse(text=gsub("list","plot_macd",
               as.expression(substitute(list(x=current.chob(),fast=fast,slow=slow,signal=signal,maType=maType,
                                             histogram=histogram,...)))),
               srcfile=NULL)
  plot_object <- current.chob()

  # now we can evaluate plot_object, as the parse/substitute is behind us
  if(is.null(plot_object$Env$theme$macd)) {
    plot_object$Env$theme$macd$macd   <- "#555555"
    plot_object$Env$theme$macd$signal <- "black"
    plot_object$Env$theme$macd$up.col <- "green"
    plot_object$Env$theme$macd$dn.col <- "red"
  }
  xdata <- plot_object$Env$xdata
  xsubset <- plot_object$Env$xsubset
  macd <- MACD(HLC(xdata),fast,slow,signal,maType)
  lenv$xdata <- structure(cbind(macd,macd[,1]-macd[,2]),.Dimnames=list(NULL,c("macd","signal","histogram")))
  lenv$macd <- cbind(macd,macd[,1]-macd[,2])
  
  # text annotation
  plot_object$add_frame(ylim=c(0,1),asp=0.15)
  plot_object$next_frame()
  text.exp <- expression(text(x=c(0,
                                  0+strwidth(paste("MACD(",paste(fast,slow,signal,sep=","),"):",sep="")),
                                  0+strwidth(paste("MACD(",paste(fast,slow,signal,sep=","),"):",sep=""))+strwidth("5")*7),
                              y=0.5,
                              labels=c(paste("MACD(",paste(fast,slow,signal,sep=","),"):",sep=""),round(last(xdata[xsubset,1]),5),
                                       round(last(xdata[xsubset,2]),5)),
                              col=c(1,theme$macd$macd,theme$macd$signal),adj=c(0,0),cex=0.9,offset=0,pos=4))
  plot_object$add(rect(par("usr")[1],0,par("usr")[2],1,col=theme$grid,border=NA))
  plot_object$add(text.exp, env=c(lenv,plot_object$Env), expr=TRUE)

  # main MACD plot from expression above
  lenv$grid_lines <- function(xdata,x) { 
    axTicksByValue(xdata[xsubset],c(5,4,3,2,1),3)
  }
  plot_object$add_frame(ylim=range(na.omit(macd[xsubset])),fixed=FALSE,asp=1)
  plot_object$next_frame()

  # add grid lines
  exp <- c(expression(abline(h=grid_lines(xdata,xsubset),col=theme$grid)), exp,
  # add axis labels/boxes
           expression(text(1-1/3-max(strwidth(grid_lines(xdata,xsubset))),grid_lines(xdata,xsubset),
                      noquote(format(grid_lines(xdata,xsubset),justify="right")),
                      col=theme$labels,offset=0,pos=4,cex=0.9)),
           expression(text(length(xsubset)+1/3,grid_lines(xdata,xsubset),
                      noquote(format(grid_lines(xdata,xsubset),justify="right")),
                      col=theme$labels,offset=0,pos=4,cex=0.9)))
  plot_object$add(exp,env=c(lenv, plot_object$Env),expr=TRUE)
  # return plot_object to allow for auto-printing
  plot_object
} # }}}

# add_BBands {{{
add_BBands <- function(n=20, maType="EMA", sd=2, on=-1, ...) { 
  lenv <- new.env()
  lenv$plot_bbands <- function(x, n, maType, sd, on, ...) {
    xdata <- x$Env$xdata
    xsubset <- x$Env$xsubset
    theme <- x$Env$theme
    bbands <- coredata(BBands(Cl(xdata),n=n, maType,sd)[xsubset])
    if(on < 0) {
      xx <- do.call("seq",as.list(plot_object$get_xlim()))
      polygon(c(xx,rev(xx)), c(bbands[,1],rev(bbands[,3])),col=theme$bbands$bg,border=NA)
      lines(1:NROW(xdata[xsubset]), bbands[,1], lty='dashed', col=theme$bbands$upper,...)
      lines(1:NROW(xdata[xsubset]), bbands[,3], lty='dashed', col=theme$bbands$lower,...)
      lines(1:NROW(xdata[xsubset]), bbands[,2], lty='dotted', col=theme$bbands$ma,...)
    } else {
      lines(1:NROW(xdata[xsubset]), bbands[,1], lty='dashed', ...)
      lines(1:NROW(xdata[xsubset]), bbands[,3], lty='dashed', ...)
      lines(1:NROW(xdata[xsubset]), bbands[,2], lty='dotted', ...)
    }
  }
  mapply(function(name,value) { assign(name,value,envir=lenv) },
         names(list(n=n,maType=maType,sd=sd,on=on,...)), list(n=n,maType=maType,sd=sd,on=on,...))
  exp <- parse(text=gsub("list","plot_bbands",as.expression(substitute(list(x=current.chob(),n=n,maType=maType,
               sd=sd,on=on,...)))),srcfile=NULL)
  # save data that is drawn on charts
  plot_object <- current.chob()
  xdata <- plot_object$Env$xdata
  lenv$xdata <- BBands(Cl(xdata),n=n, maType,sd)[,-4]  # pctB is bad for ylim calculation on subset
  if(is.null(plot_object$Env$theme$bbands)) {
  plot_object$Env$theme$bbands <- list(bg="#f1f1f1", #plot_object$Env$theme$grid2,
                                       upper=plot_object$Env$theme$grid,
                                       lower=plot_object$Env$theme$grid,
                                       ma=plot_object$Env$theme$grid)
  }

  plot_object$set_frame(sign(on)*(abs(on)+1L)) # need to adjust for header offset
  plot_object$add(exp,env=c(lenv, plot_object$Env),expr=TRUE)
  plot_object
} # }}}

# add_Vo {{{
add_Vo <- function(...) {
  lenv <- new.env()

  lenv$plot_vo <- function(x, ...) {
    # this is local to this function, but can be anywhere visible
    xdata <- x$Env$xdata        # internal main series
    xsubset <- x$Env$xsubset    # subset of series to plot
    vo <- x$Env$vo[xsubset]    # get and set ylim
    if(is.OHLC(xdata[xsubset])) {
      Opens <- as.numeric(Op(xdata[xsubset]))
      Highs <- as.numeric(Hi(xdata[xsubset]))
      Lows <- as.numeric(Lo(xdata[xsubset]))
      Closes <- as.numeric(Cl(xdata[xsubset]))
    }
    bar.col <- ifelse(Opens < Closes, x$Env$theme$up.col, x$Env$theme$dn.col)
    bar.border <- ifelse(Opens < Closes, x$Env$theme$up.border, x$Env$theme$dn.border)

    cur_ylim <- x$get_ylim()
    cur_ylim[[x$get_frame()]] <- range(vo)
    x$set_ylim(cur_ylim)
    
    x.pos <- 1:NROW(vo)
    min.vol <- min(vo)
    segments(axTicksByTime(xdata[xsubset],ticks.on=x$Env$ticks.on),
             range(na.omit(vo))[1], 
             axTicksByTime(xdata[xsubset],ticks.on=x$Env$ticks.on),
             range(na.omit(vo))[2], col=x$Env$theme$grid)
    rect(x.pos-1/3, min.vol, x.pos+1/3, vo, col=bar.col, border=bar.border,...)  # base graphics call
  }

  # map all passed args (if any) to 'lenv' environment
  mapply(function(name,value) { assign(name,value,envir=lenv) }, names(list(...)), list(...))
  exp <- parse(text=gsub("list","plot_vo",as.expression(substitute(list(x=current.chob(),...)))),
               srcfile=NULL)
  plot_object <- current.chob()
  xdata <- plot_object$Env$vo
  xsubset <- plot_object$Env$xsubset
  vo <- xdata[xsubset]
  lenv$xdata <- xdata # xdata in lenv is 
    plot_object$add_frame(ylim=c(0,1),asp=0.15)
    plot_object$next_frame()
    text.exp <- expression(text(c(0,
                                  0+strwidth(paste("Volume:",sep=""))),
                         0.5,
                         c(paste("Volume:",sep=""),prettyNum(last(xdata[xsubset]),big.mark=",")),
                         col=ifelse(diff(last(xdata[xsubset],2)) >0, theme$up.col, theme$dn.col),adj=c(0,0),cex=0.9,offset=0,pos=4))
    plot_object$add(rect(par("usr")[1],0,par("usr")[2],1,col=theme$grid,border=NA))
    plot_object$add(text.exp, env=c(lenv,plot_object$Env), expr=TRUE)
    lenv$grid_lines <- function(xdata,x) { seq(0,1) }
    # add grid lines
    exp <- c(expression(abline(h=grid_lines(xdata,xsubset),col=theme$grid)),
    # add axis labels/boxes
             expression(text(0,grid_lines(xdata,xsubset),
                        sprintf("%+d",grid_lines(xdata,xsubset)),
                        col=theme$labels,offset=0,pos=2)),
             expression(text(length(xsubset),grid_lines(xdata,xsubset),
                        sprintf("%+d",grid_lines(xdata,xsubset)),
                        col=theme$labels,offset=0,pos=4)),exp)
  plot_object$add_frame(ylim=range(vo),asp=1)  # need to have a value set for ylim
  plot_object$next_frame()
  plot_object$replot(exp,env=c(lenv, plot_object$Env),expr=TRUE)
  plot_object
} # }}}

`+.replot` <- function(e1, e2) {
  e2 <- match.call()$e2
  e2$plot_object <- (substitute(e1))
  eval(e2)
}

`+.replot` <- function(e1, e2) {
  assign(".chob",e1,.GlobalEnv)
  e2 <- eval(e2)
  e2
}


##### accessor functions

re_Chart <- function() current.chob()
chart_asp <- function() current.chob()$get_asp()
chart_ylim <- function() current.chob()$get_ylim()
chart_xlim <- function() current.chob()$get_xlim()

actions <- function(obj) obj$Env$actions
chart_actions <- function() actions(current.chob())

