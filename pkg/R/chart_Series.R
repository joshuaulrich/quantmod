findOHLC <- function() {
  chob <- current.chob()
  loc <- round(locator(1)$x)
  ohlc <- current.chob()$Env$xdata[current.chob()$Env$xsubset][loc]
  actions <- chob$Env$actions
  envs <- lapply(actions[which(!sapply(actions,attr,'frame')%%2)],attr,'env')
  values <- lapply(lapply(envs[sapply(envs,is.list)],`[[`,1),
                   function(x) x$xdata[chob$Env$xsubset][loc])
  do.call('cbind',c(list(ohlc),values))
}

getSubset <- function() {
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

# axTicksByValue {{{
axTicksByValue <-
function(x,
         match.to=c(1e8,1e7,1e6,1e5,1e4,1e3,
                    500,300,200,150,100,
                    50,20,10,
                    5,2,1,
                    0.50,0.25,0.20,0.10,
                    0.05,0.02,0.01), 
         lt=20,gt=3, secondary=FALSE) {
  x <- na.omit(x)
  diff_range <- diff(range(x))
  if(diff_range > 1)
    diff_range <- diff(range(x %/% 1))
  by <- match.to[which(diff_range %/% match.to > gt & diff_range %/% match.to < lt)[1]]
  if(is.na(by)) {
    by <- 1L
  }
  ticks1 <- do.call('seq.int', as.list(c(range(x)[1]%/%by*by,range(x)[2]%/%by*by,by)))
#  if(length(ticks1) > 5) ticks1 <- ticks1[-c(1,length(ticks1))]
  ticks1
} # }}}

#axTicksByValue <- function(x, ...) pretty(x)

# UNUSED heikin.ashi.bars {{{
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
  
} # }}}

# range.bars {{{
range.bars <-
function(x, type="", spacing=1, line.col="darkorange",
         up.col="green",dn.col="red",up.border="grey",dn.border=up.border) {
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
    line.col <- rep(line.col, length.out=NCOL(x))
    for(i in 1:NCOL(x))
      lines(1:NROW(x),x[,i],lwd=2,col=line.col[i],lend=3,lty=1)
    return(NULL)
  }
  bar.col <- ifelse(Opens < Closes, up.col, dn.col)
  bar.border <- ifelse(Opens < Closes, up.border, dn.border)
  
  x.pos <- spacing*(1:NROW(x))
  if( type %in% c("ohlc", "hlc")) {
    bar.border <- bar.col
    bar.border[is.na(bar.border)] <- up.border
  }

  segments(x.pos, Lows, x.pos, apply(cbind(Opens,Closes),1,min),col=bar.border,lwd=1.2,lend=3)
  segments(x.pos, Highs, x.pos, apply(cbind(Opens,Closes),1,max),col=bar.border,lwd=1.2,lend=3)

  if (type == "candlesticks") {
     rect(x.pos - spacing/3, Opens, x.pos + spacing/3, 
          Closes, col = bar.col, border = bar.border, lwd=0.2)
  } else 
  if (type == "matchsticks") {
    bar.col[is.na(bar.col)] <- up.col
    segments(x.pos, Opens, x.pos, Closes, col=bar.col,lwd=1.2,lend=3)
  } else
  if (type == "ohlc") {
    segments(x.pos, Opens, x.pos, Closes, col=bar.border,lwd=1.2,lend=3)
    segments(x.pos-1/3, Opens, x.pos, Opens, col=bar.border,lwd=1.2,lend=3) 
    segments(x.pos, Closes, x.pos+1/3, Closes, col=bar.border,lwd=1.2,lend=3) 
  } else
  if (type == "hlc") {
    segments(x.pos, Opens, x.pos, Closes, col=bar.border,lwd=1.2,lend=3)
    segments(x.pos, Closes, x.pos+1/3, Closes, col=bar.border,lwd=1.2,lend=3) 
  }
} # }}}

# {{{ chart_theme
chart_theme <- chart_theme_white <- function() {
  theme <-list(col=list(bg="#FFFFFF",
                        label.bg="#F0F0F0",
                        grid="#F0F0F0",
                        grid2="#F5F5F5",
                        ticks="#999999",
                        labels="#333333",
                        line.col="darkorange",
                        dn.col="red",
                        up.col=NA, 
                        dn.border="#333333", 
                        up.border="#333333"),
               shading=1,
               format.labels=TRUE,
               coarse.time=TRUE,
               rylab=TRUE,
               lylab=TRUE,
               grid.ticks.lwd=1,
               grid.ticks.on="months")
  theme$bbands <- list(col=list(fill="whitesmoke",upper="#D5D5D5",
                                lower="#D5D5D5",ma="#D5D5D5"),
                       lty=list(upper="dashed",lower="dashed",ma="dotted")
                      )
  theme
} # }}}

# chart_pars {{{
chart_pars <- function() {
  list(cex=0.6, mar=c(3,1,0,1))
} # }}}

# chart_Series {{{
#  Updated: 2010-01-15
#
#  chart_Series now uses a new graphical extension
#  called 'replot'.  This enables the accumulation
#  of 'actions', in the form of (unevaluated) R 
#  expressions, to be stored within a replot object.
#  This object is an R closure, which contains
#  all the methods which are needed to perform
#  graphical operations.
#
#  Ideally all behavior is consistent with the
#  original quantmod:::chartSeries, except the
#  undesireable ones.
chart_Series <- function(x, 
                         name=deparse(substitute(x)), 
                         type="candlesticks", 
                         subset="", 
                         TA="",
                         pars=chart_pars(), theme=chart_theme(),
                         clev=0) {
  cs <- new.replot()
  #cex <- pars$cex
  #mar <- pars$mar
  line.col <- theme$col$line.col
  up.col <- theme$col$up.col
  dn.col <- theme$col$dn.col
  up.border <- theme$col$up.border
  dn.border <- theme$col$dn.border
  format.labels <- theme$format.labels
  if(is.null(theme$grid.ticks.on)) {
    xs <- x[subset]
    major.grid <- c(years=nyears(xs),
                    months=nmonths(xs),
                    days=ndays(xs))
    grid.ticks.on <- names(major.grid)[rev(which(major.grid < 30))[1]]
  } else grid.ticks.on <- theme$grid.ticks.on
  label.bg <- theme$col$label.bg
  
  cs$subset <- function(x) {
    if(FALSE) {set_ylim <- get_ylim <- set_xlim <- Env<-function(){} }  # appease R parser?
    if(missing(x)) {
      x <- "" #1:NROW(Env$xdata)
    }
    Env$xsubset <<- x
    set_xlim(c(1,NROW(Env$xdata[Env$xsubset])))
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
  if(is.character(x))
    stop("'x' must be a time-series object")
  if(is.OHLC(x)) {
    cs$Env$xdata <- OHLC(x)
    if(has.Vo(x))
      cs$Env$vo <- Vo(x)
  } else cs$Env$xdata <- x
  #subset <- match(.index(x[subset]), .index(x))
  cs$Env$xsubset <- subset
  cs$Env$cex <- pars$cex
  cs$Env$mar <- pars$mar
  cs$set_asp(3)
  cs$set_xlim(c(1,NROW(cs$Env$xdata[subset])))
  cs$set_ylim(list(structure(range(na.omit(cs$Env$xdata[subset])),fixed=FALSE)))
  cs$set_frame(1,FALSE)
  cs$Env$clev = min(clev+0.01,1) # (0,1]
  cs$Env$theme$bbands <- theme$bbands
  cs$Env$theme$shading <- theme$shading
  cs$Env$theme$line.col <- theme$col$line.col
  cs$Env$theme$up.col <- up.col
  cs$Env$theme$dn.col <- dn.col
  cs$Env$theme$up.border <- up.border
  cs$Env$theme$dn.border <- dn.border
  cs$Env$theme$bg <- theme$col$bg
  cs$Env$theme$grid <- theme$col$grid
  cs$Env$theme$grid2 <- theme$col$grid2
  cs$Env$theme$labels <- "#333333"
  cs$Env$theme$label.bg <- label.bg
  cs$Env$format.labels <- format.labels
  cs$Env$ticks.on <- grid.ticks.on
  cs$Env$grid.ticks.lwd <- theme$grid.ticks.lwd
  cs$Env$type <- type

  # axis_ticks function to label lower frequency ranges/grid lines
  cs$Env$axis_ticks <- function(xdata,xsubset) {
    ticks <- diff(axTicksByTime2(xdata[xsubset],labels=FALSE))/2 + 
                  last(axTicksByTime2(xdata[xsubset],labels=TRUE),-1)
    if(!theme$coarse.time || length(ticks) == 1)
      return(unname(ticks))
    if(min(diff(ticks)) < max(strwidth(names(ticks)))) {
      ticks <- unname(ticks)
    }
    ticks
  }
  # need to add if(upper.x.label) to allow for finer control
  cs$add(expression(atbt <- axTicksByTime2(xdata[xsubset]),
                    segments(atbt, #axTicksByTime2(xdata[xsubset]),
                             get_ylim()[[2]][1],
                             atbt, #axTicksByTime2(xdata[xsubset]),
                             get_ylim()[[2]][2], col=theme$grid, lwd=grid.ticks.lwd),
                    axt <- axis_ticks(xdata,xsubset),
                    text(as.numeric(axt),
                         par('usr')[3]-0.2*min(strheight(axt)),
                         names(axt),xpd=TRUE,cex=0.9,pos=3)),
                    clip=FALSE,expr=TRUE)
  cs$set_frame(-1)
  # background of main window
  #cs$add(expression(rect(par("usr")[1],
  #                       par("usr")[3],
  #                       par("usr")[2],
  #                       par("usr")[4],border=NA,col=theme$bg)),expr=TRUE)
  cs$add_frame(0,ylim=c(0,1),asp=0.2)
  cs$set_frame(1)

  # add observation level ticks on x-axis if < 400 obs.
  cs$add(expression(if(NROW(xdata[xsubset])<400) 
          {axis(1,at=1:NROW(xdata[xsubset]),labels=FALSE,col=theme$grid2,tcl=0.3)}),expr=TRUE)

  # add "month" or "month.abb"
  cs$add(expression(axt <- axTicksByTime(xdata[xsubset],format.labels=format.labels),
                axis(1,at=axt, #axTicksByTime(xdata[xsubset]),
                labels=names(axt), #axTicksByTime(xdata[xsubset],format.labels=format.labels)),
                las=1,lwd.ticks=1,mgp=c(3,1.5,0),tcl=-0.4,cex.axis=.9)),
         expr=TRUE)
  cs$Env$name <- name
  text.exp <- c(expression(text(1-1/3,0.5,name,font=2,col='#444444',offset=0,cex=1.1,pos=4)),
                expression(text(NROW(xdata[xsubset]),0.5,
                           paste(start(xdata[xsubset]),end(xdata[xsubset]),sep=" / "),
                           col=1,adj=c(0,0),pos=2)))
  cs$add(text.exp, env=cs$Env, expr=TRUE)
  cs$set_frame(2)

  cs$Env$axis_labels <- function(xdata,xsubset,scale=5) {
    axTicksByValue(na.omit(xdata[xsubset]))
  }
  #cs$add(assign("five",rnorm(10)))  # this gets re-evaled each update, though only to test
  #cs$add(expression(assign("alabels", axTicksByValue(na.omit(xdata[xsubset])))),expr=TRUE)
  #cs$add(expression(assign("alabels", pretty(range(xdata[xsubset],na.rm=TRUE)))),expr=TRUE)
  cs$add(expression(assign("alabels", pretty(get_ylim(get_frame())[[2]],10))),expr=TRUE)

  # add $1 grid lines if appropriate
  cs$set_frame(-2)

  # add minor y-grid lines
  cs$add(expression(if(diff(range(xdata[xsubset])) < 50)
                    segments(1,seq(min(xdata[xsubset]%/%1,na.rm=TRUE),
                                   max(xdata[xsubset]%/%1,na.rm=TRUE),1),
                             length(xsubset),
                               seq(min(xdata[xsubset]%/%1,na.rm=TRUE),
                                   max(xdata[xsubset]%/%1,na.rm=TRUE),1),
                             col=theme$grid2, lty="dotted")), expr=TRUE)
  cs$set_frame(2)
  # add main y-grid lines
  cs$add(expression(segments(1,alabels,NROW(xdata[xsubset]),alabels, col=theme$grid)),expr=TRUE)
  # left axis labels
  if(theme$lylab) {
    cs$add(expression(text(1-1/3-max(strwidth(alabels)),
                alabels, #axis_labels(xdata,xsubset), 
                noquote(format(alabels,justify="right")), 
                col=theme$labels,offset=0,cex=0.9,pos=4)),expr=TRUE)
  }
  # right axis labels
  if(theme$rylab) {
    cs$add(expression(text(NROW(xdata[xsubset])+1/3,
                alabels, 
                noquote(format(alabels,justify="right")),
                col=theme$labels,offset=0,cex=0.9,pos=4)),expr=TRUE)
  }
  # add main series
  cs$set_frame(2)
  # need to rename range.bars to something more generic, and allow type= to handle:
  #  ohlc, hlc, candles, ha-candles, line, area
  #  chart_Perf will be the call to handle relative performace plots
  cs$add(expression(range.bars(xdata[xsubset], 
                    type, 1,
                    fade(theme$line.col,clev),
                    fade(theme$up.col,clev),
                    fade(theme$dn.col,clev),
                    fade(theme$up.border,clev),
                    fade(theme$dn.border,clev))),expr=TRUE)
  assign(".chob", cs, .GlobalEnv)

  # handle TA="add_Vo()" as we would interactively FIXME: allow TA=NULL to work
  if(!is.null(TA) && nchar(TA) > 0) {
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
} #}}}

# zoom_Chart {{{
zoom_Chart <- function(subset) {
  chob <- current.chob()
  chob$subset(subset)
  chob
}
# }}}

fade <- function(col, level) {
  # adjust col toward white, (?background) by 0-1 range 
  cols <- character(length(col))
  for(i in 1:length(col))
  cols[i] <- colorRampPalette(c(col[i], "white"))(99)[level*100]
  cols
}

current.chob <- function() get(".chob",.GlobalEnv)
use.chob <- function(use=TRUE) {
  options('global.chob'=use) 
}

new_ta <- function(FUN, preFUN, postFUN, on=NA, ...) {}

# add_Series {{{
add_Series <- function(x, type="candlesticks",order=NULL, on=NA, legend="auto", theme=NULL,...) { 
  lenv <- new.env()
  lenv$name <- deparse(substitute(x))
  lenv$plot_series <- function(x, series, type, ...) {
    # vertical grid lines
    if(FALSE) theme <- NULL
    segments(axTicksByTime2(xdata[xsubset]),
             par("usr")[3], #min(-10,range(na.omit(macd))[1]), 
             axTicksByTime2(xdata[xsubset]),
             par("usr")[4], #max(10,range(na.omit(macd))[2]), col=x$Env$theme$grid)
             col=theme$grid)
             #col=x$Env$theme$grid)
    series <- merge(series, x$Env$xdata, join="outer",retside=c(TRUE,FALSE))[x$Env$xsubset]
    range.bars(series, type=type)
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
  lenv$theme <- if(is.null(theme)) plot_object$Env$theme else theme
  xdata <- plot_object$Env$xdata
  xsubset <- plot_object$Env$xsubset
  tav <- merge(x, xdata, join="left",retside=c(TRUE,FALSE))
  lenv$upper.env <- plot_object$Env
  lenv$xdata <- x
  x <- x[xsubset]

  if(is.na(on)) {
    plot_object$add_frame(ylim=c(0,1),asp=0.15)
    plot_object$next_frame()
    text.exp <- expression(text(x=c(1),y=0.3, name, col=c(1),adj=c(0,0),cex=0.9,offset=0,pos=4))
    plot_object$add(text.exp, env=c(lenv,plot_object$Env), expr=TRUE)

    plot_object$add_frame(ylim=range(na.omit(OHLC(x))),asp=1)  # need to have a value set for ylim
    plot_object$next_frame()
    plot_object$add(expression(assign("alabels", axTicksByValue(na.omit(xdata[xsubset])))),expr=TRUE)

  # add main y-grid lines
    plot_object$add(expression(segments(1,alabels,NROW(xdata[xsubset]),alabels, col=theme$grid)),expr=TRUE)
  # left axis labels
    exp <- c(expression(text(1-1/3-max(strwidth(alabels)),
                alabels, #axis_labels(xdata,xsubset), 
                noquote(format(alabels,justify="right")), 
                col=theme$labels,offset=0,cex=0.9,pos=4)),
             expression(text(NROW(upper.env$xdata[xsubset])+1/3,
                alabels, 
                noquote(format(alabels,justify="right")),
                col=theme$labels,offset=0,cex=0.9,pos=4)),exp)
#    lenv$grid_lines <- function(xdata,x) { seq(-1,1) }
#    # add grid lines
#    exp <- c(expression(abline(h=grid_lines(xdata,xsubset),col=theme$grid)),
#    # add axis labels/boxes
#             expression(text(0,grid_lines(xdata,xsubset),
#                        sprintf("%+d",grid_lines(xdata,xsubset)),
#                        col=theme$labels,pos=2)),
#             expression(text(NROW(xdata[xsubset]),grid_lines(xdata,xsubset),
#                        sprintf("%+d",grid_lines(xdata,xsubset)),
#                        col=theme$labels,pos=4)),exp)
  } else { plot_object$set_frame(sign(on)*(abs(on)+1L)) }


  plot_object$add(exp,env=c(lenv, plot_object$Env),expr=TRUE)
  plot_object
} #}}}
# add_TA {{{
add_TA <- function(x, order=NULL, on=NA, legend="auto",
                   yaxis=list(NULL,NULL),
                   col=1, taType=NULL, ...) { 
  lenv <- new.env()
  lenv$name <- deparse(substitute(x))
  lenv$plot_ta <- function(x, ta, on, taType, col=col,...) {
    xdata <- x$Env$xdata
    xsubset <- x$Env$xsubset
    if(all(is.na(on))) {
      segments(axTicksByTime2(xdata[xsubset]),
               par("usr")[3],
               axTicksByTime2(xdata[xsubset]),
               par("usr")[4],
               col=x$Env$theme$grid)
    }
    if(is.logical(ta)) {
      ta <- merge(ta, xdata, join="right",retside=c(TRUE,FALSE))[xsubset]
      shade <- quantmod:::shading(as.logical(ta,drop=FALSE))
      if(length(shade$start) > 0) # all FALSE cause zero-length results
        rect(shade$start-1/3, par("usr")[3] ,shade$end+1/3, par("usr")[4], col=col,...) 
    } else {
      # we can add points that are not necessarily at the points
      # on the main series
      subset.range <- paste(start(x$Env$xdata[x$Env$xsubset]),
                            end(x$Env$xdata[x$Env$xsubset]),sep="/")
      ta.adj <- merge(n=.xts(1:NROW(x$Env$xdata[x$Env$xsubset]),
                             .index(x$Env$xdata[x$Env$xsubset])),ta)[subset.range]
      ta.x <- as.numeric(na.approx(ta.adj[,1]))
      ta.y <- ta.adj[,-1]
      for(i in 1:NCOL(ta.y))
        lines(ta.x, as.numeric(ta.y[,i]), col=col,...)
    }
  }
  lenv$xdata <- x
  # map all passed args (if any) to 'lenv' environment
  mapply(function(name,value) { assign(name,value,envir=lenv) }, 
        names(list(x=x,order=order,on=on,legend=legend,
                   taType=taType,col=col,...)),
              list(x=x,order=order,on=on,legend=legend,
                   taType=taType,col=col,...))
  exp <- parse(text=gsub("list","plot_ta",
               as.expression(substitute(list(x=current.chob(),
                             ta=get("x"),on=on,
                             taType=taType,col=col,...)))),
               srcfile=NULL)
  plot_object <- current.chob()
  xdata <- plot_object$Env$xdata
  xsubset <- plot_object$Env$xsubset
  if(is.logical(x)) no.update <- TRUE else no.update <- FALSE
  #  this merge isn't going to work if x isn't in xdata range. Something like:
  #    na.approx(merge(n=.xts(1:NROW(xdata),.index(xdata)),ta)[,1])
  #  should allow for any time not in the original to be merged in.
  #  probably need to subset xdata _before_ merging, else subset will be wrong
  #
  #tav <- merge(x, xdata, join="right",retside=c(TRUE,FALSE))
  #lenv$xdata <- tav
  #tav <- tav[xsubset]
  lenv$col <- col
  lenv$xdata <- merge(x,xdata,retside=c(TRUE,FALSE))

  if(is.na(on)) {
    plot_object$add_frame(ylim=c(0,1),asp=0.15)
    plot_object$next_frame()
    text.exp <- expression(text(x=c(1,1+strwidth(name)),
                                y=0.3,
                                labels=c(name,round(last(xdata[xsubset]),5)),
                                col=c(1,col),adj=c(0,0),cex=0.9,offset=0,pos=4))
    plot_object$add(text.exp, env=c(lenv,plot_object$Env), expr=TRUE)

    plot_object$add_frame(ylim=range(na.omit(xdata)),asp=1)  # need to have a value set for ylim
    plot_object$next_frame()
  # add grid lines, using custom function for MACD gridlines
  lenv$grid_lines <- function(xdata,xsubset) { 
    pretty(xdata[xsubset])
  }
  exp <- c(expression(segments(1,grid_lines(xdata,xsubset),NROW(xdata[xsubset]),grid_lines(xdata,xsubset),
                               col=theme$grid)), exp,  # NOTE 'exp' was defined earlier to be plot_macd
  # add axis labels/boxes
           expression(text(1-1/3-max(strwidth(grid_lines(xdata,xsubset))),grid_lines(xdata,xsubset),
                      noquote(format(grid_lines(xdata,xsubset),justify="right")),
                      col=theme$labels,offset=0,pos=4,cex=0.9)),
           expression(text(NROW(xdata[xsubset])+1/3,grid_lines(xdata,xsubset),
                      noquote(format(grid_lines(xdata,xsubset),justify="right")),
                      col=theme$labels,offset=0,pos=4,cex=0.9)))
  plot_object$add(exp,env=c(lenv, plot_object$Env),expr=TRUE,no.update=no.update)
  } else { 
    for(i in 1:length(on)) {
      plot_object$set_frame(2*on[i]) # this is defaulting to using headers, should it be optionable?
      lenv$grid_lines <- function(xdata,xsubset) { 
        pretty(xdata[xsubset])
      }
      exp <- c(exp,
           # LHS
           #expression(text(1-1/3-max(strwidth(grid_lines(xdata,xsubset))),grid_lines(xdata,xsubset),
           #           noquote(format(grid_lines(xdata,xsubset),justify="right")),
           #           col=theme$labels,offset=0,pos=4,cex=0.9)),
           # RHS
           expression(text(NROW(xdata[xsubset])+1/3,grid_lines(xdata,xsubset),
                      noquote(format(grid_lines(xdata,xsubset),justify="right")),
                      col=theme$labels,offset=0,pos=4,cex=0.9)))
      }
      plot_object$add(exp,env=c(lenv, plot_object$Env),expr=TRUE,no.update=no.update)
    }
  }
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
    x.pos <- 1:NROW(xdata[xsubset])
    segments(axTicksByTime2(xdata[xsubset]),
             range(na.omit(smi))[1], 
             axTicksByTime2(xdata[xsubset]),
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
  plot_object$add_frame(ylim=c(0,1),asp=0.2)
  plot_object$next_frame()
  lenv$xdata <- structure(smi,.Dimnames=list(NULL, c("smi","signal")))
  text.exp <- expression(text(c(1,
                                1+strwidth(paste("SMI(",paste(n,nFast,nSlow,nSig,sep=","),"):",sep="")),
                                1+strwidth(paste("SMI(",paste(n,nFast,nSlow,nSig,sep=","),"):",sep=""))+strwidth("-22.22222")),
                       0.3,
                       c(paste("SMI(",paste(n,nFast,nSlow,nSig,sep=","),"):",sep=""),
                         round(last(xdata[xsubset,1]),5),
                         round(last(xdata[xsubset,2]),5)),
                       col=c(1,theme$smi$col$smi,theme$smi$col$signal),adj=c(0,0),cex=0.9,offset=0,pos=4))
  #plot_object$add(expression(rect(par("usr")[1],0,par("usr")[2],1,col=theme$grid,border=NA)),expr=TRUE)
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
           expression(text(NROW(xdata[xsubset])+1/3,grid_lines(xdata,xsubset),
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
    # vertical grid lines
    segments(axTicksByTime2(xdata[xsubset]),
             par("usr")[3], #min(-10,range(na.omit(macd))[1]), 
             axTicksByTime2(xdata[xsubset]),
             par("usr")[4], #max(10,range(na.omit(macd))[2]), col=x$Env$theme$grid)
             col=x$Env$theme$grid)
    lines(x.pos, rep(30,length(x.pos)), col=theme$col$lines, lwd=1,lty=2,lend=2,...) 
    lines(x.pos, rep(70,length(x.pos)), col=theme$col$lines, lwd=1,lty=2,lend=2,...) 
    lines(x.pos, rsi[,1], col=x$Env$theme$rsi$col$rsi, lwd=1.5,...) 
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
  plot_object$add_frame(ylim=c(0,1),asp=0.2)
  plot_object$next_frame()
  lenv$xdata <- structure(rsi,.Dimnames=list(NULL, "rsi"))
  text.exp <- expression(text(c(1,
                                1+strwidth(paste("RSI(",n,"):",sep=""))),
                       0.3,
                       c(paste("RSI(",n,"):",sep=""),
                         round(last(xdata[xsubset]),5)),
                       col=c(1,theme$rsi$col$rsi),adj=c(0,0),cex=0.9,offset=0,pos=4))
  #plot_object$add(expression(rect(par("usr")[1],0,par("usr")[2],1,col=theme$grid,border="black")),expr=TRUE)
  plot_object$add(text.exp, env=c(lenv,plot_object$Env), expr=TRUE)

  plot_object$add_frame(ylim=c(0,100),asp=1,fixed=TRUE)
  plot_object$next_frame()

  # add grid lines
  lenv$grid_lines <- function(xdata,x) { c(30,70) }
  # add grid lines
  exp <- c(expression(segments(1, grid_lines(xdata,xsubset),
                               NROW(xdata[xsubset]), grid_lines(xdata,xsubset), col=theme$grid)),exp,
  # add axis labels/boxes
           expression(text(1-1/3-max(strwidth(grid_lines(xdata,xsubset))),grid_lines(xdata,xsubset),
                      noquote(format(grid_lines(xdata,xsubset),justify="right")),
                      col=theme$labels,offset=0,pos=4,cex=0.9)),
           expression(text(NROW(xdata[xsubset])+1/3,grid_lines(xdata,xsubset),
                      noquote(format(grid_lines(xdata,xsubset),justify="right")),
                      col=theme$labels,offset=0,pos=4,cex=0.9)))
  plot_object$add(exp,env=c(lenv, plot_object$Env),expr=TRUE)
  plot_object
} # }}}

skeleton_TA <- function(on) {
  lenv <- new.env()
  lenv$plot_ta <- function(x, arg, ...) {
    # fill in body of low level plot calls here
    # use a switch based on type of TA to draw: bands, bars, lines, dots...
  }
  mapply(function(name, value) {assign(name,value,envir=lenv)},
         names(list(arg=arg,...)),
               list(arg=arg,...))
  exp <- parse(text=gsub("list","plot_ta",
               as.expression(substitute(list(x=current.chob(),
                                        arg=arg,
                                        ...)))), srcfile=NULL)
  chob <- current.chob()
  xsubset <- chob$Env$xsubset
  preFUN <- ""
  FUN <- ""
  postFUN <- ""
  chob$add_frame(ylin=c(0,1),asp=0.15)
  chob$next_frame()
}

# add_MACD {{{
add_MACD <- function(fast=12,slow=26,signal=9,maType="EMA",histogram=TRUE,...) {
  lenv <- new.env() # local environment for add_MACD call
  
  # plot_macd draws the indicator using the data from the first(only) call to
  # add_MACD.  This is a bit analogous to chartMACD in the first quantmod versions
  lenv$plot_macd <- function(x, fast, slow, signal, maType, histogram,...) {
    xdata <- x$Env$xdata
    xsubset <- x$Env$xsubset
    macd <- macd[xsubset]
    # vertical grid lines
    segments(axTicksByTime2(xdata[xsubset]),
             par("usr")[3], #min(-10,range(na.omit(macd))[1]), 
             axTicksByTime2(xdata[xsubset]),
             par("usr")[4], #max(10,range(na.omit(macd))[2]), col=x$Env$theme$grid)
             col=x$Env$theme$grid)
    # histogram
    x.pos <- 1:NROW(macd)
    if(histogram) {
      macd.hist <- macd[,1] - macd[,2]
      bar.col <- ifelse(macd.hist > 0, x$Env$theme$macd$up.col, x$Env$theme$macd$dn.col)
      rect(x.pos-1/3, 0, x.pos+1/3, macd.hist, col=bar.col, border="grey", lwd=0.2, ...)  # base graphics call
    }
    # macd line
    lines(x.pos, macd[,1], col=x$Env$theme$macd$macd, lwd=2,,lty=1,...) 
    # signal line
    lines(x.pos, macd[,2], col=x$Env$theme$macd$signal, lty=3,...) 
  }

  # map all passed args (if any) to 'lenv' environment
  mapply(function(name,value) { assign(name,value,envir=lenv) }, 
        names(list(fast=fast,slow=slow,signal=signal,maType=maType,histogram=histogram,...)),
              list(fast=fast,slow=slow,signal=signal,maType=maType,histogram=histogram,...))

  # exp will be what is re-evaluated during redrawing (subset, new TA, etc)
  # we need to build this piece by piece
  exp <- parse(text=gsub("list","plot_macd",
               as.expression(substitute(list(x=current.chob(),fast=fast,slow=slow,signal=signal,maType=maType,
                                             histogram=histogram,...)))),
               srcfile=NULL)

  # plot_object is the current list of actions, and chart 'state'
  plot_object <- current.chob()

  # now we can evaluate plot_object, as the parse/substitute is behind us

  # check if the theme has a macd component, if not set defaults here
  if(is.null(plot_object$Env$theme$macd)) {
    plot_object$Env$theme$macd$macd   <- "#555555"
    plot_object$Env$theme$macd$signal <- "black"
    plot_object$Env$theme$macd$up.col <- "green"
    plot_object$Env$theme$macd$dn.col <- "red"
  }

  # copy some Env data to local, make it cleaner to read
  xdata <- plot_object$Env$xdata        # original (OHLC) series
  xsubset <- plot_object$Env$xsubset    # current subset

  # calculate our indicator here
  macd <- MACD(HLC(xdata),fast,slow,signal,maType)
  lenv$xdata <- structure(cbind(macd,macd[,1]-macd[,2]),.Dimnames=list(NULL,c("macd","signal","histogram")))
  lenv$macd <- cbind(macd,macd[,1]-macd[,2])
  
  # text annotation
  plot_object$add_frame(ylim=c(0,1),asp=0.15)   # add the header frame
  plot_object$next_frame()                      # move to header frame
  text.exp <- expression(text(x=c(1,
                                  1+strwidth(paste("MACD(",paste(fast,slow,signal,sep=","),"):",sep="")),
                                  1+strwidth(paste("MACD(",paste(fast,slow,signal,sep=","),"):",sep=""))+strwidth("5")*7),
                              y=0.3,
                              labels=c(paste("MACD(",paste(fast,slow,signal,sep=","),"):",sep=""),round(last(xdata[xsubset,1]),5),
                                       round(last(xdata[xsubset,2]),5)),
                              col=c(1,theme$macd$macd,theme$macd$signal),adj=c(0,0),cex=0.9,offset=0,pos=4))
  plot_object$add(text.exp, env=c(lenv,plot_object$Env), expr=TRUE)

  # main MACD plot from expression above
  plot_object$add_frame(ylim=range(na.omit(lenv$macd[xsubset])),fixed=FALSE,asp=1)
  plot_object$next_frame()

  # add grid lines, using custom function for MACD gridlines
  lenv$grid_lines <- function(xdata,xsubset) { 
    axTicksByValue(xdata[xsubset],c(5,4,3,2,1),gt=3)
  }
  exp <- c(expression(segments(1,grid_lines(xdata,xsubset),length(xsubset),grid_lines(xdata,xsubset),
                               col=theme$grid)), exp,  # NOTE 'exp' was defined earlier to be plot_macd
  # add axis labels/boxes
           expression(text(1-1/3-max(strwidth(grid_lines(xdata,xsubset))),grid_lines(xdata,xsubset),
                      noquote(format(grid_lines(xdata,xsubset),justify="right")),
                      col=theme$labels,offset=0,pos=4,cex=0.9)),
           expression(text(NROW(xdata[xsubset])+1/3,grid_lines(xdata,xsubset),
                      noquote(format(grid_lines(xdata,xsubset),justify="right")),
                      col=theme$labels,offset=0,pos=4,cex=0.9)))

  # add 'exp' to actions list of plot_object
  plot_object$add(exp,env=c(lenv, plot_object$Env),expr=TRUE)

  # return plot_object to allow for auto-printing
  plot_object
} # }}}

# add_BBands {{{
add_BBands <- function(n=20, maType="SMA", sd=2, on=-1, ...) { 
  lenv <- new.env()
  lenv$plot_bbands <- function(x, n, maType, sd, on, ...) {
    xdata <- x$Env$xdata
    xsubset <- x$Env$xsubset
    col <- x$Env$theme$bbands$col
    lty <- x$Env$theme$bbands$lty
    bbands <- coredata(BBands(Cl(xdata),n=n, maType,sd)[xsubset])
    if(on < 0) {
      xx <- do.call("seq",as.list(x$get_xlim()))
      polygon(c(xx,rev(xx)), c(bbands[,1],rev(bbands[,3])),col=col$fill,border=NA)
      lines(1:NROW(xdata[xsubset]), bbands[,1], lty=lty$upper, col=col$upper,...)
      lines(1:NROW(xdata[xsubset]), bbands[,3], lty=lty$lower, col=col$lower,...)
      lines(1:NROW(xdata[xsubset]), bbands[,2], lty=lty$ma, col=col$ma,...)
    } else {
      lines(1:NROW(xdata[xsubset]), bbands[,1], lty=lty$upper, ...)
      lines(1:NROW(xdata[xsubset]), bbands[,3], lty=lty$lower, ...)
      lines(1:NROW(xdata[xsubset]), bbands[,2], lty=lty$ma, ...)
    }
  }
  mapply(function(name,value) { assign(name,value,envir=lenv) },
         names(list(n=n,maType=maType,sd=sd,on=on,...)), list(n=n,maType=maType,sd=sd,on=on,...))
  exp <- parse(text=gsub("list","plot_bbands",as.expression(substitute(list(x=current.chob(),n=n,maType=maType,
               sd=sd,on=on,...)))),srcfile=NULL)
  # save data that is drawn on charts
  chob <- current.chob()
  xdata <- chob$Env$xdata
  lenv$xdata <- BBands(Cl(xdata),n=n, maType,sd)[,-4]  # pctB is bad for ylim calculation on subset

  chob$set_frame(sign(on)*(abs(on)+1L)) # need to adjust for header offset
  chob$add(exp,env=c(lenv, chob$Env),expr=TRUE)
  chob
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

    #cur_ylim <- x$get_ylim()
    #cur_ylim[[x$get_frame()]] <- range(vo)
    #x$set_ylim(cur_ylim)
    
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
  theme <- plot_object$theme
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

update_charting_warning <- function() {
  if(is.null(getOption("chartSeries_warning"))) {
    warning("chartSeries functionality is being deprecated for chart_Series")
    options(chartSeries_warning=TRUE)
  }
}
