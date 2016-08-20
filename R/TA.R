# core addTA base functions
#
# written by Jeffrey A. Ryan
# Copyright 2008
# Distributed under the GPL 3 or later

`funToTA` <-
function(x,drop.arg=1) {
  drop.arg <- if(any(drop.arg < 1)) {
    1:length(formals(x))
  } else -drop.arg
  fun.args <- paste(names(formals(x))[drop.arg],'=',sapply(formals(x), deparse)[drop.arg],sep='')
  fun.args <- paste(gsub('=$','',fun.args),collapse=',')
  paste('add',deparse(substitute(x)),'(',fun.args,') {',collapse='',sep='')
}

shading <- function(x)
{
  # to be used from addTA when passed a logical object or vector
  # also from new addEvents function
  #
  # ex. rect(shading$start-spacing, par('usr')[3],
  #          shading$end-spacing, par('usr')[3])    
  if( !is.logical(x) )
    warning('need logical object')
  runs <- rle(as.logical(x))
  list(
     start=cumsum(runs$length)[which(runs$values)] - runs$length[which(runs$values)]+1,
       end=cumsum(runs$lengths)[which(runs$values)]
      )
}

# addTA {{{
`addTA` <-
function(ta, order=NULL, on=NA, legend='auto', yrange=NULL, ...) {
  if(is.character(ta)) {
    if(exists(ta)) {
      plot(do.call(paste('add',ta,sep=''),list(...)))
    } else stop(paste('no TA method found for',paste('add',ta,sep='')))
  } else {
    lenv <- new.env()
    lenv$chartTA <- function(x, ta, order, on, legend, yrange, ...) {
      xsubset <- x$Env$xsubset
      if(!is.null(order)) ta <- ta[,order]
      if(all(is.na(on))) {
        xlim <- x$Env$xlim
        frame <- x$get_frame()
        print(frame)
        ylim <- x$get_ylim()[[frame]]
        theme <- x$Env$theme
        y_grid_lines <- x$Env$y_grid_lines
        
        # add inbox color
        rect(xlim[1], ylim[1], xlim[2], ylim[2], col=theme$fill)
        # add grid lines and left-side axis labels
        segments(xlim[1], y_grid_lines(ylim), 
                 xlim[2], y_grid_lines(ylim), 
                 col = theme$grid, lwd = x$Env$grid.ticks.lwd, lty = 3)
        text(xlim[1], y_grid_lines(ylim), y_grid_lines(ylim), 
             col = theme$labels, srt = theme$srt, 
             offset = 0.5, pos = 2, cex = theme$cex.axis, xpd = TRUE)
        # add border of plotting area
        rect(xlim[1], ylim[1], xlim[2], ylim[2], border=theme$labels)
      }
      if(is.logical(ta)) {
        ta <- merge(ta, xdata, join="right",retside=c(TRUE,FALSE))[xsubset]
        shade <- shading(as.logical(ta,drop=FALSE))
        if(length(shade$start) > 0) # all FALSE cause zero-length results
          rect(shade$start-1/3, ylim[1] ,shade$end+1/3, ylim[2], col=theme$BBands$col$fill,...) 
      } else {
        # we can add points that are not necessarily at the points
        # on the main series
        subset.range <- paste(start(xdata[xsubset]),
                              end(xdata[xsubset]),sep="/")
        ta.adj <- merge(n=.xts(1:NROW(xdata[xsubset]),
                               .index(xdata[xsubset]), tzone=indexTZ(xdata)),ta)[subset.range]
        ta.x <- as.numeric(na.approx(ta.adj[,1], rule=2) )
        ta.y <- ta.adj[,-1]
        for(i in 1:NCOL(ta.y))
          lines(ta.x, as.numeric(ta.y[,i]), ...)
      }
    }
    if(!is.character(legend) || legend == "auto")
      legend <- gsub("^add", "", deparse(match.call()))
    # map all passed args (if any) to 'lenv' environment
    mapply(function(name,value) { assign(name,value,envir=lenv) }, 
           names(list(ta=ta,order=order,on=on,legend=legend,yrange=yrange,...)),
           list(ta=ta,order=order,on=on,legend=legend,yrange=yrange,...))
    exp <- parse(text=gsub("list","chartTA",
                           as.expression(substitute(list(x=current.chob(),
                                                         ta=get("ta"),order=order,
                                                         on=on,legend=legend,
                                                         yrange=yrange,...)))),
                 srcfile=NULL)
    exp <- c(exp, expression(
      frame <- get_frame(),
      lc <- xts:::legend.coords("topleft", xlim, ylim[[frame]]),
      legend(x = lc$x, y = lc$y, 
             legend = c(paste(legend, ":"),
                        paste(sprintf("%.3f", last(ta)))),
             text.col = c(theme$fg, col), 
             xjust = lc$xjust, 
             yjust = lc$yjust, 
             bty = "n", 
             y.intersp=0.95)))

    lchob <- current.chob()
    ncalls <- length(lchob$Env$call_list)
    lchob$Env$call_list[[ncalls + 1]] <- match.call()
    if(!hasArg(col)) lenv$col <- lchob$Env$theme$BBands$col$ma
    xdata <- lchob$Env$xdata
    xsubset <- lchob$Env$xsubset
    nrc <- NROW(xdata)
  
    ta <- try.xts(ta, error=FALSE)
  
    if(is.xts(ta)) {
      x <- merge(xdata, ta, fill=ifelse(is.logical(ta),0,NA),join='left', retside=c(FALSE,TRUE))
    } else {
      if(NROW(ta) != nrc)
        stop('non-xtsible data must match the length of the underlying series')
      x <- merge(xdata, ta, join='left', retside=c(FALSE,TRUE))
    }
    if(is.logical(ta))
      x <- as.logical(x, drop=FALSE)  #identical to storage.mode(x)<-"logical"
    
    lenv$xdata <- structure(x, .Dimnames=list(NULL, names(x)))
    lenv$ta <- lchob$Env$TA$ta <- x
    lenv$get_frame <- lchob$get_frame
    if(all(is.na(on))) {
      if(missing(yrange))
        lchob$add_frame(ylim=range(lenv$ta[xsubset],na.rm=TRUE), asp=1)
      else {
        lchob$add_frame(ylim=lenv$yrange, asp=1)
      }
      lchob$next_frame()
      lchob$replot(exp, env=c(lenv, lchob$Env), expr=TRUE)
    }
    else {
      for(i in seq_along(on)) {
        lchob$set_frame(on[i]+1L)
        if(!missing(yrange)) {
          frame <- lchob$get_frame()
          lchob$Env$ylim[[frame]] <- structure(yrange, fixed=FALSE)
        }
        lchob$replot(exp, env=c(lenv, lchob$Env), expr=TRUE)
      }
    }
#   if(is.null(sys.call(-1))) {
#      TA <- lchob@passed.args$TA
#      lchob@passed.args$TA <- c(TA,chobTA)
#      lchob@windows <- lchob@windows + ifelse(chobTA@new,1,0)
#      do.call('chartSeries.chob',list(lchob))
#      #quantmod:::chartSeries.chob(lchob)
#      invisible(chobTA)
#    } else {
     lchob
#    }
  }
}#}}}
# chartTA {{{
`chartTA` <-
function(x) {
    spacing <- x@params$spacing
    width <- x@params$width

    x.range <- x@params$xrange
    x.range <- seq(x.range[1],x.range[2]*spacing)

    tav <- x@TA.values

    if(x@new) {
      # draw new sub-window
      y.range <- if(is.null(x@params$yrange) || length(x@params$yrange) != 2) {
                   seq(min(tav * 0.975, na.rm = TRUE), max(tav * 1.05, na.rm = TRUE),
                   length.out=length(x.range))
                 } else seq(x@params$yrange[1],x@params$yrange[2],length.out=length(x.range))

      plot(x.range,y.range,type='n',axes=FALSE,ann=FALSE)
      coords <- par('usr')
      rect(coords[1],coords[3],coords[2],coords[4],col=x@params$colors$area)
      grid(NA,NULL,col=x@params$colors$grid.col)
    }

    pars <- x@params$pars[[1]]
    pars <- lapply(pars,
             function(x) {
              len <- NCOL(tav)
              if(length(x) < len) {
                rep(list(x), length.out=len)
              } else rep(list(x),length.out=len)
             })
#    pars <- x@params$pars#[[1]]
#    pars <- lapply(pars, function(x) rep(x, length.out=NCOL(tav)))

    col.order <- if(is.null(x@params$order)) {
      1:NCOL(tav)
    } else x@params$order

    if(is.null(x@params$legend)) legend <- function(legend,text.col,...) {}

    if(is.character(x@params$legend) && x@params$legend != "auto") {
      legend("topleft", legend=x@params$legend, bty='n', y.inter=0.95)
      legend <- function(legend,text.col,...) { }
    }

    if(!x@new) {
      legend <- function(legend,text.col,...) { list(legend=legend,text.col=text.col) }
    }

    #formals(legend) <- alist(legend=,text.col=,...=) #formals(graphics::legend)  # all have the same formals now
    legend.text <- list()

    # possibly able to handle newTA functionality
    if(is.null(x@params$legend.name)) x@params$legend.name <- deparse(x@call[-1][[1]])

    x.pos <- 1 + spacing * (1:length(x.range))
    if(NCOL(tav) == 1) {
      tmp.pars <- lapply(pars,function(x) x[[1]][[1]])
      if(x@params$isLogical) {
        do.call('rect',c(list(x.pos[shading(tav)$start-1] - spacing/3), list(par('usr')[3]),
                         list(x.pos[shading(tav)$end-1]   + spacing/3), list(par('usr')[4]), tmp.pars))
        # do not add a legend name for background shading.  probably better to have
        # the labels in another routine
      } else {
        do.call('lines',c(list(seq(1,length(x.range),by=spacing)), list(tav), tmp.pars))
        legend.text[[1]] <- legend('topleft',
             legend=c(paste(x@params$legend.name,":"),sprintf("%.3f",last(na.omit(tav)))),
             text.col=c(x@params$colors$fg.col,last(pars$col[[1]])),bty='n',y.inter=.95)
      }
    } else {
      for(cols in col.order) {
        tmp.pars <- lapply(pars,function(x) {
                                              p <- try(x[[cols]][[cols]],silent=TRUE)
                                              if(inherits(p, 'try-error')) {
                                                stop("TA parameter length must equal number of columns", call.=FALSE)
                                              } else p
                                            }
                          )
        do.call('lines',c(list(seq(1,length(x.range),by=spacing)), list(tav[,cols]), tmp.pars))
        if(cols==1) { 
          legend.text[[cols]] <- legend('topleft',
                 legend=c(paste(x@params$legend.name,":")),
                 text.col=c(x@params$colors$fg.col,last(pars$col[[cols]])),bty='n',y.inter=.95)
        }
        # for each column, add colname: value
        Col.title <- colnames(tav)[cols]
        legend.text[[cols]] <- legend('topleft',
               legend=c(rep('',cols),paste(Col.title,":",
                        sprintf("%.3f",last(na.omit(tav[,cols]))))),
               text.col=pars$col[[cols]][cols],bty='n',y.inter=.95)
      } 
    }

    axis(2)
    box(col=x@params$colors$fg.col)
    invisible(legend.text)
} # }}}
# chartSetUp {{{
`chartSetUp` <-
function(x) {
    spacing <- x@params$spacing
    width <- x@params$width

    x.range <- x@params$xrange
    x.range <- seq(x.range[1],x.range[2]*spacing)

    tav <- x@TA.values

    if(x@new) {
      y.range <- if(is.null(x@params$yrange) || length(x@params$yrange) != 2) {
                   seq(min(tav * 0.975, na.rm = TRUE), max(tav * 1.05, na.rm = TRUE),
                   length.out=length(x.range))
                 } else seq(x@params$yrange[1],x@params$yrange[2],length.out=length(x.range))

      plot(x.range,y.range,type='n',axes=FALSE,ann=FALSE)
      coords <- par('usr')
      rect(coords[1],coords[3],coords[2],coords[4],col=x@params$colors$area)
      grid(NA,NULL,col=x@params$colors$grid.col)
    }

    pars <- x@params$pars[[1]]
    pars <- lapply(pars,
             function(x) {
              len <- NCOL(tav)
              if(length(x) < len) {
                rep(list(x), length.out=len)
              } else rep(list(x),length.out=len)
             })
#    pars <- x@params$pars#[[1]]
#    pars <- lapply(pars, function(x) rep(x, length.out=NCOL(tav)))

    col.order <- if(is.null(x@params$order)) {
      1:NCOL(tav)
    } else x@params$order

    if(is.null(x@params$legend)) legend <- function(legend,text.col,...) {}
    if(is.character(x@params$legend) && x@params$legend != "auto") {
      legend("topleft", legend=x@params$legend, bty='n', y.inter=0.95)
      legend <- function(legend,text.col,...) { }
    }

    if(!x@new) {
      legend <- function(legend,text.col,...) { list(legend=legend,text.col=text.col) }
    }

    legend.text <- list()

    # possibly able to handle newTA functionality
    if(is.null(x@params$legend.name)) x@params$legend.name <- deparse(x@call[-1][[1]])

    if(NCOL(tav) == 1) {
      tmp.pars <- lapply(pars,function(x) x[[1]][[1]])
#      if(x@params$isLogical) {
#        do.call('rect',c(list(shading(tav)$start*spacing), list(par('usr')[3]),
#                         list(shading(tav)$end*spacing),   list(par('usr')[4]), tmp.pars))
#      } else
#      do.call('lines',c(list(seq(1,length(x.range),by=spacing)), list(tav), tmp.pars))
      legend.text[[1]] <- legend('topleft',
             legend=c(paste(x@params$legend.name,":"),sprintf("%.3f",last(na.omit(tav)))),
             text.col=c(x@params$colors$fg.col,last(pars$col[[1]])),bty='n',y.inter=.95)
    } else {
      for(cols in col.order) {
        tmp.pars <- lapply(pars,function(x) x[[cols]][[cols]])
#        do.call('lines',c(list(seq(1,length(x.range),by=spacing)), list(tav[,cols]), tmp.pars))
        if(cols==1) { 
          legend.text[[cols]] <- legend('topleft',
                 legend=c(paste(x@params$legend.name,":")),
                 text.col=c(x@params$colors$fg.col,last(pars$col[[cols]])),bty='n',y.inter=.95)
        }
        # for each column, add colname: value
        Col.title <- colnames(tav)[cols]
        legend.text[[cols]] <- legend('topleft',
               legend=c(rep('',cols),paste(Col.title,":",
                        sprintf("%.3f",last(na.omit(tav[,cols]))))),
               text.col=pars$col[[cols]][cols],bty='n',y.inter=.95)
      } 
    }

    axis(2)
    box(col=x@params$colors$fg.col)
    invisible(legend.text)
} # }}}

# setTA {{{
`setTA` <-
function(type=c('chartSeries','barChart','candleChart')) {
  if('chartSeries' %in% type) setDefaults(chartSeries,TA=listTA())
  if('barChart' %in% type) setDefaults(barChart,TA=listTA())
  if('candleChart' %in% type) setDefaults(candleChart,TA=listTA())
}# }}}
# unsetTA {{{
`unsetTA` <-
function(type=c('chartSeries','barChart','candleChart')) {
  if('chartSeries' %in% type) setDefaults(chartSeries,TA=NULL)
  if('barChart' %in% type) setDefaults(barChart,TA=NULL)
  if('candleChart' %in% type) setDefaults(candleChart,TA=NULL)
}# }}}
# listTA {{{
`listTA` <-
function(chob) {
  if(missing(chob)) chob <- get.chob()
  # return function calls of addTA
  chob$Env$call_list[-1]
  #sapply(get.chob()[[dev]]@passed.args$TA,function(x) x@call)
} # }}}

chartNULL <- function(...) return(invisible(NULL))
