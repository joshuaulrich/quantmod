# replot {{{
new.replot <- function(frame=1,asp=1,xlim=c(1,10),ylim=list(structure(c(1,10),fixed=FALSE))) {
  # global variables
  Env <- new.env()
  Env$frame <- frame
  Env$asp   <- asp
  Env$usr   <- par("usr")
  Env$xlim  <- xlim
  Env$ylim  <- ylim
  Env$pad1 <- -0 # bottom padding per frame
  Env$pad3 <-  0 # top padding per frame 
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
  set_pad   <- function(pad) { Env$pad1 <<- pad[1]; Env$pad3 <<- pad[2] }
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
  get_pad   <- function() c(Env$pad1,Env$pad3)

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
    ylim  <- lapply(Env$ylim, function(x) structure(x + (diff(x) * c(Env$pad1, Env$pad3)),fixed=attr(x,"fixed")))
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

  # add_frame:
  #   append a plot frame to the plot window
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
  update_frames <- function(headers=TRUE) {
    # use subset code here, without the subset part.
    from_by <- ifelse(headers,2,1)  
    ylim <- get_ylim()
    for(y in seq(from_by,length(ylim),by=from_by)) {
      if(!attr(ylim[[y]],'fixed'))
        ylim[[y]] <- structure(c(Inf,-Inf),fixed=FALSE)
    }
    lapply(Env$actions,
           function(x) {
             if(!is.null(attr(x,"no.update")) && attr(x, "no.update"))
                return(NULL)
             frame <- abs(attr(x, "frame"))
             fixed <- attr(ylim[[frame]],'fixed')
             #fixed <- attr(x, "fixed")
             if(frame %% from_by == 0 && !fixed) {
               lenv <- attr(x,"env")
               if(is.list(lenv)) lenv <- lenv[[1]]
               dat.range <- range(na.omit(lenv$xdata[Env$xsubset]))
               min.tmp <- min(ylim[[frame]][1],dat.range,na.rm=TRUE)
               max.tmp <- max(ylim[[frame]][2],dat.range,na.rm=TRUE)
               ylim[[frame]] <<- structure(c(min.tmp,max.tmp),fixed=fixed)
             }
           })
    # reset all ylim values, by looking for range(env[[1]]$xdata)
    # xdata should be either coming from Env or if lenv, lenv
    set_ylim(ylim)
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
    a <- structure(x,frame=Env$frame,clip=clip,env=env,...)
    Env$actions[[length(Env$actions)+1]] <<- a
  }

  # prepare window to draw
  #set_window()
  # return
  replot_env <- new.env()
  class(replot_env) <- c("replot","environment")
  replot_env$Env <- Env
  replot_env$set_window <- set_window
  replot_env$add <- add
  replot_env$replot <- replot
  replot_env$get_actions <- get_actions
  replot_env$subset <- subset
  replot_env$update_frames <- update_frames
  replot_env$set_frame <- set_frame
  replot_env$get_frame <- get_frame
  replot_env$next_frame <- next_frame
  replot_env$add_frame <- add_frame
  replot_env$remove_frame <- remove_frame
  replot_env$set_asp <- set_asp
  replot_env$get_asp <- get_asp
  replot_env$set_xlim <- set_xlim
  replot_env$get_xlim <- get_xlim
  replot_env$reset_ylim <- reset_ylim
  replot_env$set_ylim <- set_ylim
  replot_env$get_ylim <- get_ylim
  replot_env$set_pad <- set_pad
  return(replot_env)
} # }}}

str.replot <- function(x, ...) {
  print(str(unclass(x)))
}

# print/plot replot methods {{{
print.replot <- function(x, ...) plot(x,...)
plot.replot <- function(x, ...) {
  plot.new()
  #assign(".chob",x,.GlobalEnv)
  assign(".chob",x,.plotEnv)
  cex <- par(cex=x$Env$cex)
  mar <- par(mar=x$Env$mar)
  if(.Device=="X11") # only reasonable way to fix X11/quartz issue
    par(cex=x$Env$cex * 1.5)
  oxpd <- par(xpd=FALSE)
  usr <- par("usr")
  # plot negative (underlay) actions
  last.frame <- x$get_frame()
  x$update_frames()
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
  #for(frames in 1:length(x$get_ylim())) {
    #x$set_frame(frames)
    #abline(h=x$get_ylim()[[frames]][1], col=x$Env$theme$grid, lwd=1)
  #}
  x$set_frame(abs(last.frame),clip=FALSE)
  do.call("clip",as.list(usr))
  par(xpd=oxpd,cex=cex$cex,mar=mar$mar)#,usr=usr)
  invisible(x$Env$actions)
} # }}}

# scale.ranges {{{
scale.ranges <- function(frame, asp, ranges)
{
  asp/asp[frame] * abs(diff(ranges[[frame]]))
} # }}}

`+.replot` <- function(e1, e2) {
  e2 <- match.call()$e2
  e2$plot_object <- (substitute(e1))
  eval(e2)
}

`+.replot` <- function(e1, e2) {
  assign(".chob",e1,.plotEnv)
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

