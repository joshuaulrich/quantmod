add_Last <-
function(name=TRUE,last=TRUE) {
  lenv <- new.env()
  lenv$plot_axis <- function(x,show.last,show.name) {
    xdata <- x$Env$xdata
    nr <- NROW(x$Env$xdata[x$Env$xsubset])
    last_values <- last(xdata[x$Env$xsubset])
    offset <- offset_names <- NULL
    offsets <- NULL
    offset <- max(strwidth(pretty(as.numeric(last_values))))
    if(!x$Env$theme$rylab)
      offset <- 0
    offset_names <- max(strwidth(colnames(last_values)))  # only relevant to names & last
    for(i in 1:NCOL(last_values)) {
      if(!name) {
        text(nr+offset, as.numeric(last_values[,i]),
             sprintf("%+.2f",last_values[,i]), col=x$Env$theme$line.col[i],
             cex=.9,font=2,pos=4,xpd=TRUE)
      } else if(!last) {
        text(nr+offset, as.numeric(last_values[,i]),
             colnames(last_values)[i], col=x$Env$theme$line.col[i],
             cex=.9,font=2,pos=4,xpd=TRUE)
      } else {
        text((nr+offset)+c(0,offset_names), as.numeric(last_values[,i]),
             c(colnames(last_values)[i],sprintf("%+.2f",last_values[,i])),
             col=c(x$Env$theme$line.col[i],'black'),
             cex=.9,font=c(2,1),pos=4,xpd=TRUE)
      }
    }
  }
  #exp <- expression(plot_axis(x=current.chob(),show.last=last,show.name=name))
  exp <- parse(text = gsub("list", "plot_axis", as.expression(substitute(list(x = current.chob(), 
               show.last=last, show.name=name,...)))), srcfile = NULL)
  plot_object <- quantmod:::current.chob()
  plot_object$Env$mar <- c(3,1,0,if(name & last) if(plot_object$Env$theme$rylab) 5 else 4 else 3)
  lenv$xdata <- plot_object$Env$xdata
  plot_object$set_frame(2,clip=FALSE)
  plot_object$add(exp, env=c(lenv,plot_object$Env), expr=TRUE, clip=FALSE)
  plot_object
}

add_axis <-
function(side, at=NULL, labels=TRUE, tick=TRUE, line=NA, pos=NA, font=NA, col=NULL) {
  lenv <- new.env()
  lenv$plot_axis <- function(x,side,at,labels,tick,font,pos,col) {
    xdata <- x$Env$xdata
    if(is.OHLC(xdata))
      xdata <- OHLC(xdata)
    xsubset <- x$Env$xsubset
    nr <- NROW(x$Env$xdata[x$Env$xsubset])
    if(is.logical(labels) && labels==TRUE) {
      labels <- pretty(xdata[xsubset])
      dropped_label <- which(labels < min(xdata[xsubset],na.rm=TRUE))
      labels <- labels[-dropped_label]
    } 
    if(is.null(at))
      at <- labels
    if(side==2) {
      nr <- 0
    }
    text(nr, at, labels, col=col,
         cex=.9,font=font,pos=pos,xpd=TRUE)
    if(tick)
      segments(nr-(1/8 * max(strwidth(labels))),at,
               nr+(1/8 * max(strwidth(labels))),at)
  }
  if(missing(pos))
    pos <- side
  mapply(function(name, value) {
      assign(name, value, envir = lenv)
  }, names(list(side=side,at=at,labels=labels,font=font,tick=tick,pos=pos,col=col)),
     list(side=side,at=at,labels=labels,font=font,tick=tick,pos=pos,col=col))
  exp <- parse(text = gsub("list", "plot_axis", as.expression(substitute(list(x = current.chob(), 
               side=side, at=get("at"), labels=get("labels"), tick=tick,
               font=font,pos=pos, col=col)))), srcfile = NULL)
  plot_object <- quantmod:::current.chob()
  lenv$xdata <- plot_object$Env$xdata
  plot_object$set_frame(2)
  plot_object$add(exp, env=c(lenv,plot_object$Env), expr=TRUE)
  plot_object
}

add_title <-
function(main=NULL, sub=NULL, xlab=NULL, ylab=NULL, line=NA, ...) {
  lenv <- new.env()
  lenv$plot_title <- function(x,main,sub,xlab,ylab,line,side,font,pos) {
    xdata <- x$Env$xdata
    if(is.OHLC(xdata))
      xdata <- OHLC(xdata)
    xsubset <- x$Env$xsubset
    nr <- NROW(x$Env$xdata[x$Env$xsubset])
    if(is.logical(labels) && labels==TRUE) {
      labels <- pretty(xdata[xsubset])
      dropped_label <- which(labels < min(xdata[xsubset],na.rm=TRUE))
      labels <- labels[-dropped_label]
    } 
    if(is.null(at))
      at <- labels
    if(side==2) {
      nr <- 0
    }
    text(nr, at, labels, col=col,
         cex=.9,font=font,pos=pos,xpd=TRUE)
    segments(nr-(1/8 * max(strwidth(labels))),at,
             nr+(1/8 * max(strwidth(labels))),at)
  }
  #if(missing(pos))
  #  pos <- side
  mapply(function(name, value) {
      assign(name, value, envir = lenv)
  }, names(list(main=main,sub=sub,xlab=xlab,ylab=ylab,line=line)),
     list(main=main,sub=sub,xlab=xlab,ylab=ylab,line=line))
  exp <- parse(text = gsub("list", "plot_title", as.expression(substitute(list(x = current.chob(), 
               side=side, at=get("at"), labels=get("labels"), font=font,pos=pos, col=col)))), srcfile = NULL)
  plot_object <- quantmod:::current.chob()
  lenv$xdata <- plot_object$Env$xdata
  plot_object$set_frame(2)
  plot_object$add(exp, env=c(lenv,plot_object$Env), expr=TRUE)
  plot_object
}
