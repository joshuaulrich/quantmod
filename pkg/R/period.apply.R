#`period.apply0` <-
#function(x,INDEX,FUN,...)
#{
#  FUN <- match.fun(FUN);
#  y <- NULL;
#  for(i in 1:(length(INDEX)-1))
#  {
#    sindex <- (INDEX[i]+1):INDEX[i+1];
#    dat <- x[sindex];
#    y <- c(y,FUN(dat,...));
#  }
#  return(y);
#}
#`period.apply` <-
#function(x,INDEX,FUN,...)
#{
#  FUN <- match.fun(FUN)
#  sapply(1:(length(INDEX)-1),
#    function(y) {
#      FUN(x[(INDEX[y]+1):INDEX[y+1]],...)
#    })
#}
#`apply.daily` <-
#function(x,FUN)
#{
#  bp <- breakpoints(x,days)
#  period.apply(x,bp,FUN)
#}
#`apply.weekly` <-
#function(x,FUN)
#{
#  bp <- breakpoints(x,weeks)
#  period.apply(x,bp,FUN)
#}
#
#`apply.monthly` <-
#function(x,FUN)
#{
#  bp <- breakpoints(x,months,TRUE)
#  period.apply(x,bp,FUN)
#}
#
#`apply.quarterly` <-
#function(x,FUN)
#{
#  bp <- breakpoints(x,quarters)
#  period.apply(x,bp,FUN)
#}
#
#`apply.yearly` <-
#function(x,FUN)
#{
#  bp <- breakpoints(x,years)
#  period.apply(x,bp,FUN)
#}
#
#`first` <-
#function(x,...)
#{
#  UseMethod("first")
#}
#
#`first.default` <-
#function(x,n=1,keep=FALSE,...)
#{
#  if(is.null(dim(x))) {
#    if(n > 0) {
#      xx <- x[1:n]
#      if(keep) xx <- structure(xx,keep=x[(-(-n)+1):NROW(x)])
#      xx
#    } else {
#      xx <- x[(-n+1):NROW(x)]
#      if(keep) xx <- structure(xx,keep=x[1:(-n)])
#      xx
#    }
#  } else {
#    if(n > 0) {
#      xx <- x[1:n,]
#      if(keep) xx <- structure(xx,keep=x[(-(-n)+1):NROW(x),])
#      xx
#    } else {
#      xx <- x[(-n+1):NROW(x),]
#      if(keep) xx <- structure(xx,keep=x[1:(-n),])
#      xx
#    }
#  }
#}
#
#`last` <-
#function(x,...)
#{
#  UseMethod("last")
#}
#
#`last.default` <-
#function(x,n=1,keep=FALSE,...)
#{
#  if(is.null(dim(x))) {
#    if(n > 0) {
#      xx <- x[(NROW(x)-n+1):NROW(x)]
#      if(keep) xx <- structure(xx,keep=x[1:(NROW(x)+(-n))])
#      xx
#    } else {
#      xx <- x[1:(NROW(x)+n)]
#      if(keep) xx <- structure(xx,keep=x[((NROW(x)-(-n)+1):NROW(x))])
#      xx
#    }
#  } else {
#    if(n > 0) {
#      xx <- x[(NROW(x)-n+1):NROW(x),]
#      if(keep) xx <- structure(xx,keep=x[1:(NROW(x)+(-n)),])
#      xx
#    } else {
#      xx <- x[1:(NROW(x)+n),]
#      if(keep) xx <- structure(xx,keep=x[((NROW(x)-(-n)+1):NROW(x)),])
#      xx
#    }
#  }
#}
#
#`last.zoo` <-
#function(x,n=1,keep=FALSE,...)
#{
#  if(is.character(n)) {
#    if(!inherits(index(x),'POSIXt') && !inherits(index(x),'Date'))
#      stop(paste('subsetting by date is only possible with objects having',
#           'time based indexes'))
#    # n period set
#    np <- strsplit(n," ",fixed=TRUE)[[1]]
#    if(length(np) > 2 || length(np) < 1)
#      stop(paste("incorrectly specified",sQuote("n"),sep=" "))
#    # series periodicity
#    sp <- periodicity(x)
#    # requested periodicity$units
#    rpu <- np[length(np)]
#    rpf <- ifelse(length(np) > 1, as.numeric(np[1]), 1)
#    if(rpu == sp$unit) {
#      n <- rpf    
#    } else {
#      # if singular - add an s to make it work
#      if(substr(rpu,length(strsplit(rpu,'')[[1]]),length(strsplit(rpu,'')[[1]])) != 's')
#        rpu <- paste(rpu,'s',sep='')
#      u.list <- list(secs=4,seconds=4,mins=3,minutes=3,hours=2,days=1,
#                     weeks=1,months=1,quarters=1,years=1)
#      dt.options <- c('seconds','secs','minutes','mins','hours','days',
#                      'weeks','months','quarters','years')
#      if(!rpu %in% dt.options)
#        stop(paste("n must be numeric or use",paste(dt.options,collapse=',')))
#      dt <- dt.options[pmatch(rpu,dt.options)]
#      if(u.list[[dt]] > u.list[[sp$unit]]) {
#        #  req is for higher freq data period e.g. 100 mins of daily data
#        stop(paste("At present, without some sort of magic, it isn't possible",
#             "to resolve",rpu,"from",sp$scale,"data"))
#      }
#      bp <- breakpoints(x,dt,TRUE)
#      if(rpf > length(bp)-1) {
#        rpf <- length(bp)-1
#        warning("requested length is greater than original")
#      }
#      if(rpf > 0) {
#        n <- bp[length(bp)-rpf]+1
#        xx <- x[n:NROW(x)]
#        if(keep) xx <- structure(xx,keep=x[1:(bp[length(bp)+(-rpf)])])
#        return(xx)
#      } else {
#        n <- bp[length(bp)+rpf]
#        xx <- x[1:n]
#        if(keep) xx <- structure(xx,keep=x[(bp[length(bp)-(-rpf)]+1):NROW(x)])
#        return(xx)
#      }
#    }
#  }
#  if(length(n) != 1) stop("n must be of length 1")
#  if(n > 0) {
#    xx <- x[((NROW(x)-n+1):NROW(x))]
#    if(keep) xx <- structure(xx,keep=x[1:(NROW(x)+(-n))])
#    xx
#  } else {
#    xx <- x[1:(NROW(x)+n)]
#    if(keep) xx <- structure(xx,keep=x[((NROW(x)-(-n)+1):NROW(x))])
#    xx
#  }
#}
#`first.zoo` <-
#function(x,n=1,keep=FALSE,...)
#{
#  if(is.character(n)) {
#    # n period set
#    if(!inherits(index(x),'POSIXt') && !inherits(index(x),'Date'))
#      stop(paste('subsetting by date is only possible with objects having',
#           'time based indexes'))
#    np <- strsplit(n," ",fixed=TRUE)[[1]]
#    if(length(np) > 2 || length(np) < 1)
#      stop(paste("incorrectly specified",sQuote("n"),sep=" "))
#    # series periodicity
#    sp <- periodicity(x)
#    # requested periodicity$units
#    rpu <- np[length(np)]
#    rpf <- ifelse(length(np) > 1, as.numeric(np[1]), 1)
#    if(rpu == sp$unit) {
#      n <- rpf    
#    } else {
#      # if singular - add an s to make it work
#      if(substr(rpu,length(strsplit(rpu,'')[[1]]),length(strsplit(rpu,'')[[1]])) != 's')
#        rpu <- paste(rpu,'s',sep='')
#      u.list <- list(secs=4,seconds=4,mins=3,minutes=3,hours=2,days=1,
#                     weeks=1,months=1,quarters=1,years=1)
#      dt.options <- c('seconds','secs','minutes','mins','hours','days',
#                      'weeks','months','quarters','years')
#      if(!rpu %in% dt.options)
#        stop(paste("n must be numeric or use",paste(dt.options,collapse=',')))
#      dt <- dt.options[pmatch(rpu,dt.options)]
#      if(u.list[[dt]] > u.list[[sp$unit]]) {
#        #  req is for higher freq data period e.g. 100 mins of daily data
#        stop(paste("At present, without some sort of magic, it isn't possible",
#             "to resolve",rpu,"from",sp$scale,"data"))
#      }
#      bp <- breakpoints(x,dt,TRUE)
#      if(rpf > length(bp)-1) {
#        rpf <- length(bp)-1
#        warning("requested length is greater than original")
#      }
#      if(rpf > 0) {
#        n <- bp[rpf+1]
#        xx <- (x[1:n])
#        if(keep) xx <- structure(xx,keep=x[(bp[-(-rpf)+1]+1):NROW(x)])
#        return(xx)
#      } else {
#        n <- bp[-rpf+1]+1
#        xx <- (x[n:NROW(x)])
#        if(keep) xx <- structure(xx,keep=x[1:(bp[-rpf+1])])
#        return(xx)
#      }      
#    }
#  }
#  if(length(n) != 1) stop("n must be of length 1")
#  if(n > 0) {
#    xx <- x[1:n]
#    if(keep) xx <- structure(xx,keep=x[(-(-n)+1):NROW(x)])
#    xx
#  } else {
#    xx <- x[(-n+1):NROW(x)]
#    if(keep) xx <- structure(xx,keep=x[1:(-n)])
#    xx
#  }
#}
