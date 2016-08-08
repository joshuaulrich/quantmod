`swapTA` <-
function(ta1,ta2,occ1=1,occ2=1,chob) {
  if(missing(ta1) | missing(ta2)) stop("two TA indicator required")

  # default to the current chob if none specified  
  if(missing(chob)) chob <- get.chob()
  ta.list <- listTA(chob)

  # get the current chob
  lchob <- chob
  
  # make indicator name match original call
  if(regexpr("^add",ta1) == -1) ta1 <- paste("add",ta1,sep='')
  if(regexpr("^add",ta2) == -1) ta2 <- paste("add",ta2,sep='')

  # locate the TA which needs to be removed
  which.ta1 <- which(ta1==sapply(ta.list,
                             function(x) deparse(x[[1]])))[occ1]
  which.ta2 <- which(ta2==sapply(ta.list,
                             function(x) deparse(x[[1]])))[occ2]

  ### swap two TAs without temporary storage
  
  ta.seq <- seq_along(ta.list)
  ta.swap <- replace(ta.seq, c(which.ta1, which.ta2), ta.seq[c(which.ta2, which.ta1)])
  lchob$Env$TA <- lchob$Env$TA[ta.swap]
  lchob$Env$call_list[-1] <- lchob$Env$call_list[1 + ta.swap]
  # swap frames
  frame <- sapply(lchob$Env$actions[9+c(which.ta1, which.ta2)], function(x) attr(x, "frame"))
  attr(lchob$Env$actions[[9+which.ta1]], "frame") <- frame[2]
  attr(lchob$Env$actions[[9+which.ta2]], "frame") <- frame[1]
  # swap actions
  lchob$Env$actions[-c(1:9)] <- lchob$Env$actions[9+ta.swap]
  # swap y limits
  lchob$Env$ylim[frame] <- lchob$Env$ylim[rev(frame)]
  
  ### End swap

  lchob
  #write.chob(lchob,lchob@device)
}

`moveTA` <-
function(ta,pos,occ=1,chob) {

  pos <- pos - 1

  if(missing(ta)) stop("no TA indicator specified")

  # default to the current chob if none specified  
  if(missing(chob)) chob <- get.chob()
  ta.list <- listTA(chob)

  # get the current chob
  lchob <- chob
  
  # make indicator name match original call
  if(regexpr("^add",ta) == -1) ta <- paste("add",ta,sep='')

  # locate the TA which needs to be removed
  which.ta <- which(ta==sapply(ta.list,
                             function(x) deparse(x[[1]])))[occ]

  if(is.na(which.ta)) stop("no TA")

  lchob$Env$TA <- append(lchob$Env$TA[-which.ta],
                                 lchob$Env$TA[which.ta],
                                 after=pos)
  lchob$Env$call_list <- append(lchob$Env$call_list[-(1+which.ta)],
                                lchob$Env$call_list[1+which.ta],
                                after=pos+1)
  # move actions
  lchob$Env$actions <- append(lchob$Env$actions[-(9+which.ta)],
                              lchob$Env$actions[9+which.ta],
                              after=pos+9)
  
  lchob
  #write.chob(lchob,lchob@device)
}

`dropTA` <-
function(ta,occ=1,chob,all=FALSE) {

  if(all) return(do.call('dropTA', list(1:length(listTA()))))

  if(missing(ta)) stop("no TA indicator specified")

  # get the current chob
  if(missing(chob)) chob <- get.chob()
  ta.list <- listTA(chob)
  
  sel.ta <- NULL

  for(cta in 1:length(ta)) {

    if(is.character(ta[cta])) {
      # make indicator name match original call
      if(regexpr("^add",ta[cta]) == -1) ta[cta] <- paste("add",ta[cta],sep='')
  
      # locate the TA which needs to be removed
      which.ta <- which(ta[cta]==sapply(ta.list,
                                 function(x) deparse(x[[1]])))[occ]
    } else which.ta <- cta

    # skip and warn if no indicator found  
    if(!is.na(which.ta)) {
    
      # decrease window count if necessary
      #if(lchob@passed.args$TA[[which.ta]]@new)
      #  lchob@windows <- lchob@windows - 1
   
      sel.ta <- c(sel.ta,which.ta)
    } else {
      stop("nothing to remove")
    }
  }

  if(is.null(sel.ta)) stop("nothing to remove")

  # remove TA from current list 
  ta.list <- ta.list[-sel.ta]
  for(li in sel.ta) {
    # number of actions of chartSeries object without TA is 9
    frame <- attr(chob$Env$actions[[9 + sel.ta]], "frame")
    if(abs(frame)==2) 
      chob$Env$actions[[9 + sel.ta]] <- NULL
    else
      chob$remove_frame(frame)
    chob$Env$TA[[sel.ta]] <- NULL
    ncalls <- length(chob$Env$call_list)
    # plot.xts(...) is included in call_list but listTA() is not
    chob$Env$call_list[1 + sel.ta] <- NULL
  }

  # redraw chart
  chob

  #write.chob(lchob,lchob@device)
}
