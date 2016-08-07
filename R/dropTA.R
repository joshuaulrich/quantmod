`swapTA` <-
function(ta1,ta2,occ1=1,occ2=1,dev) {
  if(missing(ta1) | missing(ta2)) stop("two TA indicator required")

  # default to the current device if none specified  
  if(missing(dev)) dev <- dev.cur()
  ta.list <- listTA(dev)

  # get the current chob
  lchob <- get.chob()[[dev]]
  
  # make indicator name match original call
  if(regexpr("^add",ta1) == -1) ta1 <- paste("add",ta1,sep='')
  if(regexpr("^add",ta2) == -1) ta2 <- paste("add",ta2,sep='')

  # locate the TA which needs to be removed
  which.ta1 <- which(ta1==sapply(ta.list,
                             function(x) deparse(x[[1]])))[occ1]
  which.ta2 <- which(ta2==sapply(ta.list,
                             function(x) deparse(x[[1]])))[occ2]

  tmp.ta1 <- lchob@passed.args$TA[[which.ta1]]
  tmp.ta2 <- lchob@passed.args$TA[[which.ta2]]

  lchob@passed.args$TA[[which.ta1]] <- tmp.ta2
  lchob@passed.args$TA[[which.ta2]] <- tmp.ta1

  do.call("chartSeries.chob",list(lchob))
  write.chob(lchob,lchob@device)
}

`moveTA` <-
function(ta,pos,occ=1,dev) {

  pos <- pos - 1

  if(missing(ta)) stop("no TA indicator specified")

  # default to the current device if none specified  
  if(missing(dev)) dev <- dev.cur()
  ta.list <- listTA(dev)

  # get the current chob
  lchob <- get.chob()[[dev]]
  
  # make indicator name match original call
  if(regexpr("^add",ta) == -1) ta <- paste("add",ta,sep='')

  # locate the TA which needs to be removed
  which.ta <- which(ta==sapply(ta.list,
                             function(x) deparse(x[[1]])))[occ]

  if(is.na(which.ta)) stop("no TA")

  lchob@passed.args$TA <- append(lchob@passed.args$TA[-which.ta],
                                 lchob@passed.args$TA[which.ta],
                                 after=pos)

  do.call("chartSeries.chob",list(lchob))
  write.chob(lchob,lchob@device)
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
