`exportChart` <-
function(.type='pdf', ..., dev=2) {
  if(missing(dev))
    dev <- as.numeric(dev.cur())

  gchob <- get.chob()[[dev]]

  dim.inches <- par('din')
  resolution <- 1
  if(.type %in% c('png', 'jpeg')) resolution <- 72
  width  <- dim.inches[1] * resolution
  height <- dim.inches[2] * resolution

  export.pars <- c(list(...), list(file=paste(gchob@name,.type,sep='.'),width=width,height=height))[unique(names(c(list(file=1,width=width, height=height), 
        list(...))))]
  do.call(.type, export.pars)  # set up new device
  chartSeries.chob(gchob)
  invisible(dev.off())  # turn off device
  release.chob(length(get.chob()))  # remove from internal chob list
  message(paste(export.pars$file,"printed")
}

