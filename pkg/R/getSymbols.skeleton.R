# getSymbols.skeleton {{{
"getSymbols.skeleton" <-
function(Symbols,env,
         # additional source specific params
         return.class="zoo",
         ...) {
  importDefaults("")
  this.env <- environment()
  for(var in names(list(...))) {
    assign(var,list(...)[[var]], this.env)
  }

  # additional defaults to be saved
  # used if getSymbolLookup has been set
  # for a specific SYMBOL

  default.return.class <- return.class

  if(missing(verbose)) verbose <- FALSE
  if(missing(auto.assign)) auto.assign <- TRUE

  #################################################################
  #  Loop through all possible Symbols given in function call
  # 
  #################################################################

  for(i in 1:length(Symbols)) {

  
    #################################################################

    # repeat the following 2 assignments for all default arguments

    return.class <- getSymbolLookup()[[Symbols[[i]]]]$return.class
    return.class <- ifelse(is.null(return.class),default.return.class,
                           return.class)

    #################################################################
    
    if(verbose) cat("loading ",Symbols[[i]],".....")

    #################################################################
    # source specific code to fetch data
    # this is the core functionality of the method
    #################################################################

    fr <- # assign to 'fr'ame


    if(verbose)  
      cat("done.\n")

    #################################################################
    # convert to a zoo/xts object. indexing by proper format
    fr <- zoo(fr[,-1],as.Date(fr[,1],origin='1970-01-01'))


    # change colnames if necessary.  Following handle OHLC code from yahoo
    colnames(fr) <- paste(toupper(gsub('\\^','',Symbols[[i]])),
                          c('Open','High','Low','Close','Volume','Adjusted'),
                             sep='.')

    # convert.time.series to whichever class is specified by 'return.class'
    fr <- convert.time.series(fr=fr,return.class=return.class)

    Symbols[[i]] <-  # assign Symbol name to be used in environment
                     # make effort to make a legal R name
    #################################################################

    if(auto.assign)
      assign(Symbols[[i]],fr,env)
    }
    if(auto.assign)
      return(Symbols)
    return(fr)
}
#}}}
