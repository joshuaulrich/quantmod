"importDefaults" <-
function(...) {
  if(is.null(sys.call(-1))) 
    stop("importDefaults is only valid inside a function call") 
  # usage: importDefaults()
  calling.fun <- as.character(match.call(call=as.call(sys.call(-1)))[1])
  all.defaults <- getDefaults(calling.fun)
  envir <- as.environment(-1)
  passed.args <- names(sapply(match.call(call=as.call(sys.call(-1)))[-1],deparse))
  formal.args <- names(formals(as.character(sys.call(-1))))
  #default.args <- names(def[!is.null(def)])
  default.args <- names(unlist(all.defaults))
  for(arg in formal.args) {
    if(!arg %in% passed.args) {
      if(arg %in% default.args) {
        assign(arg, as.vector(unlist(all.defaults[arg])),envir=envir)
      }
    }
  }
}

"checkDefaults" <- importDefaults

"setDefaults" <-
function(name,...) {
  if(is.function(name)) name <- deparse(substitute(name))
  default.name <- paste(name,"Default",sep=".")
  all.defaults <- getDefaults(name)
  if(is.null(all.defaults)) 
    all.defaults <- sapply(names(formals(name)),function(x) {x=NULL})
  new.defaults <- list(...)
  for(arg.name in names(all.defaults)) {
    if(arg.name %in% names(new.defaults)) 
      all.defaults[[arg.name]] <- new.defaults[[arg.name]]
  }
  set.method <- paste('setDefaults',name,sep='.')
  if(!exists(set.method)) setDefaults.skeleton(name)
  do.call(set.method,all.defaults)
}

"setDefaults.skeleton" <-
function(name) {
  if(is.function(name)) name <- deparse(substitute(name))
  default.name <- paste(name,"Default",sep=".")
  sDtxt <- paste(
           "\"setDefaults.",name,"\" <-\n",
           "function(...,clear.all=FALSE) {\n",
           "  set.to <- list(...)\n",
           "  if(clear.all) set.to <- NULL\n",
           "  options(",default.name,"=set.to)\n",
           "}",sep="") 
  filename <- paste("sD",name,".tmp.R",sep="")
  cat(sDtxt, file = filename)
  source(filename)
  unlink(filename)
}

"setDefaults.mc" <-
function(...,clear.all=FALSE) {
  set.to <- list(...)
  if(clear.all) set.to <- NULL
  options(mc.Default=set.to)
}

"clearDefaults" <-
function(name,confirm=TRUE) {
  if(is.function(name)) name <- deparse(substitute(name))
  default.name <- paste(name,"Default",sep=".")
  if(confirm) {
    CONFIRMATION <- readline(prompt=
            paste("Are you sure you want to remove",name,"defaults? Y/N (N)"))
    if(toupper(substr(CONFIRMATION,1,1))!="Y") return()
  }
  do.call(paste('setDefaults',name,sep='.'),list(clear.all=TRUE))
}

"getDefaults" <-
function(name,arg=NULL) {
  if(is.function(name)) name <- deparse(substitute(name))
  default.name <- paste(name,"Default",sep=".")
  if(is.null(arg)) {
    return(options(default.name)[[1]])
  } else {
    default.list <- list()
    for(each.arg in arg) {
      default.list[[each.arg]] <- options(default.name)[[1]][[each.arg]]
    }
    return(default.list)
  }
}
