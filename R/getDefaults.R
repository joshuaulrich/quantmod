"importDefaults" <-
function(...) {
  if(is.null(sys.call(-1))) 
    stop("importDefaults is only valid inside a function call") 
  calling.fun <- as.character(match.call(call=as.call(sys.call(-1)))[1])
  all.defaults <- getDefaults(calling.fun)
  envir <- as.environment(-1)
  passed.args <- names(sapply(match.call(call=as.call(sys.call(-1)))[-1],deparse))
  formal.args <- names(formals(as.character(sys.call(-1))))
  default.args <- names(which(sapply(all.defaults,function(x) !is.null(x))==TRUE))
  for(arg in formal.args) {
    if(!arg %in% passed.args) {
      if(arg %in% default.args) {
        if(typeof(all.defaults[arg])=='list') {
          assign(arg, as.vector(all.defaults[arg][[1]]),envir=envir)
        } else {
          assign(arg, as.vector(unlist(all.defaults[arg][[1]])),envir=envir)
        }
      }
    }
  }
}

"useDefaults" <-
function(name)
{
  if(is.function(name)) name <- deparse(substitute(name))
  env <- as.environment(-1)
  new.fun.body <- as.call(parse(text=
                  c(deparse(body(name))[1],
                  'importDefaults()',
                  deparse(body(name))[-1])))[[1]]
  if(exists(name,env,inherits=FALSE))
    assign(paste('.',name,'.orig',sep=''),get(name,env),env)
  assign(name,as.function(c(formals(name),new.fun.body)),env)
}

"unDefaults" <-
function(name)
{
  if(is.function(name)) name <- deparse(substitute(name))
  env <- as.environment(-1)
  if(exists(paste('.',name,'.orig',sep=''),env,inherits=FALSE)) {
    assign(name,get(paste('.',name,'.orig',sep=''),env),env)
    remove(list=paste('.',name,'.orig',sep=''),envir=env)
  } else {
    remove(list=name,envir=env)
  }
}

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
  env <- as.environment(-1)
  eval(parse(text=paste('options(',default.name,'=list(',
       paste(paste(names(all.defaults),'=',
       lapply(all.defaults,function(x) {
         if(is.character(x)) {
           deparse(x)
         } else {
           x
         }})),collapse=','),'))',
       sep='')),envir=env)
}

"unsetDefaults" <-
function(name,confirm=TRUE) {
  if(is.function(name)) name <- deparse(substitute(name))
  default.name <- paste(name,"Default",sep=".")
  if(confirm) {
    CONFIRMATION <- readline(prompt=
            paste("Are you sure you want to remove",name,"defaults? Y/N (N) "))
    if(toupper(substr(CONFIRMATION,1,1))!="Y") {
      warning("Nothing removed!")
      invisible()
    }
  }
  env <- as.environment(-1)
  eval(parse(text=paste('options(',default.name,'=NULL)',sep='')),envir=env)
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
