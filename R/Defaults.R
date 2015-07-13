"importDefaults" <-
function(calling.fun=NULL) {
  sc <- sys.call(-1)
  if(is.null(calling.fun)) calling.fun <- as.character(sc[[1]])
  if(is.function(calling.fun)) calling.fun <- deparse(substitute(calling.fun))
  if(is.null(sc)) 
    stop("importDefaults is only valid inside a function call") 
  if(as.character(sc[[1]]) != calling.fun) return()
  #calling.fun <- as.character(match.call(call=as.call(sys.call(-1)))[1])
  all.defaults <- getDefaults(calling.fun)
  if(is.null(all.defaults)) return()
  envir <- as.environment(-1)
  #passed.args <- names(sapply(match.call(call=as.call(sys.call(-1)))[-1],deparse))
  passed.args <- names(as.list(match.call(
                       definition=eval(parse(text=calling.fun)),
                       call=as.call(sys.call(-1)))))[-1]
  formal.args <- names(formals(as.character(sys.call(-1))))
  default.args <- names(which(sapply(all.defaults,function(x) !is.null(x))==TRUE))
  for(arg in formal.args) {
    if(!arg %in% passed.args) {
      if(arg %in% default.args) {
        if(typeof(all.defaults[arg][[1]])=='list') {
          assign(arg, as.vector(all.defaults[arg][[1]]),envir=envir)
        } 
        else if(typeof(all.defaults[arg][[1]]) %in% c('symbol','language')) {
          assign(arg, all.defaults[arg][[1]],envir=envir)
        }
        else if(typeof(all.defaults[arg][[1]])=="character") {
           if(length(all.defaults[arg][[1]])==1) {
             assign(arg, eval(parse(text=all.defaults[arg][[1]])),envir=envir)
           } else {
             assign(arg, as.character(parse(text=all.defaults[arg][[1]])),envir=envir)
           }
        }
        else {
          assign(arg, as.vector(unlist(all.defaults[arg][[1]])),envir=envir)
        }
      }
    }
  }
}
`setDefaults` <-
function (name, ...) 
{
    if (is.function(name)) 
        name <- deparse(substitute(name))
    if(!is.function(eval(parse(text=name))))
      stop("argument 'name' must be a function")

    default.name <- paste(name, "Default", sep = ".")
    old.defaults <- getDefaults(name)
    new.defaults <- list(...)
    avail.defaults <- formals(name)
    matched.defaults <- list()
    for(arg in names(new.defaults)) {
      if(!is.na(pmatch(arg,names(avail.defaults)))) {
        # if partial match is made:
        arg.name <- match.arg(arg,names(avail.defaults))
        mc <- match.call()[[arg]]
        if(typeof(mc)=='language') mc <- eval(mc)
        if(is.character(mc))
            new.defaults[[arg]] <-  paste("'", mc, "'", sep = "")
        if(is.name(mc))
            new.defaults[[arg]] <- as.character(mc)
        matched.defaults[[arg.name]] <- new.defaults[[arg]]       
        if(is.null(new.defaults[[arg]])) old.defaults[[arg.name]]<-NULL
      } else {
        warning(paste(
                sQuote(arg),"was not set, possibly not a formal arg for",
                sQuote(name)))
      }
    }
    # merge original and new, then take first value only
    all.and.matched <- c(matched.defaults,old.defaults)
    all.and.matched <- all.and.matched[unique(names(all.and.matched))]
    if (length(all.and.matched) == 0) {
      if(!is.null(getDefaults(name)))  unsetDefaults(name, confirm = FALSE)
    }
    else {
        env <- as.environment(-1)
        default.deparse <- function(x) {
          if (is.character(x))
            # paste into single string (deparse may return length > 1)
            paste(deparse(x), sep="", collapse="")
          else
            x
        }
        default.values <- lapply(all.and.matched, default.deparse)
        default.list <- paste(names(all.and.matched), "=", default.values)
        eval(parse(text = paste("options(", default.name, "=list(", 
            paste(default.list, collapse = ","), "))", sep = "")), envir = env)
    }
}


`unsetDefaults` <-
function(name,confirm=TRUE) {
  importDefaults(calling.fun='unsetDefaults')
  if(is.function(name)) name <- deparse(substitute(name))
  if(is.null(getDefaults(name))) 
    invisible(return())
    #stop(paste("no Defaults set for",sQuote(name)))
  remove.yes <- TRUE
  if(confirm) {
    CONFIRMATION <- readline(prompt=
            paste("Are you sure you want to remove",
                  sQuote(name),"defaults? (N): "))
    if(toupper(substr(CONFIRMATION,1,1))!="Y") {
      remove.yes <- FALSE
      cat(paste(sQuote(name),"Defaults NOT removed\n"))
    } else {
      if(confirm)
        cat(paste(sQuote(name),"Defaults removed!\n"))
    }
  }
  if(remove.yes) {
    default.name <- paste(name,"Default",sep=".")
    env <- as.environment(-1)
    eval(parse(text=paste('options(',default.name,'=NULL)',sep='')),envir=env)
  }
}
"getDefaults" <-
function(name=NULL,arg=NULL) {
  if(is.function(name)) name <- deparse(substitute(name))
  if(!is.null(name)) {
    if(length(name) > 1) {
      if(!is.character(name))
        stop(paste(sQuote('name'),"must be a character vector",
                   "or visible function")) 
      all.names=list()
    }
    for(each.name in name) {
      default.name <- paste(each.name,"Default",sep=".")
      if(is.null(arg)) {
        if(exists('all.names',inherits=FALSE)) {
          all.names[[each.name]] <- options(default.name)[[1]]
        } else {
          return(options(default.name)[[1]])
        }
      } else {
        default.list <- list()
        for(each.arg in arg) {
          default.list[[each.arg]] <- options(default.name)[[1]][[each.arg]]
        }
        if(exists('all.names',inherits=FALSE)) {
          all.names[[each.name]] <- default.list
        } else {
          return(default.list)
        }
      }
    }
    return(all.names)
  } else {
    all.options <- names(options())
    all.Defaults <-as.character(
                     sapply(all.options[grep('.Default$',all.options)],
                       FUN=function(x) {
                         gsub('.Default$','',x)
                       })
                   )
    if(identical(all.Defaults,character(0))) return(NULL)
    return(all.Defaults)
  }
}
