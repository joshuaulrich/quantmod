"importDefaults" <-
function(calling.fun=NULL) {
  sc <- sys.call(-1)
  if(is.null(calling.fun)) calling.fun <- as.character(sc[[1]])
  if(is.function(calling.fun)) calling.fun <- deparse(substitute(calling.fun))
  if(is.null(sc)) 
    stop("importDefaults is only valid inside a function call") 
  funcall <- as.character(sc[[1]])
  funcall <- if(funcall[1] %in% c("::", ":::")) funcall[3] else funcall[1]
  if(funcall != calling.fun) return()
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
    # 'name' can be a character string or a symbol.
    # We need the character string representation of the function name so
    # we can use it to create the option name. Then we can look it up via
    # importDefaults() when the function is called.

    # Should also document that 'name' can be a symbol, but only at the top
    # level. Calls to setDefaults() (etc.) within functions must use character
    # strings to identify functions.

    is.func <-
      try({
        is.function(name)
        eval(parse(text=name))
      }, silent=TRUE)

    # 'name' can be a function name, expression, or character
    # the try() catches instances where name is an unexported symbol
    if(inherits(is.func, "try-error")) {
      # get the character representation of the symbol
      name.str <- deparse(substitute(name))
      # remove quotes in the case 'name' is already character
      name.str <- gsub("['\"]", "", name.str)

      ga.func <- getAnywhere(name.str)
      ga.objs <- ga.func[["objs"]]

      if (length(ga.objs) < 1) {
        stop("no function named '", ga.func$name, "' was found")
      }

      # check that the function body has a call to importDefaults()
      has.importDefaults <- function(fn) {
        out <- FALSE
        if (is.function(fn)) {
          chr <- as.character(body(fn))
          has <- grepl("importDefaults", chr, fixed = TRUE)
          out <- any(has)
        } else {
          out <- FALSE
        }
        out
      }
      is.valid <- sapply(ga.objs, has.importDefaults)
      is.visible <- ga.func[["visible"]]
      first.choice <- which(is.valid & is.visible)

      if(length(first.choice) < 1) {
        # first non-visible function
        first.choice <- which(is.valid)
        if(length(first.choice) < 1) {
          # nothing visible and valid
          stop("argument 'name' must be a function that contains a ",
               "call to 'importDefaults()'")
        }
      } else {
        first.choice <- first.choice[1]
      }

      name <- ga.func[["name"]]
      avail.defaults <- formals(ga.objs[[first.choice]])
    } else {
      if (is.function(name)) {
        name <- deparse(substitute(name))
      }
      func <- eval(parse(text=name))
      if (!is.function(func)) {
        stop("argument 'name' must be a function", call. = FALSE)
      }
      avail.defaults <- formals(func)
    }

    default.name <- paste(name, "Default", sep = ".")
    old.defaults <- getDefaults(name)
    new.defaults <- list(...)

    matched.defaults <- list()
    for(arg in names(new.defaults)) {
      if(!is.na(pmatch(arg,names(avail.defaults)))) {
        # if partial match is made:
        arg.name <- match.arg(arg,names(avail.defaults))
        mc <- match.call()[[arg]]
        if(is.language(mc)) mc <- eval(mc)
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

  # 'name' can be a function name, expression, or character
  # the try() catches instances where name is an unexported symbol
  name.is.function <- try(is.function(name), silent = TRUE)
  if(inherits(name.is.function, "try-error") || isTRUE(name.is.function)) {
    name <- deparse(substitute(name))
  }

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

  # 'name' can be a function name, expression, or character
  # the try() catches instances where name is an unexported symbol
  name.is.function <- try(is.function(name), silent = TRUE)
  if(inherits(name.is.function, "try-error") || isTRUE(name.is.function)) {
    name <- deparse(substitute(name))
  }

  if(!is.null(name)) {

    if(!is.character(name)) {
      fcall <- match.call()
      name <- as.character(fcall[['name']])
    }

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
