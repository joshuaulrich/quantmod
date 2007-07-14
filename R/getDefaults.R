"importDefaults" <-
function(...) {
  # usage: importDefaults()

  envir <- as.environment(-1)
  passed.args <- names(sapply(match.call(call=as.call(sys.call(-1)))[-1],deparse))
  formal.args <- names(formals(as.character(sys.call(-1))))
  default.args <- names(def[!is.null(def)])
  for(arg in formal.args) {
    if(!arg %in% passed.args) {
      if(arg %in% default.args) {
        assign(arg, as.vector(unlist(def[arg])),envir=envir)
      }
    }
  }
}

"setDefaults" <-
function(name,arg,value) {

}

"getDefaults" <-
function(name,arg) {

}
