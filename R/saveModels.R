"saveModels" <-
function(...,list=character(0),dir=NULL)
{
    if(is.null(dir)) {
        dir <- getOption('tR.dir');
    }
    if(is.null(dir)) stop('please specify model directory: via dir arg or options("tR.dir")');

    models.list <- as.character(substitute(list(...)))[-1];
    models <- c(list,models.list);
    if(length(models)==0) {
        for(i in ls(1)) {
            if(class(eval(parse(text=i)))[1]=='quantmod') models <- c(models,i);
        }
    }
    if(length(models) > 0) {    
        for(obj in models) {
            model.obj <- eval(parse(text=obj));
            model.obj <- stripModelData(model.obj);
            if(class(model.obj)[1]=='quantmod') 
                save(model.obj,file=paste(dir,model.obj@model.id,sep=''),envir=sys.frame(1));
                
        }
    }
}

"loadModels" <-
function(...,dir=NULL)
{
    if(is.null(dir)) {
        dir <- getOption('tR.dir');
    }
    if(is.null(dir)) stop('please specify model directory: via dir arg or options("tR.dir")');

    models <- as.character(list(...));
    if(length(models)==0) {
        stop('no file(s) specified');
    }
    if(length(models) > 0) {    
        for(obj in models) {
            thisName <- load(file=paste(dir,obj,sep=''))
            this <- eval(parse(text=thisName));
            this <- stripModelData(this);
            thisName <- this@model.id;
            assign(thisName,this,.quantmodEnv);
        }
    }

    
    
}
