".onLoad" <- function(lib,pkg) {
  require(methods,quietly=TRUE);
  require(zoo,quietly=TRUE);
  require(RMySQL,quietly=TRUE);
}
setOldClass("zoo");
setOldClass("Date");
setClass("tR.model",representation(
                    model.id="character",
		            model.spec="formula",
                    model.formula="formula",
                    model.target="character",
                    model.inputs="character",
                    build.inputs="character",
                    symbols="character",
                    product="character",
                    price.levels="zoo",
                    training.data="Date",
                    build.date="Date",
                    fitted.model="ANY",
                    model.data="zoo"
                    )
        );
setClass("tR.return",representation(
                    results="zoo",
                    returns="zoo",
                    CAGR="numeric",
                    HPR="numeric",
                    accuracy="zoo",
                    dist.of.returns="list",
                    returnsBy="zoo"
                    )
        );
setClass("tR.results",representation(
                    model="tR.model",
                    signal="zoo",
                    return="tR.return"
                    )
        );
setMethod("show","tR.model", function(object) {
	cat("\ntR.model object:  ",
        object@model.id,"\tBuild date: ",
        paste(object@build.date),"\n");
	cat("\nModel Specified: \n    ",
        gsub("\s+"," ",deparse(object@model.spec)),"\n");
        cat("\nModel Target: ",object@model.target,"\t\t",
            "Product: ",object@product,"\n");
	cat("Model Inputs: ",
        paste(object@model.inputs,collapse=", "),"\n\n");
	cat("Fitted Model: \n\n");
	if(class(object@fitted.model)[1]=="NULL") {
		cat("\tNone Fitted\n");
	} else {
		cat("\tModelling procedure: ",
        class(object@fitted.model),"\n");
	cat("\tTraining window: ",
        length(object@training.data)," observations from ",
        paste(object@training.data[c(1,length(object@training.data))],
        collapse=" to "));
	cat("\n")
    print(object@fitted.model) 
	}
}
)
setMethod("summary","tR.model", function(object) {
	cat("\ntR.model object:  ",
        object@model.id,"\tBuild date: ",
        paste(object@build.date),"\n");
	cat("\nModel Specified: \n    ",
        gsub("\s+"," ",deparse(object@model.spec)),"\n");
        cat("\nModel Target: ",object@model.target,"\t\t",
            "Product: ",object@product,"\n");
	cat("Model Inputs: ",
        paste(object@model.inputs,collapse=", "),"\n\n");
	cat("Fitted Model: \n\n");
	if(class(object@fitted.model)[1]=="NULL") {
		cat("\tNone Fitted\n");
	} else {
		cat("\tModelling procedure: ",
        class(object@fitted.model),"\n");
	cat("\tTraining window: ",
        length(object@training.data)," observations from ",
        paste(object@training.data[c(1,length(object@training.data))],
        collapse=" to "));
	cat("\n")
    summary(object@fitted.model) 
    }
})

setMethod("show","tR.results", function(object) {
    cat("\n  Model: ",object@model@model.id,"\n")
	cat("\n  C.A.G.R.: ",sprintf("%04.2f%%",object@return@CAGR*100),"\tH.P.R.: ",
        sprintf("%04.2f%%",object@return@HPR*100),"\n");
    to.date.ret <- sprintf("%04.2f%%",object@return@returnsBy[NROW(object@return@returnsBy),-1]*100)
    to.date.ret <- as.data.frame(t(to.date.ret),row.names="            ")

    colnames(to.date.ret) <- colnames(object@return@returnsBy[,-1])
    cat("\n  Returns by period summary:\n\n")
    print(as.data.frame(lapply(as.data.frame(object@return@returnsBy[,-1]), 
            function(x) sprintf("%04.2f%%",(rev(as.numeric(summary(x))[1:6]*100)))),
            row.names=c('    Max.','    3rd Qu.','    Mean','    Median','    2rd Qu.','    Min.')))
    cat("\n  Period to date returns:\n\n")
    print(to.date.ret)
}
)

"fittedModel"<-function(object) {object@fitted.model}
setGeneric("fittedModel<-", function(x,...,value) {standardGeneric("fittedModel<-")})
setReplaceMethod("fittedModel","tR.model", function(x,...,value)
{
    x@fitted.model <- value
    
}
)
## setGeneric('plot', function(x,y,...) { standardGeneric('plot') });
## setMethod("plot","tR.results", function(x,y,...) {
##     object <- x
##     ret.by <- object@return@returnsBy
##     plot(ret.by,type=c('l',rep('h',ncol(ret.by)-1)))
## }
## )
## setMethod("plot",signature("ANY","ANY"),function(x,y,...) { UseMethod('plot') } )
#####################################################
###
### Default S3 method and definition for predictModel
###
#####################################################

"predictModel" <-
function(object,data,...)
{
    UseMethod("predictModel");
}
"predictModel.default" <-
function(object,data,...)
{
    predict(object,data,...);
}
'plot.tR.results' <-
function(object,...)
{
    ret.by <- object@return@returnsBy
    plot(ret.by,type=c('l',rep('h',ncol(ret.by)-1)),...)
}
