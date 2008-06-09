".onLoad" <- function(lib,pkg) {
  cat("quantmod: Quantitative Financial Modelling Framework\n\n")
  cat("Version 0.3-6, Revision 433\n")
  cat("http://www.quantmod.com\n\n")
}

setOldClass("zoo");
setOldClass("Date");
setClass("quantmod",representation(
                    model.id="character",
		            model.spec="formula",
                    model.formula="formula",
                    model.target="character",
                    model.inputs="character",
                    build.inputs="character",
                    symbols="character",
                    product="character",
                    price.levels="zoo",
                    training.data="ANY",
                    build.date="character",
                    fitted.model="ANY",
                    model.data="zoo",
                    quantmod.version="numeric"
                    )
        );
setClass("quantmodReturn",representation(
                    results="zoo",
                    returns="zoo",
                    CAGR="numeric",
                    HPR="numeric",
                    accuracy="zoo",
                    directional.accuracy="list",
                    dist.of.returns="list",
                    returnsBy="ANY"
                    )
        );
setClass("quantmodResults",representation(
                    model="quantmod",
                    signal="zoo",
                    return="quantmodReturn"
                    )
        );
#setClass("tradeLog",representation(
#                    action="character",
#                    quantity="numeric",
#                    underlying="character",
#                    price="numeric",
#                    currency="character",
#                    date="Date",
#                    trade.id="numeric"),
#                    prototype = list(action='',
#                                     quantity=0,
#                                     underlying='',
#                                     price=0,
#                                     currency='USD',
#                                     date=as.Date('2007-01-01'),
#                                     trade.id=1)
#        )
#setMethod("show","tradeLog", 
#         function(object)
#         {
#         tradeLog <- cbind(object@date,object@trade.id,object@price,object@quantity)
#         print(zoo(tradeLog,order.by=object@date))
#         })
setMethod("show","quantmod", function(object) {
	cat("\nquantmod object:  ",
        object@model.id,"\tBuild date: ",
        paste(object@build.date),"\n");
	cat("\nModel Specified: \n    ",
        gsub("[ ]+"," ",deparse(object@model.spec)),"\n");
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
setMethod("summary","quantmod", function(object) {
	cat("\nquantmod object:  ",
        object@model.id,"\tBuild date: ",
        paste(object@build.date),"\n");
	cat("\nModel Specified: \n    ",
        gsub("[ ]+"," ",deparse(object@model.spec)),"\n");
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

setMethod("show","quantmodResults", function(object) {
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
#setGeneric("fittedModel<-", function(x,...,value) {standardGeneric("fittedModel<-")})
setGeneric("fittedModel<-", function(object,value) {standardGeneric("fittedModel<-")})
#setReplaceMethod("fittedModel","quantmod", function(x,...,value)
setReplaceMethod("fittedModel","quantmod", function(object,value)
{
    object@fitted.model <- value
    
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
'plot.quantmodResults' <-
function(x,...)
{
    ret.by <- x@return@returnsBy
    plot(ret.by,type=c('l',rep('h',ncol(ret.by)-1)),...)
}

'formula.quantmod' <-
function(x,...)
{
    x@model.formula
}

'coef.quantmod' <-
function(object,...)
{
    if(!is.null(fittedModel(object)))
    coef(fittedModel(object),...)    
}

'coefficients.quantmod' <- coef.quantmod

'fitted.quantmod' <-
function(object,...)
{
    if(!is.null(fittedModel(object)))
    fitted(fittedModel(object),...)
}

'fitted.values.quantmod' <- fitted.quantmod

'residuals.quantmod' <-
function(object,...)
{
    if(!is.null(fittedModel(object)))
    residuals(fittedModel(object,...))
}

'resid.quantmod' <- residuals.quantmod

'vcov.quantmod' <-
function(object,...)
{
    if(!is.null(fittedModel(object)))
    vcov(fittedModel(object,...))
}

'logLik.quantmod' <-
function(object, ...)
{
    if(!is.null(fittedModel(object)))
    logLik(fittedModel(object),...)
}

'anova.quantmod' <-
function(object,...)
{
    if(!is.null(fittedModel(object)))
    anova(fittedModel(object),...)
}

'plot.quantmod' <-
function(x,...)
{
    if(!is.null(fittedModel(x)))
    plot(fittedModel(x),...)
}
