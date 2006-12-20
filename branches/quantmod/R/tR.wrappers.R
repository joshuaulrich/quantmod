"tR.glm" <-
function(tR.model=tR.model,training.data=training.data,...)
{
	gl <- glm(formula=tR.model@model.formula,data=training.data,...);
	return(list("fitted"=gl,
                "inputs"=attr(terms(gl),"term.labels"))); 
}

"tR.lm" <-
function(tR.model=tR.model,training.data=training.data,...)
{
	l <- lm(formula=tR.model@model.formula,data=training.data,...);
	return(list("fitted"=l,
                "inputs"=attr(terms(l),"term.labels"))); 
}

"tR.step" <-
function(tR.model,training.data,...)
{
    s <- step(lm(formula=tR.model@model.formula,data=training.data,...),...);
	return(list("fitted"=s,
                "inputs"=NULL)); 
}

"tR.loess" <-
function(tR.model,training.data,...)
{
    l <- loess(tR.model@model.formula,data=training.data,...);
	return(list("fitted"=l,
                "inputs"=attr(terms(l),"term.labels"))); 
}

####### quantile regression method 'rq' - requires package quantreg
"tR.rq" <-
function(tR.model,training.data,...)
{
    if(length(try(library(quantreg),FALSE)) == 1) 
        stop("Required package 'quantreg' for method 'rq' was not found\n");
    r <- rq(tR.model@model.formula,data=training.data,...);
	return(list("fitted"=r,
                "inputs"=attr(terms(r),"term.labels"))); 
}

####### resistant regression method 'lqs' - requires package MASS
"tR.lqs" <-
function(tR.model,training.data,...)
{
    if(length(try(library(MASS),FALSE)) == 1) 
        stop("Required package 'MASS' for method 'lqs' was not found\n");
    lq <- lqs(tR.model@model.formula,data=training.data,...);
	return(list("fitted"=lq,
                "inputs"=attr(terms(lq),"term.labels"))); 
}

####### robust regression method 'rlm' - requires package MASS
"tR.rlm" <-
function(tR.model,training.data,...)
{
    if(length(try(library(MASS),FALSE)) == 1) 
        stop("Required package 'MASS' for method 'rlm' was not found\n");
    rl <- rlm(tR.model@model.formula,data=training.data,...);
	return(list("fitted"=rl,
                "inputs"=attr(terms(rl),"term.labels"))); 
}

####### neural net method - requires package nnet
"tR.nnet" <-
function(tR.model,training.data,...)
{
    if(length(try(library(nnet),FALSE)) == 1) 
        stop("Required package 'nnet' for method 'nnet' was not found\n");
	nn <- nnet(tR.model@model.formula,data=training.data,...);
	return(list("fitted"=nn,
                "inputs"=attr(terms(nn),"term.labels"))); 
}
"predictModel.nnet" <-
function(object,data,...)
{
    if(length(try(library(nnet),FALSE)) == 1) 
        stop("Required package 'nnet' for method 'nnet' was not found\n");
    predict(object,data,...)
}

####### projection pursuit regression method - requires stats
"tR.ppr" <-
function(tR.model,training.data,...)
{
	p <- ppr(tR.model@model.formula,data=training.data,...);
	return(list("fitted"=p,
                "inputs"=attr(terms(p),"term.labels"))); 
}

####### mars method - requires package mda
"tR.mars" <-
function(tR.model,training.data,...)
{
    if(length(try(library(mda),FALSE)) == 1)
        stop("Required package 'mda' for method 'mars' was not found\n");
	x <- training.data[,-1];
	y <- training.data[,1];
	m <- mars(x=x,y=y,...);
	return(list("fitted"=m,
                "inputs"=colnames(x))); 
}

"predictModel.mars" <-
function(object,data,...)
{
    if(length(try(library(mda),FALSE)) == 1) 
        stop("Required package 'mda' for method 'mars' was not found\n");
    predict(object,data[,-1]);
}

####### polymars method - requires package polspline
"tR.polymars" <-
function(tR.model,training.data,...)
{
    if(length(try(library(polspline),FALSE)) == 1)
        stop("Required package 'polspline' for method 'polymars' was not found\n");
	responses <- training.data[,1];
	predictors <- training.data[,-1];
	m <- polymars(responses,predictors,...);
	return(list("fitted"=m,
                "inputs"=colnames(predictors))); 
}

"predictModel.polymars" <-
function(object,data,...)
{
    if(length(try(library(polspline),FALSE)) == 1) 
        stop("Required package 'polspline' for method 'polymars' was not found\n");
    predict(object,data[,-1]);
}

####### lars method - requires package lars
"tR.lars" <-
function(tR.model,training.data,...)
{
    if(length(try(library(lars),FALSE)) == 1)
        stop("Required package 'lars' for method 'lars' was not found\n");
	x <- training.data[,-1];
	y <- training.data[,1];
	m <- lars(x=x,y=y,...);
	return(list("fitted"=m,
                "inputs"=colnames(x))); 
}

"predictModel.lars" <-
function(object,data,lars.s,...)
{
    if(length(try(library(lars),FALSE)) == 1) 
        stop("Required package 'lars' for method 'lars' was not found\n");
    lars.s = min(lars.s,object$Cp)
    predict(object,data[,-1],s=lars.s,...)$fit;
}

####### rpart method - requires package rpart
"tR.rpart" <-
function(tR.model,training.data,...)
{
    if(length(try(library(rpart),FALSE)) == 1)
         stop("Required package 'rpart' for method 'rpart' was not found\n");
	rp <- rpart(tR.model@model.formula,data=training.data,...);
	return(list("fitted"=rp,
                "inputs"=attr(terms(rp),"term.labels"))); 
}

"predictModel.rpart" <-
function(object,data,...)
{
    if(length(try(library(rpart),FALSE)) == 1)
        stop("Required package 'rpart' for method 'rpart' was not found\n");
    predict(object,data,...)
}

####### tree method - requires package tree
"tR.tree" <-
function(tR.model,training.data,...)
{
    if(length(try(library(tree),FALSE)) == 1)
         stop("Required package 'tree' for method 'tree' was not found\n");
	rp <- tree(tR.model@model.formula,data=training.data,...);
	return(list("fitted"=rp,
                "inputs"=attr(terms(rp),"term.labels"))); 
}

"predictModel.tree" <-
function(object,data,...)
{
    if(length(try(library(tree),FALSE)) == 1)
        stop("Required package 'tree' for method 'tree' was not found\n");
    predict(object,data,...)
}

####### randomForest method - requires package randomForest
"tR.randomForest" <-
function(tR.model,training.data,...)
{
    if(length(try(library(randomForest),FALSE)) == 1)
         stop("Required package 'randomForest' for method 'randomForest' was not found\n");
	rp <- randomForest(tR.model@model.formula,data=training.data,...);
	return(list("fitted"=rp,
                "inputs"=attr(terms(rp),"term.labels"))); 
}

"predictModel.randomForest" <-
function(object,data,...)
{
    if(length(try(library(randomForest),FALSE)) == 1)
        stop("Required package 'randomForest' for method 'randomForest' was not found\n");
    predict(object,data,...)
}
