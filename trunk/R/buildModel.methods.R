"buildModel.glm" <-
function(quantmod,training.data=training.data,...)
{
	gl <- glm(formula=quantmod@model.formula,data=training.data,...);
	return(list("fitted"=gl,
                "inputs"=attr(terms(gl),"term.labels"))); 
}

"buildModel.lm" <-
function(quantmod,training.data,...)
{
	l <- lm(formula=quantmod@model.formula,data=training.data,...);
	return(list("fitted"=l,
                "inputs"=attr(terms(l),"term.labels"))); 
}

"buildModel.step" <-
function(quantmod,training.data,...)
{
    s <- step(lm(formula=quantmod@model.formula,data=training.data,...),...);
	return(list("fitted"=s,
                "inputs"=NULL)); 
}

"buildModel.loess" <-
function(quantmod,training.data,...)
{
    l <- loess(quantmod@model.formula,data=training.data,...);
	return(list("fitted"=l,
                "inputs"=attr(terms(l),"term.labels"))); 
}

####### quantile regression method 'rq' - requires package quantreg
"buildModel.rq" <-
function(quantmod,training.data,...)
{
    if(length(try(library(quantreg),FALSE)) == 1) 
        stop("Required package 'quantreg' for method 'rq' was not found\n");
    r <- rq(quantmod@model.formula,data=training.data,...);
	return(list("fitted"=r,
                "inputs"=attr(terms(r),"term.labels"))); 
}

####### resistant regression method 'lqs' - requires package MASS
"buildModel.lqs" <-
function(quantmod,training.data,...)
{
    if(length(try(library(MASS),FALSE)) == 1) 
        stop("Required package 'MASS' for method 'lqs' was not found\n");
    lq <- lqs(quantmod@model.formula,data=training.data,...);
	return(list("fitted"=lq,
                "inputs"=attr(terms(lq),"term.labels"))); 
}

####### robust regression method 'rlm' - requires package MASS
"buildModel.rlm" <-
function(quantmod,training.data,...)
{
    if(length(try(library(MASS),FALSE)) == 1) 
        stop("Required package 'MASS' for method 'rlm' was not found\n");
    rl <- rlm(quantmod@model.formula,data=training.data,...);
	return(list("fitted"=rl,
                "inputs"=attr(terms(rl),"term.labels"))); 
}

####### neural net method - requires package nnet
"buildModel.nnet" <-
function(quantmod,training.data,...)
{
    if(length(try(library(nnet),FALSE)) == 1) 
        stop("Required package 'nnet' for method 'nnet' was not found\n");
	nn <- nnet(quantmod@model.formula,data=training.data,...);
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
"buildModel.ppr" <-
function(quantmod,training.data,...)
{
	p <- ppr(quantmod@model.formula,data=training.data,...);
	return(list("fitted"=p,
                "inputs"=attr(terms(p),"term.labels"))); 
}

####### mars method - requires package mda
"buildModel.mars" <-
function(quantmod,training.data,...)
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
"buildModel.polymars" <-
function(quantmod,training.data,...)
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
"buildModel.lars" <-
function(quantmod,training.data,...)
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
"buildModel.rpart" <-
function(quantmod,training.data,...)
{
    if(length(try(library(rpart),FALSE)) == 1)
         stop("Required package 'rpart' for method 'rpart' was not found\n");
	rp <- rpart(quantmod@model.formula,data=training.data,...);
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
"buildModel.tree" <-
function(quantmod,training.data,...)
{
    if(length(try(library(tree),FALSE)) == 1)
         stop("Required package 'tree' for method 'tree' was not found\n");
	rp <- tree(quantmod@model.formula,data=training.data,...);
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
"buildModel.randomForest" <-
function(quantmod,training.data,...)
{
    if(length(try(library(randomForest),FALSE)) == 1)
         stop("Required package 'randomForest' for method 'randomForest' was not found\n");
	rp <- randomForest(quantmod@model.formula,data=training.data,...);
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

`buildModel.svm` <-
function(quantmod,training.data,...)
{
  if(length(try(library(e1071),FALSE))==1)
    stop("Required package 'e1071' for method 'svm' was not found\n")
  rp <- svm(formula(quantmod),data=training.data,...)
  return(list('fitted'=rp,attr(terms(rp),'term.labels')))
}

`predictModel.svm` <-
function(object,data,...)
{
  if(length(try(library(e1071),FALSE))==1)
    stop("Required package 'e1071' for method 'svm' was not found\n")
  predict(object,data[-NROW(data),],...) 
}
