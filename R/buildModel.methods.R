`buildModel.glm` <-
function(quantmod,training.data=training.data,...)
{
	gl <- glm(formula=quantmod@model.formula,data=training.data,...);
	return(list("fitted"=gl,
                "inputs"=attr(terms(gl),"term.labels"))); 
}

`buildModel.lm` <-
function(quantmod,training.data,...)
{
	l <- lm(formula=quantmod@model.formula,data=training.data,...);
	return(list("fitted"=l,
                "inputs"=attr(terms(l),"term.labels"))); 
}

`buildModel.step` <-
function(quantmod,training.data,...)
{
    s <- step(lm(formula=quantmod@model.formula,data=training.data,...),...);
	return(list("fitted"=s,
                "inputs"=NULL)); 
}

`buildModel.loess` <-
function(quantmod,training.data,...)
{
    l <- loess(quantmod@model.formula,data=training.data,...);
	return(list("fitted"=l,
                "inputs"=attr(terms(l),"term.labels"))); 
}

####### quantile regression method 'rq' - requires package quantreg
`buildModel.rq` <-
function(quantmod,training.data,...)
{
  if(is.method.available('rq','quantreg')) {
    #r <- rq(quantmod@model.formula,data=training.data,...)
    r <- do.call('rq',list(quantmod@model.formula,data=training.data,...))
	return(list("fitted"=r,
                "inputs"=attr(terms(r),"term.labels"))) 
  }
}

####### resistant regression method 'lqs' - requires package MASS
`buildModel.lqs` <-
function(quantmod,training.data,...)
{
  if(is.method.available('lqs','MASS')) {
    #lq <- lqs(quantmod@model.formula,data=training.data,...)
    lq <- do.call('lqs',list(quantmod@model.formula,data=training.data,...))
	return(list("fitted"=lq,
                "inputs"=attr(terms(lq),"term.labels"))) 
  }
}

####### robust regression method 'rlm' - requires package MASS
`buildModel.rlm` <-
function(quantmod,training.data,...)
{
  if(is.method.available('lqs','MASS')) {
    #rl <- rlm(quantmod@model.formula,data=training.data,...)
    rl <- do.call('rlm',list(quantmod@model.formula,data=training.data,...))
	return(list("fitted"=rl,
                "inputs"=attr(terms(rl),"term.labels"))) 
  }
}

####### neural net method - requires package nnet
`buildModel.nnet` <-
function(quantmod,training.data,...)
{
  if(is.method.available('nnet','nnet')) {
	#nn <- nnet(quantmod@model.formula,data=training.data,...)
    nn <- do.call('nnet',list(quantmod@model.formula,data=training.data,...))
	return(list("fitted"=nn,
                "inputs"=attr(terms(nn),"term.labels"))) 
  }
}
`predictModel.nnet` <-
function(object,data,...)
{
  if(is.method.available('nnet','nnet')) {
    predict(object,data,...)
  }
}

####### projection pursuit regression method - requires stats
`buildModel.ppr` <-
function(quantmod,training.data,...)
{
	#p <- ppr(quantmod@model.formula,data=training.data,...)
    p <- do.call('ppr',list(quantmod@model.formula,data=training.data,...))
	return(list("fitted"=p,
                "inputs"=attr(terms(p),"term.labels"))) 
}

####### mars method - requires package mda
`buildModel.mars` <-
function(quantmod,training.data,...)
{
  if(is.method.available('mars','mda')) {
	x <- training.data[,-1]
	y <- training.data[,1]
	#m <- mars(x=x,y=y,...)
    m <- do.call('mars',list(x=x,y=y,...))
	return(list("fitted"=m,
                "inputs"=colnames(x))) 
  }
}

`predictModel.mars` <-
function(object,data,...)
{
  if(is.method.available('mars','mda')) {
    predict(object,data[,-1])
  }
}

####### polymars method - requires package polspline
`buildModel.polymars` <-
function(quantmod,training.data,...)
{
  if(is.method.available('polymars','polspline')) {
	responses <- training.data[,1]
	predictors <- training.data[,-1]
	#m <- polymars(responses,predictors,...)
    m <- do.call('polymars',list(responses,predictors,...))
	return(list("fitted"=m,
                "inputs"=colnames(predictors))) 
  }
}

`predictModel.polymars` <-
function(object,data,...)
{
  if(is.method.available('polymars','polspline')) {
    predict(object,data[,-1]);
  }
}

####### lars method - requires package lars
`buildModel.lars` <-
function(quantmod,training.data,...)
{
  if(is.method.available('lars','lars')) {
	x <- training.data[,-1]
	y <- training.data[,1]
	#m <- lars(x=x,y=y,...)
    m <- do.call('lars',list(x=x,y=y,...))
	return(list("fitted"=m,
                "inputs"=colnames(x))) 
  }
}

`predictModel.lars` <-
function(object,data,lars.s,...)
{
  if(is.method.available('lars','lars')) {
    lars.s = min(lars.s,object$Cp)
    predict(object,data[,-1],s=lars.s,...)$fit
  }
}

####### rpart method - requires package rpart
`buildModel.rpart` <-
function(quantmod,training.data,...)
{
  if(is.method.available('rpart','rpart')) {
	#rp <- rpart(quantmod@model.formula,data=training.data,...);
    rp <- do.call('rpart',list(quantmod@model.formula,data=training.data,...))
	return(list("fitted"=rp,
                "inputs"=attr(terms(rp),"term.labels"))); 
  }
}

`predictModel.rpart` <-
function(object,data,...)
{
  if(is.method.available('rpart','rpart')) {
    predict(object,data,...)
  }
}

####### tree method - requires package tree
`buildModel.tree` <-
function(quantmod,training.data,...)
{
  if(is.method.available('tree','tree')) {
	#rp <- tree(quantmod@model.formula,data=training.data,...);
    rp <- do.call('tree',list(quantmod@model.formula,data=training.data,...))
	return(list("fitted"=rp,
                "inputs"=attr(terms(rp),"term.labels"))); 
  }
}

`predictModel.tree` <-
function(object,data,...)
{
  if(is.method.available('tree','tree')) {
    predict(object,data,...)
  }
}

####### randomForest method - requires package randomForest
`buildModel.randomForest` <-
function(quantmod,training.data,...)
{
  if(is.method.available('randomForest','randomForest')) {
    #rp <- randomForest(quantmod@model.formula,data=training.data,...)
    rp <- do.call('randomForest',list(quantmod@model.formula,data=training.data,...))
    return(list("fitted"=rp,
            "inputs"=attr(terms(rp),"term.labels"))) 
  }
}

`predictModel.randomForest` <-
function(object,data,...)
{
    if(is.method.available('randomForest','randomForest')) {
      predict(object,data,...)
    }
}

`buildModel.svm` <-
function(quantmod,training.data,...)
{
  if(is.method.available('svm','e1071')) {
    #rp <- svm(formula(quantmod),data=training.data,...)
    rp <- do.call('svm',list(formula(quantmod),data=training.data,...))
    return(list('fitted'=rp,attr(terms(rp),'term.labels')))
  }
}

`predictModel.svm` <-
function(object,data,...)
{
  if(is.method.available('svm','e1071')) {
    predict(object,data[-NROW(data),],...) 
  }
}

`is.method.available` <-
function(method,package)
{
  if(!package %in% .packages()) {
    if(package %in% .packages(all=TRUE)) {
      cat(paste("loading required package:",package,"\n"))
      library(package,character.only=TRUE)
    } else {
      stop(paste('package',sQuote(package),'containing',
                 sQuote(method),'unable to be located'))
    }
  }
  return(TRUE)
}
