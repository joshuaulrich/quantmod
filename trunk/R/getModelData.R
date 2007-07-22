"getModelData" <-
function(x,na.rm=TRUE)
{
    model <- x
    if(!is.quantmod(model)) 
        stop(sQuote('x'),"must be of class",dQuote("quantmod"),"\n");
    if(length(model@model.inputs) == 0) {
        #if model.inputs is not yet defined, create full zoo object for building
        build.vars <- c(model@model.target,model@build.inputs);
    } else {
        #else create data object with only relevant model.inputs
        build.vars <- c(model@model.target,model@model.inputs);
    }
    #price.level <- paste(c("Op(","Hi(","Lo(","Cl("),model@product,")",sep="");

    model.symbols <- model@symbols;
    missing.symbols = NULL
    for(i in 1:length(model.symbols)) {
        if(!exists(model.symbols[[i]],1)) {
            ## create vector of symbols to retrieve from getSymbols call
            missing.symbols <- c(missing.symbols,model.symbols[[i]])
        } else {
            ## get symbols from GlobaEnv and place in this environment
            assign(model.symbols[[i]],get(model.symbols[[i]],1),environment())
            ## NEED to coerce to quantmod.OHLC and zoo object
        }
    }

    if(length(missing.symbols > 0)) getSymbols(missing.symbols,env=environment())

    target.data <- get(model.symbols[[1]],environment())
    if("zoo" %in% class(target.data)) {
        target.dates <- index(target.data)
    } else {
        target.dates  <- rownames(target.data)
    }
    total.columns = NULL
    for(j in 1:length(model.symbols)) { # build single zoo object
        if(j == 1) {
            m <- merge(zoo(target.data,as.Date(target.dates)))     #target columns
        } else {
            m <- merge(m,
                       zoo(get(model.symbols[[j]],environment()), #input columns from symbol i
                       as.Date(index(get(model.symbols[[j]],environment()))))) 
        }            
        total.columns[j] <- ncol(m)
    }

    fullIndex <- index(m) 
    m <- as.data.frame(m)

    from.col = 1
    for(i in 1:length(model.symbols)) { # assign portions of 
        assign(model.symbols[[i]],m[,from.col:(total.columns[i])],environment())
        from.col = total.columns[i] + 1
    }

    mf <- zoo(model.frame(model@model.spec,data=environment(),na.action=NULL),fullIndex);

    #pl.formula <- as.formula(paste('Op(',model@product,') ~ ',
    #                paste(c('Hi(',' + Lo(',' + Cl('),model@product,
    #                ')',collapse=''),sep=''));
    #pl.mf <- zoo(model.frame(pl.formula,data=environment()),as.Date(target.dates))

    if(na.rm) mf <- rbind(na.exclude(mf[-nrow(mf),]),mf[nrow(mf),]);

    colnames(mf) <- lapply(colnames(mf),function(x) 
                        { gsub("[) ]","",gsub("[(,=:'\"]",".",x)) });
    #colnames(pl.mf) <- lapply(price.level,function(x) 
    #                    { gsub("[) ]","",gsub("[(,=:'\"]",".",x)) });
    #colnames(pl.mf) <- paste(model@product,c('.Open','.High','.Low','.Close'),sep='')

    model@model.data <- mf;
    #model@price.levels <- pl.mf;
    model@build.inputs <- colnames(mf)[-1]
    model@model.formula = as.formula(paste(colnames(mf)[1],'~',
                                        paste(colnames(mf)[-1],collapse='+'),
                                        sep=''))

    ##removeSymbols()
    return(model);
}
"getModelData.original" <-
function(model,na.rm=TRUE) {
  if(class(model) != "quantmod") stop("model must be of class 'quantmod'");
  if(length(model@model.inputs) == 0) {
    #if model.inputs is not yet defined, create full zoo object for building
    build.vars <- c(model@model.target,model@build.inputs);
  } else {
    #else create data object with only relevant model.inputs
    build.vars <- c(model@model.target,model@model.inputs);
  }
  # add OHLC of model.target product
  price.level <- paste(c("O(","H(","L(","C("),model@product,")",sep="");
  rd <- fetchRawData(model@symbols); 

  build.and.price <- c(build.vars,price.level);  
  bapd <- sapply(build.and.price,tR.transformData,data=rd);
  dt <- as.Date(rd[,1]);

  # convert original vars to zoo object
  md <- zoo(as.matrix(bapd[,1:length(build.vars)]),dt);

  # convert price.levels (last 4 cols OHLC) to zoo object
  pl <- zoo(as.matrix(bapd[,((length(build.vars)+1):(length(build.vars)+4))]),dt);

  # convert parens to dot notation (e.g. OC(DIA) => OC.DIA)
  colnames(md) <- lapply(build.vars,function(x) { gsub("\\)","",gsub("\\(",".",x)) });
  colnames(pl) <- lapply(price.level,function(x) { gsub("\\)","",gsub("\\(",".",x)) });

  # remove all NAs from zoo, excepting LAST NA
  if(na.rm) md <- rbind(na.exclude(md[-nrow(md),]),md[nrow(md),]);
  model@model.data <- md;
  model@price.levels <- pl;
  return(model);
}

"stripModelData" <-
function(model) {
    if(class(model) != "quantmod") stop("model must be of class 'quantmod'");
    model@model.data <- zoo(0,0);
    model@price.levels <- zoo(0,0);
    return(model);
}

