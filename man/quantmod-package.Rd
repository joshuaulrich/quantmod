\name{quantmod-package}
\alias{quantmod-package}
\alias{quantmod}
\docType{package}
\title{
Quantitative Financial Modelling Framework
}
\description{
Specify, build, trade, and analyse quantitative financial
trading srategies
}
\details{
\tabular{ll}{
Package: \tab quantmod\cr
Type: \tab Package\cr
Version: \tab 0.1-4\cr
Date: \tab 2007-08-06\cr
Depends: \tab zoo,Defaults\cr
Suggests: \tab 
MASS,mda,randomForest,lars,nnet,polspline,rpart,quantreg,tree,its,DBI,RMySQL,fCalendar\cr
LazyLoad: \tab yes\cr
License: \tab GPL 2 or later\cr
Packaged: \tab Thu Aug 2 00:04:35 2007; jryan\cr
URL: \tab http://www.quantmod.com\cr
Built: \tab R 2.5.0; ; 2007-08-02 00:04:38; unix\cr
}

Index:
\preformatted{
Delt                    Calculate Percent Change
Lag                     Lag a Time Series
Next                    Advance a Time Series
OHLC.Transformations    Extract and Transform quantmod.OHLC Cols
as.quantmod.OHLC        Create Open High Low Close Object
breakpoints             Locate Breakpoints by Date
buildModel              Build quantmod model 
fittedModel             quantmod Fitted Objects
getModelData            Update model's dataset
getSymbols              Manage Data from Multiple Sources
getSymbols.MySQL        Retrieve Data from MySQL Database
getSymbols.yahoo        Download OHLC Data From Yahoo!
is.quantmod             Test If Object of Type quantmod
modelData               Extract Dataset Created by specifyModel
modelSignal             Extract Model Signal Object
period.apply            Apply Function Over Specified Interval
periodReturn            Calculate Periodic Returns
quantmod-class          Class "quantmod"
quantmod-package        Quantitative Financial Modelling Framework
setSymbolLookup         Manage Symbol Lookup Table
specifyModel            Specify Model Formula For quantmod Process
tradeModel              Simulate Trading of Fitted quantmod Object
weekdays.zoo            Extract Parts of a zoo Object
weeks                   Extract Weeks of a Time Series Object
}
'quantmod' is a workflow tool to be used to manage the quantitative
trading process within R. At present, much work has been done to manage
data as well as provide a uniform interface to a wide array of modelling
methods.
}
\author{
Jeffrey A. Ryan

Maintainer: Jeffrey A. Ryan <jeff.a.ryan@gmail.com>
}
\keyword{ package }
