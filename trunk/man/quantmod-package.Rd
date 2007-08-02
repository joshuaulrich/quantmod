\name{quantmod-package}
\alias{quantmod-package}
\alias{quantmod}
\docType{package}
\title{
quantmod - Quantitative Financial Modelling Framework
}
\description{
Trading support package for building, testing, and trading
statistical models.
}
\details{
\tabular{ll}{
Package: \tab quantmod\cr
Type: \tab Package\cr
Version: \tab 0.1.0\cr
Date: \tab 2007-05-19\cr
Depends: \tab zoo, Defaults\cr
Suggests: \tab 
MASS,mda,randomForest,lars,nnet,polspline,rpart,quantreg,tree,its,fCalendar,DBI,RMySQL\cr
LazyLoad: \tab yes\cr
License: \tab GPL Version 2 or later\cr
Packaged: \tab Wed Jul 4 2007; jeff ryan\cr
Built: \tab R 2.5.0; ; 2007-07-19 11:20:18; unix\cr
}

Index:
\preformatted{
Ad                      Extract and Transform quantmod.OHLC Columns
Delt                    Calculate Percent Change
Lag                     Lag a Time Series
Next                    Advance a Time Series
SPY                     SP500 Data Set
breakpoints             Locate Breakpoints by Date
buildModel              Build quantmod model given specified fitting
                        method
getModelData            Update model's dataset
getSymbols              Manage Data from Multiple Sources
period.apply            Apply Function Over Specified Interval
periodReturn            Calculate Periodic Returns
quantmod-class          Class "quantmod"
specifyModel            Specify Model Formula For quantmod Process
tradeModel              Simulate Trading of Fitted quantmod Object



\pkg{quantmod} is designed to augment the existing financial
modelling tools available in R, by providing an interface
designed to accomodate a standardized workflow of data management,
model specification and building, through post-modelling
analysis.
}
}
\author{
Jeffrey A. Ryan

Maintainer: Jeffrey A. Ryan <jeff.a.ryan@gmail.com>
}
\references{
}
\keyword{ package }
\seealso{
~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
}
