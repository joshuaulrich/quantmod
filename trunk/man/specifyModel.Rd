\name{specifyModel}
\alias{specifyModel}
\alias{Next}
\alias{Lag}
\alias{Delta}
\alias{Op}
\alias{Hi}
\alias{Lo}
\alias{Cl}
\alias{Vo}
\alias{Ad}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Create quantmod object }
\description{
Used to create a quantmod object for use in building, trading, and testing.
}
\usage{
specifyModel(formula)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{formula: target ~ input1 [+ input2 [+ ...]]}
}
\details{
Models are specified through the standard formula mechanism. Dependent variable ~ Independent variables.  Individual elements must be specified using tR package conventions. (e.g. To use the close of Motorola for an input you would type: Cl(MOT).

Current transforms include:

[Op]en, [Lo]w, [Hi]gh, [Cl]ose, [Vo]lume, [Ad]justed close

[Next](x, k=1) next k period's value,  
[Lag](x, k=1) previous period's value, an S3 method.
[Delta] change over k periods
}

\value{
Returns object of class \code{quantmod}
}

\author{ Jeffrey Ryan }
\note{ Additional transforms are coming }


\seealso{ \code{\link{getModelData}} \code{\link{getSymbols}}
 \code{\link{buildModel}} \code{\link{tradeModel}} \code{\link{formula}}
\examples{
# create model with dependent variable next_open_to_close on DIA,
# against independent variables open_to_high DIA and a 2,3,4-day lag
# of the open of the DIA.

#DO NOT RUN
#newmodel <- specifyModel(next.per(delta(Op(DIA),Cl(DIA))) ~ delta(Op(DIA),Hi(DIA)) + lag(Op(DIA),2:4))

# returns a tR.object for modelling and evaluation

}
\keyword{ models formula }% at least one, from doc/KEYWORDS
