\name{getRwls}
\alias{getRwls}
\title{Get Data-Frames With Ring Width and Density Values}
\usage{
getRwls(x)
}
\arguments{
\item{x}{an "xRingList" object}
}
\value{
a list with 8 elements:
\describe{
 \item{trw}{a data.frame with tree-ring widths}
 \item{ew}{a data.frame with earlywood widths}
 \item{lw }{a data.frame with latewood widths}
 \item{Dmean}{a data.frame with mean tree-ring density}
 \item{Dew}{a data.frame with mean earlywood density}
 \item{Dlw}{a data.frame with mean latewood density}
 \item{Dmin}{a data.frame with the minimum ring density}
 \item{Dmax}{a data.frame with the maximum ring density}
}
}
\description{
Produce a list with 8 data.frames (trw, ew, lw, Dmean, Dew, Dlw, Dmin, Dmax ) that can be used by other packages (dplR, detrendeR)
}
\examples{

data(PaPiRaw)
data(PaPiSpan)
PaPi <- detectRings(PaPiRaw, PaPiSpan)
PaPi <- combineFrag(PaPi)
PaPi <- detectEwLw(PaPi)
rwls <- getRwls(PaPi)
names(rwls)
library(dplR)
rwl.report(rwls$trw)
library(detrendeR)
RwlInfo(rwls$trw)

}
