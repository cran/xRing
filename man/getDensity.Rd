\name{getDensity}
\alias{getDensity}
\title{Get Density Values}
\usage{
getDensity(x)
}
\arguments{
\item{x}{a "xRingList" or "xRing" object}
}
\value{
a "xRingList" or "xRing" object with density values c("Dmean", "Dmin", "Dmax", "Dew", "Dlw") for each ring
}
\description{
Get wood density parameters by tree-ring.
}
\examples{

data(PaPiRaw)
data(PaPiSpan)
PaPi <- detectRings(PaPiRaw, PaPiSpan)
PaPi.merge <- combineFrag(PaPi, frag = 9)
PaPiRings <- detectEwLw(PaPi.merge, ew = 0.5)

PaPi <- detectRings(PaPiRaw, PaPiSpan)
PaPiRings <- detectEwLw(PaPi, ew = 0.5)

# xRingList object
PaPiDen <- getDensity(PaPiRings)

PaPiDen$AFO1001a[]
PaPiDen$AFO1001a$density

# xRing object
PaPi_AFO1001a <- getDensity(PaPi$AFO1001a)
# the same
PaPi_1 <- getDensity(PaPi[[1]])
identical(PaPi_AFO1001a, PaPi_1)

# do not work for PaPi[1]
# class(PaPi[1])
# getDensity(PaPi[1]) # 'list' class

}
