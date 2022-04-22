\name{print}
\alias{print}
\alias{print.xRing}
\alias{print.xRingList}
\title{Print xRing and xRingList Objects}
\usage{
\method{print}{xRing}(x, ...)

\method{print}{xRingList}(x, ...)
}
\arguments{
\item{x}{the object of class "xRing" or "xRingList" to print}

\item{...}{additional parameters}
}
\value{
None
}
\description{
Print method for objects of class "xRing" and "xRingList".
}
\examples{

data(PaPiRaw)
data(PaPiSpan)
PaPi <- detectRings(PaPiRaw, PaPiSpan)
class(PaPi)
print(PaPi$AFO1001a)
PaPi$AFO1001a
PaPi$AFO1001a[]
print(PaPi)
PaPi

}
