\name{removeRing}
\alias{removeRing}
\title{Remove Tree-Ring Border(s)}
\usage{
removeRing(object, x, series = NULL)
}
\arguments{
\item{object}{an object of class "xRing" or "xRingList"}

\item{x}{the position to delete the closest tree-ring border}

\item{series}{the name of the series to be changed when the object is a "xRingList", by default is NULL}
}
\value{
an object of class "xRing" or "xRingList" without the tree-ring border at the position \code{x} for the series given by \code{series} argument
}
\description{
Remove the closest tree-ring border
}
\examples{
data(PaPiRaw)
data(PaPiSpan)
PaPi <- detectRings(PaPiRaw, PaPiSpan)
plotRings(PaPi$AFO1001a)
abline(v = 60, lty = 2, col = 2)
PaPi$AFO1001a <- removeRing(PaPi$AFO1001a, x = 60)
# PaPi$AFO1001a <- removeRing(PaPi$AFO1001a, x = locator(1)$x)
plotRings(PaPi$AFO1001a)

}
