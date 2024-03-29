\name{addRing}
\alias{addRing}
\title{Add Tree-Ring Border(s)}
\usage{
addRing(object, x, series = NULL)
}
\arguments{
\item{object}{an object of class "xRingList" or "xRing"}

\item{x}{the position (number of the resp. pixel(s)) to set the new tree-ring border}

\item{series}{the name of the series to be changed when the \code{object} is "xRingList", by default is \code{NULL}}
}
\value{
a "xRing" or "xRingList" object with a tree-ring border added at the position \code{x} for the series given by \code{series} argument
}
\description{
Add a tree-ring border by defining the position of the new border
}
\examples{

data(PaPiRaw)
data(PaPiSpan)
PaPi <- detectRings(PaPiRaw, PaPiSpan)
plot(PaPi$"AFO1001a")
PaPi$AFO1001a <- removeRing(PaPi$AFO1001a, 47)
plot(PaPi$"AFO1001a")
PaPi <- addRing(PaPi, series = "AFO1001a", x = 47)
plot(PaPi$"AFO1001a")

}
