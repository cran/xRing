\name{plot}
\alias{plot}
\alias{plot.xRing}
\alias{plot.xRingList}
\title{Plot xRing and xRingList Objects}
\usage{
\method{plot}{xRing}(x, years = NULL, EwLw = TRUE,  xlim = NULL, ylim = NULL, ...)

\method{plot}{xRingList}(x, series = 1, years = NULL, EwLw = TRUE,  xlim = NULL, ylim = NULL, ...)
}
\arguments{
\item{x}{an object of class "xRing" or "xRingList".}

\item{years}{the years to be plotted, if \code{NULL} the whole time span is plotted.}

\item{EwLw}{logical. If \code{TRUE} the earlywood and latewood boundaries and width is plotted.}

\item{xlim}{vector of length 2 giving the x limits for the plot.}

\item{ylim}{the y limits of the plot.}

\item{...}{other arguments to be passed to plotRings function}

\item{series}{gives the name (or the index) of the series to be plotted, by default is 1 (i.e., the first series)}
}
\value{
None.
}
\description{
Plot method for objects of class "xRing" and "xRingList".
}
\examples{

data(PaPiRaw)
data(PaPiSpan)

PaPi <- detectRings(PaPiRaw, PaPiSpan)
class(PaPi)

PaPiRings <- detectEwLw(PaPi, ew = 0.5)
plot(PaPiRings, series = "AFO1001a")

PaPiRings1 <- detectEwLw(PaPi, ew = 0.35, lw = 0.55)
plot(PaPiRings1, series = "AFO1001a")

plot(PaPiRings, series = "AFO1001a", years = c(1990, 2000))
plot(PaPiRings$AFO1001a)

}
\seealso{
\link{plotRings}
}
