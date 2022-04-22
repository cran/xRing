\name{plotRings}
\alias{plotRings}
\title{Plot xRing Objects}
\usage{
plotRings(x, xlim = NULL, ylim = NULL, id = NULL, corr = NULL, EwLw = TRUE)
}
\arguments{
\item{x}{an object of class "xRing"}

\item{xlim}{the x limits of the plot. The default value, NULL, indicates that the whole profile will be plotted.}

\item{ylim}{the y limits of the plot.}

\item{id}{a sufix to be added to the name of the series (<series_name> [id])}

\item{corr}{value to be print at the top of the graph}

\item{EwLw}{logical. If \code{TRUE} the earlywood and latewood assignments are plotted, by default is \code{TRUE}}
}
\value{
None. A plot is produced.
}
\description{
Plot "xRing" objects.
}
\examples{
if (interactive()) {
  data(PaPiRaw)
  data(PaPiSpan)

  PaPi <- detectRings(PaPiRaw[, 1, drop = FALSE], PaPiSpan)
  plotRings(PaPi$AFO1001a)
  plotRings(PaPi, series = "AFO1001a")
  plotRings(PaPi, series = "AFO1001a", xlim = c(120, 450))

  PaPi1 <- detectEwLw(PaPi, ew = 0.5)
  plotRings(PaPi1, series = "AFO1001a", EwLw = FALSE)
  plotRings(PaPi1, series = "AFO1001a")
}

}
\seealso{
\link{plot.xRing}
}
