\name{getBorders}
\alias{getBorders}
\title{Get Tree-Ring Borders}
\usage{
getBorders(x, k = 3, minTrw = 3, threshold = 0.215, addLastBorder = FALSE)
}
\arguments{
\item{x}{an object of class "xRing"}

\item{k}{integer; width of the rolling window}

\item{minTrw}{integer; width of the narrowest tree-ring, rings narrower than this value will not be considered}

\item{threshold}{the minimum difference between the local maximum and minimum density to detect tree-ring borders}

\item{addLastBorder}{logical; if \code{FALSE} the last border is not added. If \code{TRUE} the last border is placed at the position of the last value.}
}
\value{
The \code{getBorders} function returns an object of lass "xRing" including the following elements:

\code{names} a \code{string} giving the series name

\code{span} the first and last year

\code{trw} a \code{data.frame} with tree-ring width

\code{limits} a \code{vector} with the position of the tree-ring borders

\code{years} a \code{vector} with the calendar year

\code{profile.raw} a \code{vector} with the raw X-ray values

\code{profile} a \code{vector} with the the smoothed X-ray values (if is supplied in the input)
}
\description{
Identify tree-ring borders
}
\details{
This function uses local maximum and minimum densities in order to detect tree-ring borders.
}
\examples{

data("PaPiRaw")
data("PaPiSpan")
AFO1001a <- toxRing(PaPiRaw, PaPiSpan, "AFO1001a")
AFO1001a <- getBorders(AFO1001a)

AFO1001a <- toxRing(PaPiRaw, seriesName = "AFO1001a")
AFO1001a <- getBorders(AFO1001a)

}
