\name{detectRings}
\alias{detectRings}
\title{Detect Tree-Ring Borders}
\usage{
detectRings(x, y = NULL, k = 3, minTrw = 3, threshold = 0.215)
}
\arguments{
\item{x}{a dataframe with X-ray microdensity profiles or an "xRingList" object}

\item{y}{a dataframe with the first and last year in columns and the series in rows, is \code{NULL} by default}

\item{k}{width of the rolling window to find the local maximum and minimum (for more details please see the help of \code{\link{getBorders}} function)}

\item{minTrw}{integer width of the narrowest tree-ring, rings narrower than this value will not be considered}

\item{threshold}{the minimum difference between local maximum and minimum density to identify a tree-ring border}
}
\value{
\code{detectRings} returns an "xRingList" object, an S3 class with "xRing" lists as members, with the following elements:

\code{span} first and last year

\code{trw} gives the tree-ring width

\code{name} a \code{string} giving the series name

\code{limits} a \code{vector} with the position of the tree-ring borders

\code{years} a \code{vector} with the calendar year

\code{profile.raw} a \code{vector} with the input
}
\description{
This function identifies tree-ring borders on X-ray microdensity profiles.
}
\details{
This function uses the \code{\link{getBorders}} function to identify tree-ring borders based on the difference between local maximum and minimum density.
}
\examples{

data(PaPiRaw)
data(PaPiSpan)
PaPi <- toxRingList(PaPiRaw, PaPiSpan)
PaPi <- detectRings(PaPi)
# give the same
PaPi <- detectRings(PaPiRaw, PaPiSpan)
# Because the last year is not supplied the last year for all series is the last calendar year
# as.numeric(format(Sys.time(), "\%Y"))-1
PaPi <- detectRings(PaPiRaw)

}
\seealso{
\link{getBorders}
}
