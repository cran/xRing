\name{setLastYear}
\alias{setLastYear}
\title{Set Last Year}
\usage{
setLastYear(x, lastYear, series = NULL)
}
\arguments{
\item{x}{an "xRing" or "xRingList" object}

\item{lastYear}{the new calendar year for the last tree ring}

\item{series}{individual series to be changed when the object is a "xRingList", by default is NULL}
}
\value{
the modified input object with new set last ring of the specified series.
}
\description{
Changes the calendar year of the last ring for a specific series.
}
\examples{

data(PaPiRaw)
data(PaPiSpan)
PaPi <- detectRings(PaPiRaw, PaPiSpan)
plot(PaPi, series = "AFO1001b")
PaPi <- setLastYear(PaPi, 2005, series = "AFO1001b")
plot(PaPi, series = "AFO1001b")

}
