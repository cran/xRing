\name{detectEwLw}
\alias{detectEwLw}
\title{Detect the Transition from Earlywood to Latewood}
\usage{
detectEwLw(x, ew = 0.5, lw = NULL)
}
\arguments{
\item{x}{an "xRingList" object}

\item{ew}{defines the end of earlywood as the ratio of the density range. The default value is 0.5, which means that the end of earlywood is placed at the point where the density is half the range between the minimum and maximum density values within an annual ring}

\item{lw}{defines the start of latewood, the default value is \code{NULL}. When ew is 0.5 and lw is \code{NULL} the boundary between earlywood and latewood is placed where the density is half the range between the minimum and maximum density values within an annual ring}
}
\value{
an "xRingList" object with limits.ew and limits.lw added.
}
\description{
This function detects the end of earlywood and the start of latewood
}
\examples{

data(PaPiRaw)
data(PaPiSpan)
PaPi <- detectRings(PaPiRaw, PaPiSpan)
PaPi.merge <- combineFrag(PaPi, frag = 9)
PaPiRings <- detectEwLw(PaPi.merge, ew = 0.5)

}
