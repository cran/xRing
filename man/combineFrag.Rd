\name{combineFrag}
\alias{combineFrag}
\title{Combine Fragments}
\usage{
combineFrag(x, frag = NULL)
}
\arguments{
\item{x}{an "xRingList" object}

\item{frag}{integer, defines the character position within the series name that identifies fragments. If \code{NULL} the function considers series with names having one more character as fragments}
}
\value{
an object of class "xRingList" with merged fragments
}
\description{
This function combines fragments by series
}
\examples{

data(PaPiRaw)
data(PaPiSpan)
PaPi <- detectRings(PaPiRaw, PaPiSpan)
PaPi.merge <- combineFrag(PaPi, frag = 9)

}
