\name{toxRing}
\alias{toxRing}
\title{Create an "xRing" Object}
\usage{
toxRing(x, y = NULL, seriesName)
}
\arguments{
\item{x}{a dataframe with X-ray microdensity profiles}

\item{y}{a dataframe with the numerical values of the first and last year in columns. The individual series are specified as row names.}

\item{seriesName}{the name of series from x and y to be used to produce the "xRing" object.}
}
\value{
an "xRing" object, an S3 class with the following elements:

\code{profile.raw} a \code{vector} with the input density profile

\code{span} first and last year

\code{name} a \code{string} giving the series name
}
\description{
Converts a dataframe with X-ray microdensity profiles into an "xRing" object
}
\examples{

data(PaPiRaw)
data(PaPiSpan)
PaPi.AFO1001a <- toxRing(PaPiRaw, PaPiSpan, seriesName = "AFO1001a")
class(PaPi.AFO1001a)

}
\seealso{
\link{toxRingList}
}
