\name{toxRingList}
\alias{toxRingList}
\title{Create a "xRingList" Object}
\usage{
toxRingList(x, y = NULL)
}
\arguments{
\item{x}{a dataframe with X-ray microdensity profiles}

\item{y}{a dataframe with the numerical values of the first and last year in columns. The individual series are specified as row names. By default is NULL}
}
\value{
an "xRingList" object, an S3 class which list membera are "xRing" objects containing:

\code{profile.raw} a \code{vector} with the input density profile

\code{span} first and last year

\code{name} a \code{string} giving the series name
}
\description{
Converts a dataframe with X-ray microdensity profiles in an "xRingList" object
}
\examples{

data(PaPiRaw)
data(PaPiSpan)
PaPi <- toxRingList(PaPiRaw, PaPiSpan)
class(PaPi)

}
\seealso{
\link{toxRing}
}
