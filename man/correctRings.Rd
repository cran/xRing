\name{correctRings}
\alias{correctRings}
\title{Correct Tree-Ring Borders Interactively}
\usage{
correctRings(x, chrono = NULL)
}
\arguments{
\item{x}{an \code{xRingList} object}

\item{chrono}{a data.frame with a reference chronology, if \code{NULL} a reference chronology is calculated using tree-ring width series from \code{x}}
}
\value{
an \code{xRingList} object
}
\description{
A Graphical User Interface (GUI) to correct tree-ring borders
}
\details{
This function uses the \code{tkRplot} function (tkRplotR package) to interact with X-ray microdensity profiles.
}
\examples{
if (interactive()) {
  data(PaPiRaw)
  data(PaPiSpan)
  PaPi <- detectRings(PaPiRaw, PaPiSpan)
  PaPiCorrect <- correctRings(PaPi)
}

}
