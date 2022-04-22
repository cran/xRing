\name{getSteps}
\alias{getSteps}
\title{Select the Steps of a Calibration Wedge Interactively}
\usage{
getSteps(im, nSteps = NULL, auto = FALSE, nPixel = 50)
}
\arguments{
\item{im}{an image.}

\item{nSteps}{number of steps of the calibration wedge to obtain grayvalues from.}

\item{auto}{logical. If TRUE, automatic detection of the steps given a line is carried out. Use with care.}

\item{nPixel}{gives the line width when `auto = TRUE`}
}
\value{
a numeric vector
}
\description{
Obtain the Grayvalue of Each Step of a Calibration Wedge
}
\examples{
if (interactive()) {
  # read a sample file
  im <- imRead(file = system.file("img", "AFO1046.1200dpi.png", package = "xRing"))

  # display the image
  imDisplay(im)

  # get the grayvalues from the calibration wedge on the film
  steps <- grayvalues <- getSteps(im, 7) # select 7 ROIs
  steps1 <- grayvalues <- getSteps(im, 7, auto = TRUE) # select a single ROI
  cor(steps, steps1)
}

}
