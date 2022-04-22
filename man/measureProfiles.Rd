\name{measureProfiles}
\alias{measureProfiles}
\title{Measure Profiles Interactively}
\usage{
measureProfiles(im, nPixel = 50, cal = NULL)
}
\arguments{
\item{im}{an image}

\item{nPixel}{the line width}

\item{cal}{calibration}
}
\value{
an xRingList object with all xRing objects
}
\description{
Several profiles can be selected in an image and a calibration for that image is used to convert pixels into wood density
}
\examples{
if (interactive()) {
  # read a sample file
  im <- imRead(file = system.file("img", "AFO1046.1200dpi.png", package = "xRing"))

  # to display the image
  imDisplay(im)

  cal1 <- calibrateFilm(im, thickness = stepIncrease(0.24, 7), density = 1.2922, plot = TRUE)
  profiles <- measureProfiles(im, cal = cal1)
}

}
