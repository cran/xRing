\name{selectProfiles}
\alias{selectProfiles}
\title{Select Profile(s)}
\usage{
selectProfiles(im, nPixel = 50, cal = NULL, multiple = TRUE)
}
\arguments{
\item{im}{an image}

\item{nPixel}{the width of the line}

\item{cal}{calibration}

\item{multiple}{a single or several profiles}
}
\value{
a vector with the average grayvalue along the selected line when a multiple is TRUE and a list when multiple is FALSE
}
\description{
Uses a line to select a profile (or a region of interest), when selecting a radius the line should start at the pith side and end at the bark side of the sample.
}
\examples{
if (interactive()) {
  # read a sample file
  im <- imRead(file = system.file("img", "AFO1046.1200dpi.png", package = "xRing"))

  # to display the image
  imDisplay(im)

  # select a profile
  profile <- selectProfile(im)

  # to display the profile
  plot(profile, type = "l")
}

}
