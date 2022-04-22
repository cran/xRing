\name{calibrateFilm}
\alias{calibrateFilm}
\title{Calibrate Film}
\usage{
calibrateFilm(
  im,
  thickness = stepIncrease(0.24, 7),
  density = 1.2922,
  plot = TRUE,
  auto = FALSE,
  nPixel = 50,
  plotAuto = FALSE,
  ...
)
}
\arguments{
\item{im}{a grayscale image}

\item{thickness}{a vector specifying the thickness of the calibration wedge at each step}

\item{density}{the density of the reference material (i.e. the calibration wedge)}

\item{plot}{if TRUE the calibration model is displayed}

\item{auto}{logical. If TRUE, automatic detection of the steps given a line is carried out. Use with care}

\item{nPixel}{if `auto = TRUE`: number of pixels gives the line width}

\item{plotAuto}{if TRUE the automatic detection of the grayscale values is displayed}

\item{...}{further arguments to be passed to \link{loess}}
}
\value{
an object of class `loess` representing the film calibration
}
\description{
Convenience function to do the whole calibration of a densitometry image in one function call internally calling \link{getSteps} and \link{fitCalibrationModel}
}
\examples{
if (interactive()) {
  # read a sample file
  im <- imRead(file = system.file("img", "AFO1046.1200dpi.png", package = "xRing"))

  # display the image
  imDisplay(im)

  # calibrate the film:
  calibration <- calibrateFilm(im,
    thickness = stepIncrease(0.24, 7),
    density = 1.2922,
    plot = TRUE
  )
}

}
\seealso{
\link{getSteps}
}
