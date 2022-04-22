\name{fitCalibrationModel}
\alias{fitCalibrationModel}
\title{Fit a Calibration Curve}
\usage{
fitCalibrationModel(
  grayvalues,
  thickness = stepIncrease(0.24, 7),
  density = 1.2922,
  plot = TRUE,
  ...
)
}
\arguments{
\item{grayvalues}{a numeric vector containing the gray values of the steps of the calibration wedge at various thicknesses given by the argument `thickness`}

\item{thickness}{a vector specifying the thickness of the calibration wedge at each step.}

\item{density}{the density of the reference material}

\item{plot}{if TRUE the calibration model is displayed}

\item{...}{further arguments to be passed to \link{loess}}
}
\value{
an object of class `loess` representing the film calibration
}
\description{
Fit a model to calibrate a film from X-ray densitometry.
}
\examples{
if (interactive()) {
  # read a sample file
  im <- imRead(file = system.file("img", "AFO1046.1200dpi.png", package = "xRing"))

  # display the image
  imDisplay(im)

  # get the grayvalues from the calibration wedge on the film
  grayvalues <- getSteps(im, 7)

  # calibrate the film by fitting a model:
  calibration <- fitCalibrationModel(grayvalues,
    thickness = stepIncrease(0.24, 7),
    density = 1.2922,
    plot = TRUE
  )
}

}
\seealso{
\link{getSteps}
}
