#' @name stepIncrease
#' @title Calculate the Steps Thickness of the Calibration Wedge
#' @description convenience function to calculate the thickness of each steps of the calibration wedge for wedges with continous step increase.
#' 
#' @param step.increase height increase per wedge step
#' @param nsteps total number of steps (the first step has the thickness of 0 - the area beside the wedge. Mention that when setting nsteps)
#'
#' @return a numeric vector
#' @export
#'
stepIncrease <- function(step.increase =  0.24, nsteps = 7) {
  step.increase * (seq_len(nsteps) - 1)
  }


#' @title Fit a Calibration Curve
#' @description
#' Fit a model to calibrate a film from X-ray densitometry.
#' @param grayvalues a numeric vector containing the gray values of the steps of the calibration wedge at various thicknesses given by the argument `thickness`
#' @param thickness a vector specifying the thickness of the calibration wedge at each step.
#' @param density the density of the reference material
#' @param plot if TRUE the calibration model is displayed
#' @param ... further arguments to be passed to \link{loess}
#' @return an object of class `loess` representing the film calibration
#' @seealso
#' \link{getSteps}
#' @export
#' @examples
#' if(interactive()){
#' # read a sample file
#'  im <- imRead(file = system.file("img", "AFO1046.1200dpi.png", package="xRing"))
#'
#' # display the image
#'   imDisplay(im)
#'   
#' # get the grayvalues from the calibration wedge on the film
#'   grayvalues <- getSteps(im, 7)
#'
#' # calibrate the film by fitting a model:
#'   calibration <- fitCalibrationModel(grayvalues,
#'                                     thickness = stepIncrease(0.24, 7),
#'                                     density = 1.2922,
#'                                     plot = TRUE)
#' }
#'
fitCalibrationModel <- function(grayvalues,
                          thickness = stepIncrease(0.24, 7),
                          density = 1.2922,
                          plot = TRUE,
                          ...) {
  # input validation
  if(!(is.numeric(grayvalues) && is.numeric(thickness) && (length(grayvalues) == length(thickness)))) {stop('`grayvalues` and `thickness` need to be provided as numeric vectors of the same length')}
  if(!((length(density)) == 1 && is.numeric(density))) {stop("`density` must be provided as a numeric vector of length 1")}  

  # main function
  optical_density <- density * thickness
  cal_film <- loess(optical_density ~ grayvalues, ...)

  if(plot){
    rng_grayvalues <- range(grayvalues)
    xrange <- seq(rng_grayvalues[[1]], rng_grayvalues[[2]], length.out = 1000)
    plot(optical_density ~ grayvalues)
    lines(xrange, predict(cal_film, newdata = c(grayvalues = xrange)), col = 'red')
  }
  return(cal_film)
}
  
#' @title Calibrate Film
#' @description
#' Convenience function to do the whole calibration of a densitometry image in one function call internally calling \link{getSteps} and \link{fitCalibrationModel}
#' @param im a grayscale image
#' @param thickness a vector specifying the thickness of the calibration wedge at each step
#' @param density the density of the reference material (i.e. the calibration wedge)
#' @param auto logical. If TRUE, automatic detection of the steps given a line is carried out. Use with care
#' @param nPixel if `auto = TRUE`: number of pixels gives the line width
#' @param plot if TRUE the calibration model is displayed
#' @param plotAuto if TRUE the automatic detection of the grayscale values is displayed
#' @param ... further arguments to be passed to \link{loess}
#' @return an object of class `loess` representing the film calibration
#' @seealso
#' \link{getSteps}
#' @export
#' @examples
#' if(interactive()){
#' # read a sample file
#'  im <- imRead(file = system.file("img", "AFO1046.1200dpi.png", package="xRing"))
#'
#' # display the image
#'   imDisplay(im)
#'   
#' # calibrate the film:
#'   calibration <- calibrateFilm(im,
#'                                thickness = stepIncrease(0.24, 7),
#'                                density = 1.2922,
#'                                plot = TRUE)
#' }
#'
calibrateFilm <- function(im,
                          thickness = stepIncrease(0.24, 7),
                          density = 1.2922,
                          plot = TRUE,
                          auto = FALSE, 
                          nPixel = 50,
                          plotAuto = FALSE,
                          ...) {
  
  nSteps <- length(thickness)
  grayvalues <- getSteps(im, nSteps, auto = auto, nPixel = nPixel)
  fitCalibrationModel(grayvalues,
                      thickness = thickness,
                      density = density,
                      plot = plot, 
                      ...)
}


