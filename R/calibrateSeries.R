
calibrateSeries <- function(profile, thickness, calibration) {
  if (NCOL(profile) == 2)
    profile <- profile[, 2]
  if (is.list(calibration))
    calibration <- loess( calibration$y ~ calibration$x)
  profile <-
    pmax(pmin(profile, max(calibration$x)), min(calibration$x))
  optical_density <- predict(calibration, newdata = profile)
  if (is.list(optical_density))
    optical_density <- optical_density$y
  density <- optical_density / thickness
  return(density)
}
