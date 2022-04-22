trimSeries <- function(x) {
  xrange <- range(x$limits)
  x$limits <- x$limits - xrange[1] + 1
  x$profile.raw <- x$profile.raw[xrange[1]:xrange[2]]
  if ("profile" %in% names(x)) {
    x$profile <- x$profile[xrange[1]:xrange[2]]
  }
  x
}
