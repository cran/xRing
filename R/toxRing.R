#' @title Create an "xRing" Object
#' @description
#' Converts a dataframe with X-ray microdensity profiles into an "xRing" object
#' @param x a dataframe with X-ray microdensity profiles
#' @param y a dataframe with the numerical values of the first and last year in columns. The individual series are specified as row names.
#' @param seriesName the name of series from x and y to be used to produce the "xRing" object.
#' @return an "xRing" object, an S3 class with the following elements:
#' @return \code{profile.raw} a \code{vector} with the input density profile
#' @return \code{span} first and last year
#' @return \code{name} a \code{string} giving the series name
#' @seealso
#' \link{toxRingList}
#' @export
#' @examples
#' 
#'  data(PaPiRaw)
#'  data(PaPiSpan)
#'  PaPi.AFO1001a <- toxRing(PaPiRaw, PaPiSpan, seriesName = "AFO1001a")
#'  class(PaPi.AFO1001a)
#' 
toxRing <- function(x, y = NULL, seriesName ) {
  if (is.null(y)) {
    span <- c(NA, NA)
  } else{
    span <- y[seriesName,]
    span <- c(span[[1]], span[[2]])
  }
  out <- list(
    "profile.raw" = as.vector(na.omit(x[, seriesName])),
    "span" =  span,
    "name" = seriesName
  )
  out <- as.xRing(out)
  return(out)
}
