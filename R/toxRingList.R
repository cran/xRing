#' @title Create a "xRingList" Object
#' @description Converts a dataframe with X-ray microdensity profiles in an "xRingList" object
#' @param x a dataframe with X-ray microdensity profiles
#' @param y a dataframe with the numerical values of the first and last year in columns. The individual series are specified as row names. By default is NULL
#' @return an "xRingList" object, an S3 class which list membera are "xRing" objects containing:
#' @return \code{profile.raw} a \code{vector} with the input density profile
#' @return \code{span} first and last year
#' @return \code{name} a \code{string} giving the series name
#' @seealso
#' \link{toxRing}
#' @export
#' @examples
#' 
#'  data(PaPiRaw)
#'  data(PaPiSpan)
#'  PaPi <- toxRingList(PaPiRaw, PaPiSpan)
#'  class(PaPi)
#' 
toxRingList <- function(x, y = NULL) {
  n <- ncol(x)
  seriesName <- colnames(x)
  out <- vector("list", n)
  names(out) <- seriesName
  for (i in 1:n) {
    out[[i]] <- toxRing(x, y, seriesName[i])
  }
  class(out) <- c("xRingList", "list")
  return(out)
}
