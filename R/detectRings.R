#' @title Detect Tree-Ring Borders
#' @description
#' This function identifies tree-ring borders on X-ray microdensity profiles.
#' @param x a dataframe with X-ray microdensity profiles or an "xRingList" object
#' @param y a dataframe with the first and last year in columns and the series in rows, is \code{NULL} by default
#' @param k width of the rolling window to find the local maximum and minimum (for more details please see the help of \code{\link{getBorders}} function)
#' @param minTrw integer width of the narrowest tree-ring, rings narrower than this value will not be considered
#' @param threshold the minimum difference between local maximum and minimum density to identify a tree-ring border
#' @details
#' This function uses the \code{\link{getBorders}} function to identify tree-ring borders based on the difference between local maximum and minimum density.
#' @return \code{detectRings} returns an "xRingList" object, an S3 class with "xRing" lists as members, with the following elements:
#' @return \code{span} first and last year
#' @return \code{trw} gives the tree-ring width
#' @return \code{name} a \code{string} giving the series name
#' @return \code{limits} a \code{vector} with the position of the tree-ring borders
#' @return \code{years} a \code{vector} with the calendar year
#' @return \code{profile.raw} a \code{vector} with the input
#' @seealso
#' \link{getBorders}
#' @export
#' @examples
#'
#' data(PaPiRaw)
#' data(PaPiSpan)
#' PaPi <- toxRingList(PaPiRaw, PaPiSpan)
#' PaPi <- detectRings(PaPi)
#' # give the same
#' PaPi <- detectRings(PaPiRaw, PaPiSpan)
#' # Because the last year is not supplied the last year for all series is the last calendar year
#' # as.numeric(format(Sys.time(), "%Y"))-1
#' PaPi <- detectRings(PaPiRaw)
#'
detectRings <- function(x,
                        y = NULL,
                        k = 3,
                        minTrw = 3,
                        threshold = 0.215) {

  # if ("xRingList" %in% class(x)) {
  if (is.xRingList(x)) {
    out <- x
  } else {
    out <- toxRingList(x, y)
  }

  # out <- lapply(out,
  #               FUN = getBorders,
  #               minTrw = minTrw,
  #               threshold = threshold)
  for (i in seq_along(out)) {
    out[[i]] <- getBorders(out[[i]], minTrw = minTrw, threshold = threshold, k = k)
  }

  class(out) <- c("xRingList", "list")
  return(out)
}
