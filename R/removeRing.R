#' @title Remove Tree-Ring Border(s)
#' @description Remove the closest tree-ring border
#' @param object an object of class "xRing" or "xRingList"
#' @param x the position to delete the closest tree-ring border
#' @param series the name of the series to be changed when the object is a "xRingList", by default is NULL
#' @return an object of class "xRing" or "xRingList" without the tree-ring border at the position \code{x} for the series given by \code{series} argument
#' @export
#' @examples
#'  data(PaPiRaw)
#'  data(PaPiSpan)
#'  PaPi <- detectRings(PaPiRaw, PaPiSpan)
#'  plotRings(PaPi$AFO1001a)
#'  abline(v = 60, lty = 2, col = 2) 
#'  PaPi$AFO1001a <- removeRing(PaPi$AFO1001a, x = 60)
#'  # PaPi$AFO1001a <- removeRing(PaPi$AFO1001a, x = locator(1)$x)
#'  plotRings(PaPi$AFO1001a)
#' 

removeRing = function(object, x, series = NULL) {
  if (!any(c("xRingList", "xRing") %in% class(object))) {
    stop("Use only with \"xRingList\" and \"xRing\" objects.")
  }

  if (!is.null(series)) {
    object[[series]] <- removeRingSeries(object[[series]], x)
    return(object)
  }
  x <- removeRingSeries(object, x)
  return(x)
}
