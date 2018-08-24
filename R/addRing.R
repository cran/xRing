#' @name addRing
#' @title Add Tree-Ring Border(s)
#' @description Add a tree-ring border by defining the position of the new border
#' @param object an object of class "xRingList" or "xRing"
#' @param x the position (number of the resp. pixel(s)) to set the new tree-ring border
#' @param series the name of the series to be changed when the \code{object} is "xRingList", by default is \code{NULL}
#' @return a "xRing" or "xRingList" object with a tree-ring border added at the position \code{x} for the series given by \code{series} argument
#' @export
#' @examples
#' 
#'  data(PaPiRaw)
#'  data(PaPiSpan)
#'  PaPi <- detectRings(PaPiRaw, PaPiSpan)
#'  plot(PaPi$'AFO1001a')
#'  PaPi$AFO1001a <- removeRing(PaPi$AFO1001a, 47)
#'  plot(PaPi$'AFO1001a')
#'  PaPi <- addRing(PaPi, series = 'AFO1001a', x = 47)
#'  plot(PaPi$'AFO1001a')
#' 
#'

addRing = function(object, x, series = NULL) {
  if (!any(c(is.xRing(object), is.xRingList(object))))
    stop("Use only with \"xRingList\" and \"xRing\" objects.")
  
  if(is.xRingList(object) && is.null(series)) {
    stop("please provide the argument \"series\" for objects of class \"xRingList\"")
  }

  if (!is.null(series)) {
    object[[series]] <- addRingSeries(object[[series]], x)
    return(object)
  }
  addRingSeries(object, x)
}
