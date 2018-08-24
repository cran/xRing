#' @title Detect the Transition from Earlywood to Latewood
#' @description
#' This function detects the end of earlywood and the start of latewood
#' @param x an "xRingList" object
#' @param ew defines the end of earlywood as the ratio of the density range. The default value is 0.5, which means that the end of earlywood is placed at the point where the density is half the range between the minimum and maximum density values within an annual ring
#' @param lw defines the start of latewood, the default value is \code{NULL}. When ew is 0.5 and lw is \code{NULL} the boundary between earlywood and latewood is placed where the density is half the range between the minimum and maximum density values within an annual ring
#' @return an "xRingList" object with limits.ew and limits.lw added.
#' @export
#' @examples
#' 
#'  data(PaPiRaw)
#'  data(PaPiSpan)
#'  PaPi <- detectRings(PaPiRaw, PaPiSpan)
#'  PaPi.merge <- combineFrag(PaPi, frag = 9)
#'  PaPiRings <- detectEwLw(PaPi.merge, ew = 0.5)
#' 
detectEwLw = function(x, ew = 0.5, lw = NULL) {

  if (!is.xRingList(x))
    stop("Use only with \"xRingList\" objects.")

  if (is.null(lw))
    lw <- ew

  out <- lapply(x, EwLw2Series, ew, lw)
  out <- as.xRingList(out)
  return(out)
}
