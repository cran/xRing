#' @title Get Density Values
#' @description
#' Get wood density parameters by tree-ring.
#' @param x a "xRingList" or "xRing" object
#' @return a "xRingList" or "xRing" object with density values c("Dmean", "Dmin", "Dmax", "Dew", "Dlw") for each ring
#' @export
#' @examples
#' 
#'  data(PaPiRaw)
#'  data(PaPiSpan)
#'  PaPi <- detectRings(PaPiRaw, PaPiSpan)
#'  PaPi.merge <- combineFrag(PaPi, frag = 9)
#'  PaPiRings <- detectEwLw(PaPi.merge, ew = 0.5)
#'
#'  PaPi <- detectRings(PaPiRaw, PaPiSpan)
#'  PaPiRings <- detectEwLw(PaPi, ew = 0.5)
#'
#'  # xRingList object
#'  PaPiDen <- getDensity(PaPiRings)
#'
#'  PaPiDen$AFO1001a[]
#'  PaPiDen$AFO1001a$density
#'
#'  # xRing object
#'  PaPi_AFO1001a <- getDensity(PaPi$AFO1001a)
#'  #the same
#'  PaPi_1 <- getDensity(PaPi[[1]])
#'  identical(PaPi_AFO1001a , PaPi_1)
#'
#'  # do not work for PaPi[1]
#'  # class(PaPi[1])
#'  # getDensity(PaPi[1]) # 'list' class
#'
#' 
#'

getDensity = function(x) {
 # if ("xRingList" %in% class(x)) {
  if (is.xRingList(x)) {
    out <- lapply(x, getDenSeries)
    class(out) <- c("xRingList", "list")
    return(out)
  }
#  if ("xRing" %in% class(x)) {
  if (is.xRing(x)) {
    return(getDenSeries(x))
  } else{
    stop("Use only with \"xRingList\" or \"xRing\" objects.")
  }
}

