#' @title Combine Fragments
#' @description
#' This function combines fragments by series
#' @param x an "xRingList" object
#' @param frag integer, defines the character position within the series name that identifies fragments. If \code{NULL} the function considers series with names having one more character as fragments
#' @return an object of class "xRingList" with merged fragments
#' @export
#' @examples
#' 
#'  data(PaPiRaw)
#'  data(PaPiSpan)
#'  PaPi <- detectRings(PaPiRaw, PaPiSpan)
#'  PaPi.merge <- combineFrag(PaPi, frag = 9)
#' 
combineFrag = function(x, frag = NULL) {
  if (!(c("xRingList") %in% class(x))) {
    stop("Use only with \"xRingList\" objects.")
  }

  seriesName <- names(x)
  nNameLength <- unique(nchar(seriesName))
  if (length(nNameLength) == 1) {
    message("Series without fragments")
    return(x)
  }
  if (is.null(frag))
    frag <- nNameLength[length(nNameLength) - 1] + 1

  coresNames <- substr(seriesName, 1, frag - 1)
  uniqueSeries <- unique(coresNames)
  out <- vector("list", length(uniqueSeries))
  names(out) <- uniqueSeries

  for (i in unique(coresNames)) {
    out[i] <- putTogether(x[coresNames %in% i], newName = i)
  }
  class(out) <- c("xRingList", "list")
  return(out)
}
