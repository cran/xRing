#' @title Set Last Year
#' @description
#' Changes the calendar year of the last ring for a specific series.
#' @param x an "xRing" or "xRingList" object
#' @param lastYear the new calendar year for the last tree ring
#' @param series individual series to be changed when the object is a "xRingList", by default is NULL
#' @return the modified input object with new set last ring of the specified series.
#' @export
#' @examples
#'
#' data(PaPiRaw)
#' data(PaPiSpan)
#' PaPi <- detectRings(PaPiRaw, PaPiSpan)
#' plot(PaPi, series = "AFO1001b")
#' PaPi <- setLastYear(PaPi, 2005, series = "AFO1001b")
#' plot(PaPi, series = "AFO1001b")
#'
setLastYear <- function(x, lastYear, series = NULL) {
  if (!any(c("xRingList", "xRing") %in% class(x))) {
    stop("Use only with \"xRingList\" and \"xRing\" objects.")
  }

  if (!is.null(series)) {
    x[[series]] <- setLastYearSeries(x[[series]], lastYear)
    return(x)
  }

  x <- setLastYearSeries(x, lastYear)
  return(x)
}
