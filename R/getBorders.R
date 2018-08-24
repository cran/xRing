#' @title Get Tree-Ring Borders
#' @description Identify tree-ring borders
#' @param x an object of class "xRing"
#' @param k integer; width of the rolling window
#' @param minTrw integer; width of the narrowest tree-ring, rings narrower than this value will not be considered
#' @param threshold the minimum difference between the local maximum and minimum density to detect tree-ring borders
#' @param addLastBorder logical; if \code{FALSE} the last border is not added. If \code{TRUE} the last border is placed at the position of the last value.
#' @details
#' This function uses local maximum and minimum densities in order to detect tree-ring borders.
#' @return The \code{getBorders} function returns an object of lass "xRing" including the following elements:
#' @return \code{names} a \code{string} giving the series name
#' @return \code{span} the first and last year
#' @return \code{trw} a \code{data.frame} with tree-ring width
#' @return \code{limits} a \code{vector} with the position of the tree-ring borders
#' @return \code{years} a \code{vector} with the calendar year
#' @return \code{profile.raw} a \code{vector} with the raw X-ray values
#' @return \code{profile} a \code{vector} with the the smoothed X-ray values (if is supplied in the input)
#' @export
#' @examples
#' 
#' data("PaPiRaw")
#' data("PaPiSpan")
#' AFO1001a <- toxRing(PaPiRaw, PaPiSpan, "AFO1001a")
#' AFO1001a <- getBorders(AFO1001a)
#'
#' AFO1001a <- toxRing(PaPiRaw, seriesName = "AFO1001a")
#' AFO1001a <- getBorders(AFO1001a)
#'
#' 

getBorders <- function(x,
                      k = 3,
                      minTrw = 3,
                      threshold = 0.215,
                      addLastBorder = FALSE) {
  extractedProfile <- x$profile
  lastYear <- x$span[[2]]

  if (is.na(lastYear)) {
    lastYear <- as.integer(format(Sys.time(), "%Y")) - 1
    message(paste(x$name, lastYear, "#"))
    x$span[2] <- lastYear
  } else {
    message(paste(x$name, lastYear))
  }

  lastBorder <- NULL
  if (addLastBorder)
    lastBorder <- length(extractedProfile)

  limits <-
    Limits <-
    c(border(x = extractedProfile, k = k, threshold = threshold),
      lastBorder)
  limits0 <- NA
  problems <- which(dif(limits) < minTrw) - 1
  if (length(problems) > 0) {
    limits <- Limits[-problems]
    limits.problems <- Limits[which(dif(Limits) < minTrw) - 1]
  }
  years <- lastYear - (length(limits[-1]):0)

  x$trw <-
    as.data.frame(matrix(diff(limits), dimnames = list(years[-1], paste0(x$name, ".trw"))))
  x$limits <- limits
  x$years <- years
  x$limits0 <- limits0
  if (is.na(x$span[1]))
    x$span[1] <- years[1]
  x <- as.xRing(x)
  return(x)
}
