#' @title Get Data-Frames With Ring Width and Density Values
#' @description Produce a list with 8 data.frames (trw, ew, lw, Dmean, Dew, Dlw, Dmin, Dmax ) that can be used by other packages (dplR, detrendeR)
#' @param x an "xRingList" object
#' @return a list with 8 elements:
##' \describe{
##'  \item{trw}{a data.frame with tree-ring widths}
##'  \item{ew}{a data.frame with earlywood widths}
##'  \item{lw }{a data.frame with latewood widths}
##'  \item{Dmean}{a data.frame with mean tree-ring density}  
##'  \item{Dew}{a data.frame with mean earlywood density} 
##'  \item{Dlw}{a data.frame with mean latewood density}  
##'  \item{Dmin}{a data.frame with the minimum ring density}
##'  \item{Dmax}{a data.frame with the maximum ring density}
##' }
#' @export
#' @importFrom dplR combine.rwl
#' @examples
#' 
#'  data(PaPiRaw)
#'  data(PaPiSpan)
#'  PaPi <- detectRings(PaPiRaw, PaPiSpan)
#'  PaPi <- combineFrag(PaPi)
#'  PaPi <- detectEwLw(PaPi)
#'  rwls <- getRwls(PaPi)
#'  names(rwls)
#'  library(dplR)
#'  rwl.report(rwls$trw)
#'  library(detrendeR)
#'  RwlInfo(rwls$trw)
#'  




getRwls <- function(x) {
  if (ncol(x[[1]]$trw) < 2)
    return(message("You should first detect the earlywood and latewood rings."))
  if (is.null(x[[1]]$density))
    x <- getDensity(x)
  nSeries <- length(x)
  name <- rep(NA, nSeries)
  trw <-
    ew <-
    lw <-
    Dtr <-
    Dmin <-
    Dmax <-
    Dew <-
    Dlw <- vector("list", nSeries)
  
  for (i in 1:length(x)) {
    name[i] <- x[[i]]$name
    trw[[i]] <- x[[i]]$trw[, 1, drop = FALSE]
    ew[[i]] <- x[[i]]$trw[, 2, drop = FALSE]
    lw[[i]] <-  x[[i]]$trw[, 3, drop = FALSE]
    Dtr[[i]] <- x[[i]]$density[, "Dmean", drop = FALSE]
    Dmin[[i]] <- x[[i]]$density[, "Dmin", drop = FALSE]
    Dmax[[i]] <- x[[i]]$density[, "Dmax", drop = FALSE]
    Dew[[i]] <- x[[i]]$density[, "Dew", drop = FALSE]
    Dlw[[i]] <- x[[i]]$density[, "Dlw", drop = FALSE]
  }
  

  trw <- combine.rwl(trw)
  ew <-  combine.rwl(ew)
  lw <- combine.rwl(lw)
  Dtr <- combine.rwl(Dtr)
  Dmin <- combine.rwl(Dmin)
  Dmax <- combine.rwl(Dmax)
  Dew <- combine.rwl(Dew)
  Dlw <- combine.rwl(Dlw)
  
  
  names(trw) <-
    names(ew) <-
    names(lw) <-
    names(Dtr) <-
    names(Dmin) <-
    names(Dmax) <-
    names(Dew) <-
    names(Dlw) <- name
  
  out <- list(trw, ew, lw, Dtr, Dew, Dlw, Dmin, Dmax)
  names(out) <-
    c('trw', 'ew', 'lw', 'Dmean', 'Dew', 'Dlw', 'Dmin', 'Dmax')
  return(out)
}