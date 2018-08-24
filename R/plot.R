# helpers
years2xLim <- function(x, years){
  years <- pmin(pmax(years, min(x$years) + 1), max(x$years))
  YEARmin <- which(x$years == min(years - 1))
  YEARmax <- which(x$years == max(years))
  c(range(x$limits[YEARmin:YEARmax])) + c(-1, 1)
}

#' @name plot
#' @title Plot xRing and xRingList Objects
#' @description Plot method for objects of class "xRing" and "xRingList".
#' @param x an object of class "xRing" or "xRingList".
#' @param years the years to be plotted, if \code{NULL} the whole time span is plotted.
#' @param EwLw logical. If \code{TRUE} the earlywood and latewood boundaries and width is plotted.
#' @param xlim vector of length 2 giving the x limits for the plot.
#' @param ylim the y limits of the plot.
#' @param ...	other arguments to be passed to plotRings function
#' @return None.
#' @seealso
#' \link{plotRings}
#' @export
#' @examples
#' 
#'  data(PaPiRaw)
#'  data(PaPiSpan)
#'
#'  PaPi <- detectRings(PaPiRaw, PaPiSpan)
#'  class(PaPi)
#'
#'  PaPiRings <- detectEwLw(PaPi, ew = 0.5)
#'  plot(PaPiRings, series = "AFO1001a")
#'
#'  PaPiRings1 <- detectEwLw(PaPi, ew = 0.35, lw = 0.55)
#'  plot(PaPiRings1, series = "AFO1001a")
#'
#'  plot(PaPiRings, series = "AFO1001a", years = c(1990,2000))
#'  plot(PaPiRings$AFO1001a)
#'  
#'
#'


#' @name plot
#' @aliases plot.xRing
#' @export
#' @usage \method{plot}{xRing}(x, years = NULL, EwLw = TRUE,  xlim = NULL, ylim =NULL, ...)

plot.xRing <- function(x,
                      years = NULL,
                      EwLw = TRUE,
                      xlim = NULL,
                      ylim = NULL,
                      ...
) {
  mar <- par("mar")
  on.exit(par("mar" = mar))
  if (!is.null(years))
    xlim <- years2xLim(x, years)
  local(plotRings(x,  EwLw = EwLw, xlim = xlim, ylim = ylim, ...))
}


#' @name plot
#' @aliases plot.xRingList
#' @param series gives the name (or the index) of the series to be plotted, by default is 1 (i.e., the first series)
#' @export
#' @usage \method{plot}{xRingList}(x, series = 1, years = NULL, EwLw = TRUE,  xlim = NULL, ylim = NULL, ...)



plot.xRingList <- function(x,
                          series = 1,
                          years = NULL,
                          EwLw = TRUE,
                          xlim = NULL,
                          ylim = NULL,
                          ...) {

    if (series %in% as.character(1:length(x))) {
      series <- names(x)[as.integer(series)]
    }
    seriesID <- which(names(x) == series)
    if (length(seriesID) == 0)
      return(message(
        paste("Please correct the series name or ID (ID <", length(x), ")")
      ))
    x <- x[[seriesID]]

  if (!is.null(years))
    xlim <- years2xLim(x, years)
  mar <- par("mar")
  on.exit(par("mar" = mar))
  local(plotRings(x, EwLw = EwLw, xlim = xlim, ylim = ylim, ...))

}
