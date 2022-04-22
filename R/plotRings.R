#' @importFrom  grDevices grey
#' @title Plot xRing Objects
#' @description Plot "xRing" objects.
#' @param x an object of class "xRing"
#' @param xlim the x limits of the plot. The default value, NULL, indicates that the whole profile will be plotted.
#' @param ylim the y limits of the plot.
#' @param id a sufix to be added to the name of the series (<series_name> [id])
#' @param corr value to be print at the top of the graph
#' @param EwLw logical. If \code{TRUE} the earlywood and latewood assignments are plotted, by default is \code{TRUE}
#' @return  None. A plot is produced.
#' @seealso
#' \link{plot.xRing}
#' @importFrom graphics polygon
#' @export
#' @examples
#' if (interactive()) {
#'   data(PaPiRaw)
#'   data(PaPiSpan)
#'
#'   PaPi <- detectRings(PaPiRaw[, 1, drop = FALSE], PaPiSpan)
#'   plotRings(PaPi$AFO1001a)
#'   plotRings(PaPi, series = "AFO1001a")
#'   plotRings(PaPi, series = "AFO1001a", xlim = c(120, 450))
#'
#'   PaPi1 <- detectEwLw(PaPi, ew = 0.5)
#'   plotRings(PaPi1, series = "AFO1001a", EwLw = FALSE)
#'   plotRings(PaPi1, series = "AFO1001a")
#' }
#'
plotRings <- local({
  function(x,
           xlim = NULL,
           ylim = NULL,
           id = NULL,
           corr = NULL,
           EwLw = TRUE) {
    if (!inherits(x, "xRing")) {
      stop("Use only with 'xRing' objects.")
    }

    if (is.null(ylim)) {
      ylim <- range(x$profile, na.rm = TRUE)
    }
    AtY <- pretty.default(ylim, n = 4)
    ylim <- ylim - c(diff(ylim) / 1.25, 0)

    if (!is.null(id)) {
      id <- paste0("[", id, "]")
    }

    if (is.null(xlim)) {
      xlim <- c(0, length(x$profile))
    } else {
      xlim <- pmax(0, pmin(range(xlim), length(x$profile)))
    }

    par("mar" = c(3, 3, 6, 1))

    borders0 <- which(x$limits %in% seqRange(xlim))
    borders <- sort(borders0, decreasing = TRUE)[-1]

    plot(
      NA,
      type = "n",
      ylim = ylim,
      ann = FALSE,
      axes = FALSE,
      xaxs = "i",
      xlim = xlim
    )
    grey_color <- grey(1 - x$profile / max(x$profile, na.rm = TRUE))
    yUp <- ylim[1] + diff(ylim) / 3 # 6
    yUp1 <- ylim[1] + diff(ylim) / 9 # 18
    abline1(
      v = 1:length(x$profile),
      col = grey_color,
      y = c(-2000, yUp, yUp, -2000)
    )
    par(
      "mar" = c(3, 3, 6, 1),
      new = TRUE
    )

    plot(
      x$profile,
      type = "l",
      ylim = ylim,
      ann = FALSE,
      axes = FALSE,
      xaxs = "i",
      xlim = xlim
    )

    axis(2,
      line = 0.5,
      cex = 0.9,
      at = AtY
    )
    AT <- x$limits[-1]
    at2plot <- {
      AT >= xlim[1] & AT <= xlim[2]
    }
    Xyears <- x$years[-1]
    if (any(at2plot)) {
      mtext(
        Xyears[at2plot],
        at = AT[at2plot],
        adj = -.25,
        las = 2,
        padj = 0,
        col = "blue",
        cex = 1
      )
    }

    title(
      main = paste(x$name, id),
      line = 4,
      cex = 1
    )
    xx <- x$limits
    yy <- x$profile[x$limits]
    par(xpd = NA)
    points(xx, yy, col = 2)
    par(xpd = FALSE)
    abline(v = x$limits, col = 4)

    if (!is.null(corr)) {
      mtext(
        corr,
        line = 1.5,
        side = 3,
        col = 2,
        cex = 1,
        padj = -3,
        adj = 1,
        las = 1
      )
    }
    axis(1, line = 0.5)

    if (EwLw & !is.null(x$limits.ew)) {
      k <- .5
      k1 <- 1
      if (max(x$limits.lw - x$limits.ew) > 1) {
        k <- 0
        k1 <- 0
      }
      xEw <- x$limits.ew[borders]
      xLw <- x$limits.lw[borders]
      points(xEw, x$profile[xEw], pch = 3, col = 4)
      points(xLw - k1, x$profile[xLw - k1], pch = 4, col = 4)

      y <- c(rep(par("usr")[3], 2), yUp1, yUp1)
      if (max(x$limits.lw - x$limits.ew) > 1) {
        T0 <- x$limits[min(borders)]
        T1 <- x$limits[max(borders) + 1]
        xT <- c(T0, T1, T1, T0)
        polygon(xT, y, col = "grey60")
      }

      for (i in borders) {
        E0 <- x$limits[i]
        E1 <- x$limits.ew[i] #+ k
        L0 <- x$limits.lw[i] - k1
        L1 <- x$limits[i + 1]
        xE <- c(E0, E1, E1, E0)
        xL <- c(L0, L1, L1, L0)
        polygon(xE, y, col = "white")
        polygon(xL, y, col = "black")
      }
    }
  }
})
