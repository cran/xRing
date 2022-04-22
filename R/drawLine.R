#' @importFrom graphics abline axis lines mtext par plot points title
drawLine <- function(x, ...) {
  opar <- par("xpd" = NA)
  on.exit(par(opar))
  for (i in 1:length(x)) {
    lines(c(x[i], x[i]), c(-1.08, 1.425), ...)
  }
}
