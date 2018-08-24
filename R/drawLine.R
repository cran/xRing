#' @importFrom graphics abline axis lines mtext par plot points title
drawLine =  function(x, ...) {
  opar <- par("xpd")
  on.exit(par(opar))
  par("xpd" = NA)
  for (i in 1:length(x)) {
    lines(c(x[i], x[i]), c(-1.08, 1.425), ...)
  }
  par("xpd" = opar)
}

