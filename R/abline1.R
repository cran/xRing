#
#  @importFrom graphics polygon
#
abline1 = function(v = NULL, col = 2, y = c(-2000, 0, 0, -2000)) {
  n <- length(v)
  col1 <- col
  if (length(col) < n)
    col1 <- rep(col, n)
  if (!is.null(v))
    for (vi in 1:length(v))
      graphics::polygon(
       # x = v[vi] + c(-.5, -.5, .5, .5),
        x = v[vi] + c(0, 0, 1, 1),
        y = y,
        col = col1[vi],
        border = col1[vi]
      )
}
