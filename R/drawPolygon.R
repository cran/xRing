drawPolygon <- function(x, y, r = 20) {
  x0 <- x[1]
  y0 <- y[1]
  for (i in 2:length(x)) {
    x1 <- x[2]
    y1 <- y[2]
    delta_x <- x0 - x1
    delta_y <- y0 - y1
    angle <- atan2(delta_y,  delta_x) * 180 / pi
    ang <- (angle + c(90, -90)) * pi / 180
    xx1 <- x1 + r * cos(ang)
    yy1 <- y1 + r * sin(ang)
    out <- c(xx1[2], yy1[2])
    xx0 <- x0 + r * cos(ang)
    yy0 <- y0 + r * sin(ang)
    out <- c(out, xx1[1], yy1[1], xx0[1], yy0[1], xx0[2], yy0[2])
  }
  return(out)
}
