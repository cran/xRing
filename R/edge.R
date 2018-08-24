
edge = function(x, k = 3, threshold = 50) {
  x.dif <- rollMax(x,
                   k = k,
                   align = "center") - rollMax(x,
                                               k = k,
                                               align = "right")
  x0 <- which(x.dif > threshold)
  breaks <- unique(c(which(dif(x0) > 1) - 1, length(x0)))
  return(x0[breaks])
}
