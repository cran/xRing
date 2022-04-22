
border <- function(x, k = 3, threshold = 200) {
  x.dif <- rollMax(x,
    k = k,
    fill = NA,
    align = "right"
  ) + rollMax(-x,
    k = k,
    fill = NA,
    align = "center"
  )
  x0 <- which(x.dif > threshold)
  breaks <- unique(c(1, which(dif(x0) != 1), length(x0)))
  out <- NULL
  if (length(breaks) > 1) {
    for (i in 2:length(breaks)) {
      win <- x0[breaks[(i - 1)]:(breaks[(i)] - 1)]
      if (length(win) > 1) {
        win.dif <- dif(x[win])
        out <-
          c(out, win[which(win.dif == min(win.dif, na.rm = TRUE))])
      }
    }
  }
  floor(out)
}
