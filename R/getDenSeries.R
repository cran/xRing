getDenSeries =  function(x) {
  #if (!is.xRing(x)) {
  #  stop("Use only with \"xRing\" objects.")
  #}

  out <- x
  nYears <- length(x$limits) - 1
  years <- x$years[-1]

  dens.names <- c("Dmean", "Dmin", "Dmax", "Dew", "Dlw")
  OUT <-
    data.frame(matrix(
      NA,
      nrow =  nYears,
      5,
      dimnames = list(years, dens.names)
    ))

  i0 <- (x$limits[1] + 1)
  for (i in 1:nYears) {
    i1 <- x$limits[i + 1]
    xD <- x$profile[i0:i1]
    OUT[i, 1] <- mean(xD)
    OUT[i, 2] <- min(xD)
    OUT[i, 3] <- max(xD)

    if (!is.null(x$limits.ew[i])) {
      xEw <- 1:(x$limits.ew[i] - i0)
      OUT[i, 4] <- mean(xD[xEw])
      OUT[i, 5] <- mean(xD[-xEw])
    }
    i0 <- (i1 + 1)
  }
  out$density <- OUT
  return(out)
}
