EwLw2Series =  function(x, ew = 0.5, lw = NULL) {
  if (is.null(lw))
    lw <- ew
  n <- length(x$limits) - 1
  x$limits.ew <- x$limits.lw <- vector(mode = "integer", length = n)
  message(x$name)
  for (i in 1:n) {
    message(paste(x$years[i + 1], i))
    EwLw <-
      EwLw2Years(x$profile, x$limits[i + 0:1], ew = ew, lw = lw)
    x$limits.ew[i] <- EwLw[1]
    x$limits.lw[i] <- EwLw[2]
  }
  x$trw <-
    cbind(x$trw,
          data.frame(x$limits.ew - x$limits[1:n], x$limits[-1] - x$limits.lw))
  colnames(x$trw) <- paste(x$name, c("trw", "ew", "lw"), sep = ".")
  return(x)
}
