minDmaxD = function(y) {
  mM1 <-
    c(min(c(findValleys(y), Inf)), max(findPeaks(y)))
  if (any(is.infinite(mM1))) {
    mM1[is.infinite(mM1)] <- NA
    return(mM1)
  }

  if (y[mM1[2]] < 0.9)
    mM1[2] <- length(y)
  yTrim <- y[mM1[1]:mM1[2]]
  return(c(min(which(
    yTrim %in% range(yTrim)[1]
  )), min(which(
    yTrim %in% range(yTrim)[2]
  ))) +
    mM1[1] - 1)
}
