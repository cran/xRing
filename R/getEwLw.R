getEwLw =  function(profile,
                    limits,
                    maxDens = NULL,
                    ew = 0.5,
                    lw = NULL,
                    trueLw = NULL) {
  if (is.null(lw))
    lw <- ew

  if (diff(limits) == 0)
    return(rep(NA, 2))

  if (!is.null(trueLw)) {
    profile0 <- profile
    profile0[1:(round(1 / 3 * (diff(limits) + 1)) - 1)] <- 0
    maxDens <- min(which(profile0 >= trueLw))
    if (is.infinite(maxDens))
      maxDens <- round(2 / 3 * (diff(limits) + 1))
  }

  ew.end <- max(which((profile[1:maxDens] <= ew) == 1))
  if (is.infinite(ew.end))
    ew.end <- round(2 / 3 * (diff(limits) + 1))
  ew.limite <- limits[1] + ew.end - 1
  ew.limite <- min(ew.limite, limits[2] - 1)
  #to guarantee that latewood starts before the maxDensity
  profile[1:ew.end] <- 0
  lw.limite <- limits[1] + min(which((profile >= lw) == 1)) - 1
  if (!is.finite(lw.limite))
    lw.limite <- ew.limite + 1
  lw.limite <- min(lw.limite, limits[2])
  return(c(ew.limite, lw.limite))
}
