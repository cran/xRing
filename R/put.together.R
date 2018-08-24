put.together = function(x2, newName) {

  x2 <-
    x2[order(unlist((lapply(
      x2,
      FUN = function(x)
        x$span[[1]]
    ))), decreasing = F)]
  x2 <- lapply(x2, trimSeries)
  x2 <- lapply(x2, setName, newName)
  OUT <- x2[[1]]

  for (j in 2:length(x2)) {
    shift <- max(OUT$limits)
    OUT$profile.raw <- c(OUT$profile.raw, x2[[j]]$profile.raw[-1])
    OUT$span[[2]] <- x2[[j]]$span[[2]]
    OUT$trw <- rbind(OUT$trw,  x2[[j]]$trw)
    OUT$years <- c(OUT$years, x2[[j]]$years[-1])
    OUT$limits0 <- c(OUT$limits0, shift + x2[[j]]$limits0)
    OUT$limits <- c(OUT$limits, shift + x2[[j]]$limits[-1] - 1)
    if ("limits.ew" %in% names(OUT))
      OUT$limits.ew <- c(OUT$limits.ew, shift + x2[[j]]$limits.ew[-1] - 1)
    if ("limits.lw" %in% names(OUT))
      OUT$limits.lw <- c(OUT$limits.lw, shift + x2[[j]]$limits.lw[-1] - 1)
    if ("profile" %in% names(OUT))
      OUT$profile <- c(OUT$profile, x2[[j]]$profile[-1])
  }
  out <- vector("list", 1)
  names(out) <- newName
  out[[1]] <- OUT
  return(out)
}
