as.xRing = function(x) {
  # x <- x[c(
  #   "profile.raw",
  #   "profile.smooth",
  #   "profile",
  #   "trw",
  #   "density",
  #   "limits.problems",
  #   "limits",
  #   "limits.ew",
  #   "limits.lw",
  #   "years",
  #   "scale",
  #   "span",
  #   "name"
  # )]
  x <- x[!is.na(names(x))]
  class(x) <- c("xRing", "list")
  return(x)
}
