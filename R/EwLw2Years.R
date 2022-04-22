EwLw2Years <- function(profile,
                       limits,
                       ew = 0.5,
                       lw = NULL) {
  if (is.null(lw)) {
    lw <- ew
  }

  profile <- profile[limits[1]:limits[2]]
  profile.std <- profile - min(profile)
  profile.std <- profile.std / max(profile.std)

  minDensMaxDens <- minDmaxD(profile.std)
  minDens <- minDensMaxDens[1]

  if (is.na(minDensMaxDens[1])) {
    minDensMaxDens[1] <- minDens <-
      max(which(profile.std == min(profile.std[1:round((diff(limits) + 1) * 2 /
        3)])))
  }

  if (is.na(minDensMaxDens[2])) {
    minDensMaxDens[2] <- maxDens <-
      min(which(profile.std == max(profile.std[-1:-round((diff(limits) + 1) *
        (2 / 3))])))
  }

  maxDens <-
    which(profile.std == max(profile.std[-1:-minDens])) # - 1
  maxDens <- min(maxDens, minDensMaxDens[2], na.rm = TRUE)

  firstValley <- min(minDens, findValleys(profile.std))
  profile.std[1:firstValley] <- min(profile.std[1:firstValley])
  profile.std <- profile.std - min(profile.std[1:maxDens])
  profile.std <- profile.std / max(profile.std)
  profile.std[profile.std < 0] <- 0


  getEwLw(profile.std,
    limits,
    maxDens = maxDens,
    ew = ew,
    lw = lw
  )
}
