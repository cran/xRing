removeRingSeries = function(xray, x) {
  for (i in 1:length(x)) {
    xLimite <- abs(xray$limits - x[i])
    year <-
      xray$years[which(xLimite == min(xLimite))[1]]

    xray$limits <-
      limits <- xray$limits[-which(xray$years == year)]
    xray$years <- years <- xray$years[-1]
    xray$trw <- as.data.frame(matrix(diff(limits),
                                     dimnames = list(years[-1], xray$names)))
  }
  xray
}

