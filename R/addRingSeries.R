addRingSeries <- function(xray, x) {
  for (i in 1:length(x)) {
    xray$limits <- limits <- sort(c(xray$limits, round(x[i])))
    xray$years <- years <- c(xray$years[1] - 1, xray$years)
    xray$trw <- as.data.frame(matrix(diff(limits),
      dimnames = list(years[-1], xray$name)
    ))
  }
  xray
}
