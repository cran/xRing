setLastYearSeries = function(x, lastYear) {
  x$years <- lastYear - ((length(x$years) - 1):0)
  rownames(x$trw) <- x$years[-1]
  x$span <- range(x$years)
  return(x)
}
