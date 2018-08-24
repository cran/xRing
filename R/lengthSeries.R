
lengthSeries = function (rwl) {
  apply(
    rwl,
    2,
    FUN = function(x) {
      length(na.omit(x))
    }
  )
}
