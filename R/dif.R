dif = function(x,
               lag = 1,
               differences = 1,
               ...) {
  c(rep(NA, lag * differences),
    diff(x, lag = lag, differences = differences, ...))
}
