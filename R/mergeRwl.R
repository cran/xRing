mergeRwl <- function(x, y) {
  X <- as.list(as.data.frame(x))
  Y <- as.list(as.data.frame(y))
  rangeX <- range(as.integer(row.names(x)))
  rangeY <- range(as.integer(row.names(y)))

  ADD.na <- function(x, n = 0) {
    c(rep(NA, n), x)
  }
  ADD.na.End <- function(x, n = 0) {
    c(x, rep(NA, n))
  }

  FLAG <- rangeX - rangeY
  if (FLAG[1] != 0) {{ if (FLAG[1] < 0) {
    lapply(Y, FUN = ADD.na, n = abs(FLAG[1])) -> Y
  } else {
    lapply(X, FUN = ADD.na, n = abs(FLAG[1])) -> X
  } }}

  if (FLAG[2] != 0) {{ if (FLAG[2] < 0) {
    lapply(X, FUN = ADD.na.End, n = abs(FLAG[2])) -> X
  } else {
    lapply(Y, FUN = ADD.na.End, n = abs(FLAG[2])) -> Y
  } }}

  list(X, Y) -> out
  as.data.frame(out) -> out
  rownames(out) <-
    min(rangeX[1], rangeY[1]):max(rangeX[2], rangeY[2])
  out
}
