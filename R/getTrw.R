getTrw <- function(x) {
  out <- x[[1]]$trw[, 1, drop = FALSE]
  for (i in 2:length(x)) {
    if (nrow(x[[i]]$trw) > 2) {
      out <- mergeRwl(x = out, y = x[[i]]$trw[, 1, drop = FALSE])
    }
  }
  out
}
