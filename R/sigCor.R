#' @importFrom stats qt
sigCor <- function(p = 0.05,
                   n = 50,
                   tail = 2) {
  t <- qt((p / tail), n - 2)
  t / (sqrt((n - tail) + t^2))
}
