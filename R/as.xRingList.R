# helper
#' @export
"[.xRingList" <-
  function(x, i, ...) {
    r <- NextMethod("[")
    class(r) <- class(x)
    r
  }

as.xRingList <- function(x) {
  x <- lapply(x, as.xRing)
  class(x) <- c("xRingList", "list")
  x
}
