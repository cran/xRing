
#' @name print
#' @title Print xRing and xRingList Objects
#' @description Print method for objects of class "xRing" and "xRingList".
#' @param x the object of class "xRing" or "xRingList" to print
#' @param ...	additional parameters
#' @return None
#' @export
#' @examples
#'
#' data(PaPiRaw)
#' data(PaPiSpan)
#' PaPi <- detectRings(PaPiRaw, PaPiSpan)
#' class(PaPi)
#' print(PaPi$AFO1001a)
#' PaPi$AFO1001a
#' PaPi$AFO1001a[]
#' print(PaPi)
#' PaPi
#'
#' @name print
#' @aliases print.xRing
#' @usage \method{print}{xRing}(x, ...)
#' @export
print.xRing <- function(x, ...) {
  cat(paste0(
    x$name, ": ", x$span[1], "-", x$span[2],
    " (Length: ", length(x$profile.raw), ")"
  ))
  invisible(x)
}


#' @name print
#' @aliases print.xRingList
#' @usage \method{print}{xRingList}(x, ...)
#' @export
print.xRingList <- function(x, ...) {
  nn <- names(x)
  ll <- length(x)

  nSeries <- formatC(sapply(seq.int(ll), FUN = toString))
  if (length(nn) != ll) {
    for (i in seq_len(ll)) {
      nn[i] <- x[[i]]$name
    }
  }
  nn <- formatC(nn)

  for (i in seq_len(ll)) {
    span <- as.vector(x[[i]]$span)
    len <- formatC(length(x[[i]]$profile.raw), width = 4)
    cat(paste0(nSeries[i], ": ", nn[i], ": ", span[1], "-", span[2], " (Length: ", len, ")\n"))
  }
  invisible(x)
}
