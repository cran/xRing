findValleys <- function(x) {
  which(diff(sign(diff(x, na.pad = FALSE)), na.pad = FALSE) > 0) + 1
}
