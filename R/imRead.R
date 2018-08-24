#' @export
#' @title Load Image From a File
#' @description Load an image using the \link{load.image} function from \link{imager} package
#' @param file path to file
#' @return an object of class "\link{cimg}"
#' @seealso
#' \link{load.image}
#' @examples
#' if(interactive()){
#'  file_path <- system.file("img", "AFO1046.1200dpi.png", package="xRing")
#'  im <- imRead(file_path)
#'  imDisplay(im)
#'  }


imRead = function(file) {
  on.exit(gc())
  im <- load.image(file)
  if (dim(im)[4] > 1)
    im <- grayscale(im)
  #max_image <- max(im)
  max_image <- max(im[sample(1:nrow(im),10),sample(1:ncol(im),10),,]) #faster
  if (max_image <= 1)
    im <- im * 255
  if (max_image <= 2 ^ 8)
    im <- im * 255
  storage.mode(im) <- "integer"
  return(im)
}
