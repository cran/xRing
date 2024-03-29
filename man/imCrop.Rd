\name{imCrop}
\alias{imCrop}
\title{Crop Image Interactively}
\usage{
imCrop(im)
}
\arguments{
\item{im}{a cimg object}
}
\value{
a cropped image
}
\description{
A GUI for cropping an image
}
\examples{

if (interactive()) {
  file_path <-
    system.file("img", "AFO1046.1200dpi.png", package = "xRing")
  im <- imRead(file_path)
  print(dim(im))
  im_crop <- imCrop(im)
  print(dim(im_crop))
}

}
