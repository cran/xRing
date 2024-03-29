\name{imRead}
\alias{imRead}
\title{Load Image From a File}
\usage{
imRead(file)
}
\arguments{
\item{file}{path to file}
}
\value{
an object of class "\link{cimg}"
}
\description{
Load an image using the \link{load.image} function from \link{imager} package
}
\examples{
if (interactive()) {
  file_path <- system.file("img", "AFO1046.1200dpi.png", package = "xRing")
  im <- imRead(file_path)
  imDisplay(im)
}

}
\seealso{
\link{load.image}
}
