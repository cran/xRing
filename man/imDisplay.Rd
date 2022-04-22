\name{imDisplay}
\alias{imDisplay}
\title{Display Image Using tcltk Package}
\usage{
imDisplay(im, zoom = NULL, title = NULL)
}
\arguments{
\item{im}{an image (an object of class "\link{cimg}")}

\item{zoom}{the zoom factor (ratio), for zoom = 1 the image is shown with no zoom (original size), when zoom is less than 1 the image is zoomed out. The default value of zoom is NULL.}

\item{title}{the window title}
}
\value{
a tcltk object
}
\description{
xRing
}
\examples{
if (interactive()) {
  file_path <- system.file("img", "AFO1046.1200dpi.png", package = "xRing")
  im <- imRead(file_path)
  tkWin <- imDisplay(im, zoom = .25)
  tkWin$env$ZOOM # 4 means 25\% zoom
}

}
