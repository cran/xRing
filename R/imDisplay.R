# helpers
#' @importFrom imager as.cimg
#'
#'

.tcl2num <- function(x) {
  as.numeric(unlist(strsplit(tclvalue(x), split = " ")))
}


tkDim <- function(win = ".") {
  as.numeric(unlist(strsplit(tclvalue(
    tkwm.maxsize(win)
  ), " ")))
}

resizeIm <- function(im, n = NULL, outputList = FALSE) {
  if (is.null(n)) {
    n <- round(max(width(im) / tkDim()[1]), height(im) / tkDim()[2])
  }
  if (n <= 1) {
    if (outputList) {
      return(list(image = im, zoom = 1))
    }
    return(im)
  }

  x <- seq(
    from = 1,
    to = width(im),
    by = n
  )
  y <- seq(
    from = 1,
    to = height(im),
    by = n
  )
  image <- as.cimg(im[x, y, , ])
  zoom <- n
  if (outputList) {
    return(list(image = image, zoom = zoom))
  }
  image
}



updateWindow <- function(tt) {
  IMAGE_WIDTH <- tt$env$IMAGE_WIDTH
  IMAGE_HEIGHT <- tt$env$IMAGE_HEIGHT
  XSCR <- tt$env$XSCR
  YSCR <- tt$env$YSCR
  widthT <- as.numeric(.Tcl(paste("winfo width", tt)))
  heightT <- as.numeric(.Tcl(paste("winfo height", tt)))
  if (widthT >= IMAGE_WIDTH) {
    tkgrid.forget(tt$env$hscroll)
    tkwm.maxsize(tt, IMAGE_WIDTH, .tcl2num(tkwm.maxsize(tt))[2])
  } else {
    tkgrid(
      tt$env$hscroll,
      row = 1,
      column = 0,
      sticky = "we"
    )
    tkwm.maxsize(tt, XSCR, .tcl2num(tkwm.maxsize(tt))[2])
  }
  if (heightT >= IMAGE_HEIGHT) {
    tkgrid.forget(tt$env$vscroll)
    tkwm.maxsize(tt, .tcl2num(tkwm.maxsize(tt))[1], IMAGE_HEIGHT)
  } else {
    tkgrid(
      tt$env$vscroll,
      row = 0,
      column = 1,
      sticky = "ns"
    )
    tkwm.maxsize(tt, .tcl2num(tkwm.maxsize(tt))[1], YSCR)
  }
}


showImage <- function(image = NULL, parent = .TkRoot, title = NULL) {
  IMAGE_WIDTH <- as.integer(tkimage.width(image))
  IMAGE_HEIGHT <- as.integer(tkimage.height(image))
  tt <- tktoplevel(parent = parent, background = "grey90")
  XSCR <- tkDim(tt)[1]
  YSCR <- tkDim(tt)[2]
  tkwm.maxsize(tt, min(IMAGE_WIDTH, XSCR), min(IMAGE_HEIGHT, YSCR))
  widthCan <- min(XSCR, IMAGE_WIDTH)
  heightCan <- min(YSCR, IMAGE_HEIGHT)

  tt$env$canvas <- tkcanvas(
    tt,
    width = widthCan,
    height = heightCan,
    scrollregion = paste(0, 0, IMAGE_WIDTH, IMAGE_HEIGHT)
  )

  tcl(tt$env$canvas,
    "create",
    "image",
    0,
    0,
    image = image,
    anchor = "nw"
  )

  tktitle(tt) <- title

  tt$env$hscroll <-
    tkscrollbar(tt,
      orient = "horiz",
      command = paste(tt$env$canvas$ID, "xview")
    )
  tt$env$vscroll <-
    tkscrollbar(tt, command = paste(tt$env$canvas$ID, "yview"))

  tkconfigure(
    tt$env$canvas,
    xscrollcommand = function(...) {
      tkset(tt$env$hscroll, ...)
    }
  )
  tkconfigure(
    tt$env$canvas,
    yscrollcommand = function(...) {
      tkset(tt$env$vscroll, ...)
    }
  )

  tkgrid(tt$env$canvas,
    row = 0,
    column = 0,
    sticky = "nswe"
  )
  tkgrid.rowconfigure(tt, 0, weight = 1)
  tkwm.minsize(tt, round(XSCR / 5), round(YSCR / 5))
  tkgrid.columnconfigure(tt, 0, weight = 1)
  tt$env$IMAGE_WIDTH <- IMAGE_WIDTH
  tt$env$IMAGE_HEIGHT <- IMAGE_HEIGHT
  tt$env$XSCR <- XSCR
  tt$env$YSCR <- YSCR

  tkbind(tt, "<Configure>", function() {
    updateWindow(tt)
  })

  if (IMAGE_WIDTH > XSCR | IMAGE_HEIGHT > YSCR) {
    # updateWindow(tt)
    tkwm.resizable(tt, 1, 1)
  } else {
    tkwm.resizable(tt, 0, 0)
  }
  return(tt)
}


#' @importFrom grDevices png

png2tcltk <- function(im, filePath = "tmp.png", title = NULL) {
  png(
    filename = filePath,
    width = width(im),
    height = height(im)
  )
  on.exit(unlink(filePath))
  par(mar = rep(0, 4))
  plot(im)
  dev.off()
  imageWork <-
    tkimage.create("photo", "xRingImageWork", file = filePath)
  showImage(imageWork, title = title)
}


#' @export
#' @title Display Image Using tcltk Package
#' @usage imDisplay(im, zoom = NULL, title = NULL)
#' @description xRing
#' @param im an image (an object of class "\link{cimg}")
#' @param zoom the zoom factor (ratio), for zoom = 1 the image is shown with no zoom (original size), when zoom is less than 1 the image is zoomed out. The default value of zoom is NULL.
#' @param title the window title
#' @return a tcltk object
#' @examples
#' if (interactive()) {
#'   file_path <- system.file("img", "AFO1046.1200dpi.png", package = "xRing")
#'   im <- imRead(file_path)
#'   tkWin <- imDisplay(im, zoom = .25)
#'   tkWin$env$ZOOM # 4 means 25% zoom
#' }
#'
imDisplay <- function(im, zoom = NULL, title = NULL) {
  title <- if (is.null(title)) {
    deparse(substitute(im))
  } else {
    title
  }
  if (!is.null(zoom)) zoom <- 1 / zoom
  im <- resizeIm(im, outputList = TRUE, n = zoom)
  title <- paste0(title, " [", round(1 / im$zoom, 2), "x]")
  tmpFile <- tempfile(tmpdir = tempdir(), fileext = ".png")
  tt <- png2tcltk(im$image, tmpFile, title = title)
  tt$env$ZOOM <- im$zoom
  return(invisible(tt))
}
