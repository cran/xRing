#helpers

plt <- function(im, xlim = c(1, width(im)), ylim = c(1, height(im))) {
  im <- as.cimg(im[seqRange(xlim), seqRange(ylim), , ])
  w <- width(im)
  h <- height(im)
  
  zoom <-
    max(round(w / tkDim()[1]), round(h / tkDim()[2]))
  im <- resizeIm(im, zoom)
  plot(im, ann = FALSE, axes = FALSE) #TODO  rescale = FALSE im / (2^16-1)
  op <- par(new = TRUE)
  on.exit(par(op))
  plot(
    NA,
    xlim = xlim, 
    ylim = ylim[2:1],
    ann = FALSE,
    axes = FALSE,
    asp = 1L,
    xaxs = "i",
    yaxs = "i"
  )
}


#' @title Crop Image Interactively
#' @description A GUI for cropping an image
#' @param im a cimg object
#' @return a cropped image 
#' @export
#' @examples
#' 
#' if(interactive()){
#'  file_path <-
#'   system.file("img", "AFO1046.1200dpi.png", package = "xRing")
#'   im <- imRead(file_path)
#'   print(dim(im))
#'   im_crop <- imCrop(im)
#'   print(dim(im_crop))
#'   }
#'
#' 
imCrop <- function(im) {
  FLAG <- FALSE
  ROI_TMP <- list(x = NULL, y = NULL)
  n <- 0
  done <- tclVar(0)
  xR0 <- xR <- 1:dim(im)[1]
  yR0 <- yR <- 1:dim(im)[2]
  tt <- tktoplevel()
  tcl("wm", "attribute", tt, alpha = 0)
  tktitle(tt) <- 'crop image'
  tt <-
    tkRplot(tt, width = tkDim()[1], height = tkDim()[2], function() {
      im <<- im[xR0, yR0, , , drop = FALSE]
      plt(im)
    })
  tcl("wm", "attribute", tt , alpha = 1)
  CAN <- tt$env$canvas
  tkconfigure(CAN, cursor = "cross")
  rplot <- function(...) {
    if (!identical(xR0 , xR) || !identical(yR0, yR)) {
      xR0 <<- xR
      yR0 <<- yR
      n <<- 0
      tkdelete(CAN, "TK_ROI_TMP")
      if (length(xR) > 10 && length(yR) > 10) {
        tkRreplot(tt)
      }
      # else{
      #   message("Are you kidding me?")
      # }
    }
  }
  
  tkMenu <- tkmenu(tt, tearoff = FALSE)
  tkadd(
    tkMenu,
    "command",
    label = "Crop",
    command = function() {
      if (n > 0)
        rplot()
    }
  )
  
  tkadd(
    tkMenu,
    "command",
    label = "Cancel ",
    command = function() {
      tkdelete(CAN, "TK_ROI_TMP")
      n <<- 0
    }
  )
  
  tkMenuExit <- tkmenu(tt, tearoff = FALSE)
  tkadd(
    tkMenuExit,
    "command",
    label = "Exit",
    command = function() {
      tkdelete(CAN, "TK_ROI_TMP")
      tclvalue(done) <<- 1
    }
  )
  
  
  addRoiTmp = function(W, xy, col = "#0000ff") {
    tkdelete(W, "TK_ROI_TMP")
    n <- length(xy$x)
    out <- vector("numeric", length = 2 * n)
    for (i in 1:n) {
      out[i * 2 - 1] <- xy$x[i]
      out[i * 2] <- xy$y[i]
    }
    roi <-
      tkcreate(W, "rectangle", out, outline = col, dash = "- -")
    tkaddtag(W, "TK_ROI_TMP", "withtag", roi)
  }
  
  Button1  = function(W, x, y, X, Y) {
    if (n > 0 && FLAG) {
      tkpopup(tkMenu, X, Y)
    }
    ROI_TMP$x <<- tkCanvasX0x(W, x)
    ROI_TMP$y <<- tkCanvasY0y(W, y)
  }
  
  Button1Motion = function(W, x, y) {
    nPoints <- length(ROI_TMP$x)
    if (nPoints == 1) {
      ROI_TMP$x <- c(ROI_TMP$x, tkCanvasX0x(W, x))
      ROI_TMP$y <- c(ROI_TMP$y, tkCanvasY0y(W, y))
      addRoiTmp(W, ROI_TMP)
    }
  }
  
  Button1Release = function(W, x, y, X, Y) {
    X0 <- tkCanvasX0(W)
    Y0 <- tkCanvasY0(W)
    nPoints <- length(ROI_TMP$x)
    x <- X0 + min(max(as.numeric(x), 0), tt$env$IMAGE_WIDTH)
    y <- Y0 + min(max(as.numeric(y), 0), tt$env$IMAGE_HEIGHT)
    
    if (ROI_TMP$x == x) {
      ROI_TMP <<- list(x = NULL, y = NULL)
      return()
    }
    if (ROI_TMP$y == y) {
      ROI_TMP <<- list(x = NULL, y = NULL)
      return()
    }
    
    ROI_TMP$x <<-  c(ROI_TMP$x, x)
    ROI_TMP$y <<-  c(ROI_TMP$y, y)
    
    roi <-
      tkcreate(CAN,
               "rectangle",
               coord2tcl(ROI_TMP),
               outline = "blue",
               dash = "- -")
    tkaddtag(CAN, "TK_ROI_TMP", "withtag", roi)
    n <<-  1
    
    cropXY <- tk2usr(ROI_TMP$x, ROI_TMP$y)
    xR <<- seqRange(round(xInsideImage(cropXY[1:2], width(im) + 1)))
    yR <<-
      seqRange(round(yInsideImage(cropXY[3:4], height(im) + 1)))
    FLAG <<- FALSE
    tkpopup(tkMenu, X, Y)
  }
  Button3 <- function(X, Y) {
    if (n == 1) {
      tkpopup(tkMenu, X, Y)
    }
    else{
      tkpopup(tkMenuExit, X, Y)
    }
  }
  
  tkbind(tt, "<3>", Button3)
  tkbind(CAN, "<Button-1>", Button1)
  tkbind(CAN, "<Motion>", Button1Motion)
  tkbind(CAN, "<B1-ButtonRelease>", Button1Release)
  tkbind(tt, "<q>", function()
    tclvalue(done) <- 1)
  tkbind(tt, "<Escape>", function()
    tclvalue(done) <- 1)
  
  tkwm.protocol(tt, "WM_DELETE_WINDOW", function() {
    if (n == 0) {
      tclvalue(done) <<- 1
    } else{
      tclvalue(done) <<- 3
    }
  })
  
  tkwait.variable(done)
  
  tkdestroy(tt)
  if (tclvalue(done) == "3") {
    if (length(xR) > 10 && length(yR) > 10) {
      im <- im[xR, yR, , , drop = FALSE]
    }
  }
  return(im)
}
