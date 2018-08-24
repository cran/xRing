# helpers -----------------------------------------------------------------

divide <- function(x) {
  x[[1]] / x[[2]]
}

tkCanvasX0 <- function(canvas)
{
  as.numeric(tkcanvasx(canvas, 0))
}

tkCanvasY0 <- function(canvas)
{
  as.numeric(tkcanvasy(canvas, 0))
}

xInsideImage <- function(x, width)  {
  pmax(pmin(x, width - 1), 1)
}

yInsideImage <- function(y, height)
{
  pmax(pmin(y, height - 1), 1)
}

coord2tcl <- function(xy)
{
  as.vector(matrix(c(xy$x, xy$y), nrow = 2, byrow = TRUE))
}

rollMin <- function(x,
                    k,
                    align = c("center", "left", "right"),
                    fill = NA) {
  n_s <- length(x) - k + 1
  out <- rep(NA, n_s)
  for (i in seq_len(n_s)) {
    out[i] <- min(x[i + 0:(k - 1)])
  }
  out <- switch(
    match.arg(align),
    "left" = {
      c(out, rep(fill, k - 1))
    },
    "center" = {
      c(rep(fill, floor((k - 1) / 2)), out, rep(fill, ceiling((k - 1) / 2)))
    },
    "right" = {
      c(rep(fill, k - 1), out)
    }
  )
  return(out)
}


getStepsManual <- function(im, nSteps) {
  tt <- imDisplay(im)
  tktitle(tt) <- "Select Steps"
  tkconfigure(tt$env$canvas, cursor = "cross")
  done <- tclVar(0)
  n <- 0
  n.steps <- nSteps - 1
  ROI_TMP <- list(x = NULL, y = NULL)
  CAL_ROI <- matrix(NA, nrow = 0, ncol = 5)
  
  addRoiTmp = function(CAN, xy, col = "#ff0000") {
    tkdelete(CAN, "TK_ROI_TMP")
    n <- length(xy$x)
    out <- vector("numeric", length = 2 * n)
    for (i in 1:n) {
      out[i * 2 - 1] <- xy$x[i]
      out[i * 2] <- xy$y[i]
    }
    roi <- tkcreate(CAN, "rectangle", out, "-outline", col)
    tkaddtag(CAN, "TK_ROI_TMP", "withtag", roi)
  }
  
  Button1  = function(x, y) {
    if (n > n.steps)
      tclvalue(done) <- 3
    ROI_TMP$x <<- tkCanvasX0(tt$env$canvas) + as.numeric(x)
    ROI_TMP$y <<- tkCanvasY0(tt$env$canvas) + as.numeric(y)
  }
  
  Button1Motion = function(x, y) {
    nPoints <- length(ROI_TMP$x)
    if (nPoints == 1) {
      x <-
        xInsideImage(tkCanvasX0(tt$env$canvas) + as.numeric(x),
                     tt$env$IMAGE_WIDTH)
      y <-
        yInsideImage(tkCanvasY0(tt$env$canvas) + as.numeric(y),
                     tt$env$IMAGE_HEIGHT)
      ROI_TMP$x <- c(ROI_TMP$x, x)
      ROI_TMP$y <- c(ROI_TMP$y, y)
      addRoiTmp(tt$env$canvas, ROI_TMP)
    }
  }
  
  Button1Release = function(x, y) {
    nPoints <- length(ROI_TMP$x)
    x <-
      xInsideImage(tkCanvasX0(tt$env$canvas) + as.numeric(x),
                   tt$env$IMAGE_WIDTH)
    y <-
      yInsideImage(tkCanvasY0(tt$env$canvas) + as.numeric(y),
                   tt$env$IMAGE_HEIGHT)
    tkdelete(tt$env$canvas, "TK_ROI_TMP")
    if (n > n.steps) {
      ROI_TMP <<- list(x = NULL, y = NULL)
      return()
    }
    if (ROI_TMP$x == x) {
      ROI_TMP <<- list(x = NULL, y = NULL)
      return()
    }
    if (ROI_TMP$y == y) {
      ROI_TMP <<- list(x = NULL, y = NULL)
      return()
    }
    ROI_TMP$x <<- ROI_TMP$x <- c(ROI_TMP$x, x)
    ROI_TMP$y <<- ROI_TMP$y <- c(ROI_TMP$y, y)
    
    roi <-
      tkcreate(tt$env$canvas,
               "rectangle",
               coord2tcl(ROI_TMP),
               "-outline",
               "blue")
    tkaddtag(tt$env$canvas,
             paste0("TK_CAL_ROI_", n <<- n + 1),
             "withtag",
             roi)
    lab <-
      tcltk::tkcreate(
        tt$env$canvas,
        "text",
        mean(ROI_TMP$x),
        mean(ROI_TMP$y),
        text = as.character(n),
        fill = "red"
      )
    tkaddtag(tt$env$canvas, paste0("TK_CAL_ROI_LAB", n), "withtag", lab)
    XX <- seqRange(round(ROI_TMP$x * tt$env$ZOOM))
    YY <- seqRange(round(ROI_TMP$y * tt$env$ZOOM))
    CAL_ROI <<-
      rbind(CAL_ROI, c(coord2tcl(ROI_TMP), mean(im[XX, YY, ,])))
    
    if (n > n.steps)
      tclvalue(done) <- 2
    ROI_TMP <<- list("x" = NULL, "y" = NULL)
  }
  
  
  tkbind(tt$env$canvas, "<Button-1>", Button1)
  tkbind(tt$env$canvas, "<Motion>", Button1Motion)
  tkbind(tt$env$canvas, "<B1-ButtonRelease>", Button1Release)
  tkbind(tt, "<q>", function()
    tclvalue(done) <- 1)
  tkbind(tt, "<Escape>", function()
    tclvalue(done) <- 1)
  tkbind(tt, "<Return>", function()
    if (n > n.steps)
      tclvalue(done) <- 3)
  tkbind(tt, "<KP_Enter>", function()
    if (n > n.steps)
      tclvalue(done) <- 3)
  
  deleteLastStep <- function(x, y) {
    if (n > 0) {
      tkdelete(tt$env$canvas, paste0("TK_CAL_ROI_", n))
      tkdelete(tt$env$canvas, paste0("TK_CAL_ROI_LAB", n))
      CAL_ROI <<- CAL_ROI[-n, , drop = FALSE]
      n <<- n - 1
    }
  }
  tkbind(tt, "<Control-KeyPress-z>", deleteLastStep)
  
  tkwm.protocol(tt, "WM_DELETE_WINDOW", function()
    tk_messageBox("ok", "Please use 'Esc' or 'q' to close the window."))
  
  tkMenu <- tkmenu(tt, tearoff = FALSE)
  tkadd(tkMenu, "command", label = "Delete", command = deleteLastStep)
  tkadd(
    tkMenu,
    "command",
    label = "Exit",
    command = function() {
      if (n > n.steps)
      {
        tclvalue(done) <<- 3
      }
      else{
        tclvalue(done) <<- 1
      }
    }
  )
  
  .Tcl(#A function to pop up the menu
    "proc popupMenu {theMenu theX theY} {
    tk_popup $theMenu $theX $theY
}")

  .Tcl(paste("bind ", tt, " <3> {popupMenu ", tkMenu, " %X %Y}"))
  
  tkwait.variable(done)
  
  if (tclvalue(done) == "1") {
    tkdestroy(tt)
    return(NULL)
  }
  
  if (tclvalue(done) == "3") {
    tkdestroy(tt)
  } else{
    done <- tclVar(0)
    tkbind(tt, "<Destroy>", function() {
      tclvalue(done) <- 2
    })
    tkwm.protocol(tt, "WM_DELETE_WINDOW", function() {
      tclvalue(done) <- 2
    })
    tkwait.variable(done)
  }
  tkdestroy(tt)
  grayscale <- CAL_ROI[, 5]
  grayscale
  }




detectBreakpoints <- function(x,
                              nSteps = NULL,
                              minPeakHeight = NULL) {
  diffProfile <- rollMax(x, 5, "left") - rollMin(x, 5, "right")
  diffProfile[is.na(diffProfile)] <- 0
  diffProfile <- predict(smooth.spline(diffProfile, spar = 0.3))$y
  if (is.null(minPeakHeight))
    minPeakHeight <- round(max(diffProfile) / 60)
  peaks <- findPeaks(diffProfile)
  peaks <- peaks[diffProfile[peaks] >= minPeakHeight]
  rngSegDistance <- range(diff(peaks))
  
  while (divide(rngSegDistance) < .5) {
    minPeakHeight <- minPeakHeight * 1.05
    peaks <- findPeaks(diffProfile)
    peaks <- peaks[diffProfile[peaks] >= minPeakHeight]
    rngSegDistance <- range(diff(peaks))
  }
  
  if (is.null(nSteps))
    nSteps <- length(peaks)
  
  if (is.na(peaks[nSteps])) {
    if ((length(x) - max(peaks)) > (rngSegDistance[2] / 2)) {
      nSteps <- nSteps
      peaks <- c(peaks, length(x))
    }
  }
  peaks <- peaks[1:nSteps]
  peaks[!is.na(peaks)]
}



getStepsAuto <- function(im, nSteps = NULL, nPixel = 50) {
  message("Please draw a line from the step with lowest density to the step with highest density.")
  
  profile <- selectProfiles(im, nPixel, multiple = FALSE)
  breakpoints <- detectBreakpoints(profile, nSteps)
  group <- cut(seq_along(profile), c(1, breakpoints), right = FALSE)
  out <- tapply(profile, group, mean, na.rm = TRUE)
  
  plot(profile / 255,
       ylab = 'grayscale x 255',
       xlab = '',
       type = "l")
  abline(h = out / 255, col = 'red')
  as.numeric(out)
}


# main function -----------------------------------------------------------


#' @title Select the Steps of a Calibration Wedge Interactively 
#' @description Obtain the Grayvalue of Each Step of a Calibration Wedge
#' @param im an image.
#' @param nSteps number of steps of the calibration wedge to obtain grayvalues from.
#' @param auto logical. If TRUE, automatic detection of the steps given a line is carried out. Use with care.
#' @param nPixel gives the line width when `auto = TRUE`
#' @importFrom stats loess predict smooth.spline
#' @return a numeric vector
#' @export
#' @examples
#' if(interactive()){
#' # read a sample file
#'  im <- imRead(file = system.file("img", "AFO1046.1200dpi.png", package="xRing"))
#'
#' # display the image
#'   imDisplay(im)
#'
#' # get the grayvalues from the calibration wedge on the film
#'   steps <- grayvalues <- getSteps(im, 7) #select 7 ROIs
#'   steps1 <- grayvalues <- getSteps(im, 7, auto = TRUE) #select a single ROI
#'   cor(steps, steps1)
#' }
#'
getSteps <- function(im,
                     nSteps = NULL,
                     auto = FALSE,
                     nPixel = 50) {
  #input validation
  if (!"cimg" %in% class(im))
    stop("please provide an image of class 'cimg'")
  
  if (!is.null(nSteps) &&
      !(is.numeric(nSteps) &&
        length(nSteps) == 1 &&
        (nSteps %% 1 == 0)))
    stop(
      "please provide an numeric vector of length 1 containing a whole number as argument `nSteps`"
    )
  
  if (!(is.numeric(nPixel) &&
        length(nPixel) == 1 &&
        (nPixel %% 1 == 0)))
    stop(
      "please provide an numeric vector of length 1 containing a whole number as argument `nPixel`"
    )
  
  # main function
  if (auto) {
    getStepsAuto(im, nSteps, nPixel)
  } else {
    getStepsManual(im, nSteps)
  }
}
