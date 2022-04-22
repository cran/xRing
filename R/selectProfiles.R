# helpers ----------------------------------------------------------------------
tkCanvasX0x <-
  function(canvas, x) {
    tkCanvasX0(canvas) + as.numeric(x)
  }
# as.numeric(tclvalue(tkcanvasx(canvas,x)) #TODO

tkCanvasY0y <-
  function(canvas, y) {
    tkCanvasY0(canvas) + as.numeric(y)
  }


addFrame <- function(im, nPixel = 1) {
  im <- squeeze(im)
  cM <- matrix(NA, nrow = dim(im)[1], ncol = nPixel)
  im1 <- cbind(cM, cM, im, cM)
  rM <- matrix(NA, nrow = nPixel, ncol = dim(im1)[2])
  im2 <- rbind(rM, rM, im1, rM)
  as.cimg(im2)
}

get_angle <- function(x, y) {
  delta_x <- x[1] - x[2]
  delta_y <- y[1] - y[2]
  -atan2(delta_y, delta_x) * 180 / pi
}

gcd <- function(x, y) {
  r <- x %% y
  return(ifelse(r, gcd(y, r), y))
}

crop_along_line <- function(x,
                            y,
                            im,
                            split = TRUE,
                            width = 40) {
  shift <- 2 * width
  im <- addFrame(im, shift)
  out <- NULL
  xx3 <- x + shift
  yy3 <- y + shift
  if (split & (abs(diff(x)) > 200 & abs(diff(y)) > 200)) {
    # this step is important for speed
    x_part <- diff(x) / abs(gcd(diff(x), diff(y)))
    xx3 <-
      seq(x[1], x[2], x_part) # divide image into equal parts according to the great common divisor
    yy3 <- seq(y[1], y[2], by = x_part / (diff(x) / diff(y)))
  }

  xx3 <- xx3 + shift
  yy3 <- yy3 + shift
  x_start <- xx3[1]
  y_start <- yy3[1]
  for (i in 2:length(xx3)) {
    x_end <- xx3[i]
    y_end <- yy3[i]
    xx <- c(x_start, x_end)
    yy <- c(y_start, y_end)
    len_profile <-
      round(sqrt(diff(xx3[i - 1:0])^2 + diff(yy3[i - 1:0])^2))
    xR <-
      range(as.integer(drawPolygon(
        c(x_start, x_end), c(y_start, y_end), 2 * width
      ))[1:4 * 2 - 1])
    yR <-
      range(as.integer(drawPolygon(
        c(x_start, x_end), c(y_start, y_end), 2 * width
      ))[1:4 * 2])
    im1 <- im[seqRange(xR), seqRange(yR), , , drop = FALSE]
    angle <- get_angle(xx, yy)
    im2 <- imrotate(im1, angle)
    x_crop <-
      seqRange(round(width(im2) / 2) + c(
        -floor(len_profile / 2),
        len_profile - floor(len_profile / 2) - 1
      ))
    y_crop <-
      seqRange(round(height(im2) / 2) + (c(-1, 1) * width / 2))
    im3 <- im2[x_crop, y_crop, , , drop = FALSE]
    out <- c(out, rowMeans(im3, na.rm = TRUE))
    x_start <- x_end
    y_start <- y_end
    # TODO merge all image segments and return also an image
  }
  out
}


#' @import imager
#' @title Select Profile(s)
#' @description Uses a line to select a profile (or a region of interest), when selecting a radius the line should start at the pith side and end at the bark side of the sample.
#' @param im an image
#' @param nPixel the width of the line
#' @param cal calibration
#' @param multiple a single or several profiles
#' @return a vector with the average grayvalue along the selected line when a multiple is TRUE and a list when multiple is FALSE
#' @examples
#' if (interactive()) {
#'   # read a sample file
#'   im <- imRead(file = system.file("img", "AFO1046.1200dpi.png", package = "xRing"))
#'
#'   # to display the image
#'   imDisplay(im)
#'
#'   # select a profile
#'   profile <- selectProfile(im)
#'
#'   # to display the profile
#'   plot(profile, type = "l")
#' }
#'
selectProfiles <- function(im,
                           nPixel = 50,
                           cal = NULL,
                           multiple = TRUE) {
  name <- ""
  thickness <- ""
  button1IsOk <- TRUE
  OUT <- NULL
  n <- 0
  done <- tclVar(0)
  tt <- imDisplay(im)
  tktitle(tt) <- ifelse(multiple, "GetProfiles", "GetOneProfile")
  CAN <- tt$env$canvas
  ZOOM <- tt$env$ZOOM
  nPixel <- nPixel / ZOOM

  ROI_TMP <- list(x = NULL, y = NULL)
  tkconfigure(tt$env$canvas, cursor = "cross")


  addRoiTmp <- function(CAN, xy, col = "#ff0000") {
    tkdelete(CAN, "TK_ROI_TMP")
    polygon_coord <- as.integer(drawPolygon(xy$x, xy$y, nPixel / 2))
    roi <-
      tkcreate(CAN,
        "polygon",
        polygon_coord,
        fill = "",
        outline = col
      )
    tkaddtag(CAN, "TK_ROI_TMP", "withtag", roi)
    roi <-
      tkcreate(
        CAN,
        "line",
        coord2tcl(xy),
        fill = col,
        width = 1,
        dash = "--"
      )
    tkaddtag(CAN, "TK_ROI_TMP", "withtag", roi)
  }

  Button1 <- function(W, x, y) {
    if (!multiple &
      n > 0) {
      # if there is a profile and multiple is FALSE return the profile and closes the window
      return(tkdestroy(tt))
    } # TODO call a dialog box to know what to do
    if (!button1IsOk) {
      return()
    }
    ROI_TMP$x <<-
      xInsideImage(tkCanvasX0x(W, x), tt$env$IMAGE_WIDTH)
    ROI_TMP$y <<-
      yInsideImage(tkCanvasY0y(W, y), tt$env$IMAGE_HEIGHT)
  }


  Button1Motion <- function(W, x, y, ...) {
    if (!multiple & n > 0) {
      return()
    }
    nPoints <- length(ROI_TMP$x)
    if (nPoints == 1) {
      x <- xInsideImage(tkCanvasX0x(W, x), tt$env$IMAGE_WIDTH)
      y <- yInsideImage(tkCanvasY0y(W, y), tt$env$IMAGE_HEIGHT)
      ROI_TMP$x <- c(ROI_TMP$x, x)
      ROI_TMP$y <- c(ROI_TMP$y, y)
      ifelse(((
        abs(ROI_TMP$x - x) + abs(ROI_TMP$y - y)
      )) < nPixel / 2, col <- "blue", col <- "red")
      addRoiTmp(tt$env$canvas, ROI_TMP, col = col)
    }
  }


  Button1Release <- function(W, x, y, ...) {
    if (!button1IsOk) {
      tklower(tt)
      return()
    }
    if (!multiple & n > 0) {
      return()
    }
    gc()
    on.exit(ROI_TMP <<- list("x" = NULL, "y" = NULL))
    nPoints <- length(ROI_TMP$x)
    x <- xInsideImage(tkCanvasX0x(W, x), tt$env$IMAGE_WIDTH)
    y <- yInsideImage(tkCanvasY0y(W, y), tt$env$IMAGE_HEIGHT)

    if ((ROI_TMP$x == x &&
      ROI_TMP$y == y) |
      ((abs(ROI_TMP$x - x) + abs(ROI_TMP$y - y)) < nPixel / 2)) {
      return()
    }

    ROI_TMP$x <- x <- c(ROI_TMP$x, x)
    ROI_TMP$y <- y <- c(ROI_TMP$y, y)

    polygon_coord <- as.integer(drawPolygon(x, y, nPixel))
    xx <- round(x * ZOOM)
    yy <- round(y * ZOOM)
    ROI_TMP <<- list("x" = NULL, "y" = NULL)
    if (multiple) {
      currentSeries <-
        varEntryDialog(
          vars = c("name", "lastYear", "thickness"),
          valInitials = c(name, "", thickness),
          fun = list(make.names, AsNumericNaN, AsNumeric)
        )

      tkdelete(CAN, "TK_ROI_TMP")
      if (!is.null(currentSeries)) {
        name <<- currentSeries$name
        thickness <<-
          ifelse(is.na(currentSeries$thickness),
            "",
            currentSeries$thickness
          )
        n <<- n + 1
        polygon_coord <- as.integer(drawPolygon(x, y, nPixel / 2))
        roi0 <-
          roi <-
          tkcreate(CAN,
            "polygon",
            polygon_coord,
            fill = "",
            outline = "yellow"
          )
        tkaddtag(CAN, paste0("ray", n), "withtag", roi)
        roi <-
          tkcreate(
            CAN,
            "line",
            coord2tcl(ROI_TMP),
            fill = "yellow",
            width = 1,
            dash = "--"
          )
        tkaddtag(CAN, paste0("ray", n), "withtag", roi)

        profile.raw <- crop_along_line(xx, yy, im, width = nPixel)
        if (multiple) {
          name <-
            make.names(c(names(OUT), currentSeries$name), unique = TRUE)
          name <- name[length(name)]
        }
        profileCalibrated <- NULL

        if (!is.null(cal)) {
          profileCalibrated <- calibrateSeries(profile.raw,
            thickness = currentSeries$thickness,
            calibration = cal
          )
        }
        OUT[[n]] <<- list(
          "coordinates" = list(x = x * ZOOM, y = y * ZOOM),
          "nPixel" = nPixel * ZOOM,
          "profile.raw" = profile.raw,
          "profile" = profileCalibrated,
          "thickness" = currentSeries$thickness,
          "span" = c(NA, AsNumeric(currentSeries$lastYear)),
          "name" = name
        )

        names(OUT)[n] <<- name
      }
    } else {
      tkdelete(CAN, "TK_ROI_TMP")
      {
        n <<- n + 1
        polygon_coord <- as.integer(drawPolygon(x, y, nPixel / 2))
        roi0 <-
          roi <-
          tkcreate(CAN,
            "polygon",
            polygon_coord,
            fill = "",
            outline = "yellow"
          )
        tkaddtag(CAN, paste0("ray", n), "withtag", roi)
        roi <-
          tkcreate(
            CAN,
            "line",
            coord2tcl(ROI_TMP),
            fill = "yellow",
            width = 1,
            dash = "--"
          )
        tkaddtag(CAN, paste0("ray", n), "withtag", roi)
        OUT[[n]] <<- crop_along_line(xx, yy, im, width = nPixel)
      }
    }
    tkraise(tt)
  }
  tkbind(tt$env$canvas, "<Button-1>", Button1)
  tkbind(tt$env$canvas, "<Motion>", Button1Motion)
  tkbind(tt$env$canvas, "<B1-ButtonRelease>", Button1Release)
  tkbind(tt, "<q>", function() {
    tclvalue(done) <- 1
  })
  tkbind(tt, "<Escape>", function() {
    tclvalue(done) <- 1
  })
  tkbind(tt, "<Return>", function() {
    tclvalue(done) <- 3
  })
  tkbind(tt, "<KP_Enter>", function() {
    tclvalue(done) <- 3
  })

  tkbind(tt, "<Control-KeyPress-z>", function(x, y) {
    if (n >= 0) {
      tkdelete(tt$env$canvas, paste0("ray", n))
      tkdelete(CAN, "TK_ROI_TMP")
      OUT <<- OUT[-n, drop = FALSE]
      n <<- n - 1
    }
  })

  tkwm.protocol(tt, "WM_DELETE_WINDOW", function() {
    done <- tclVar(1)
  })

  # tkwait.variable(done)

  if (tclvalue(done) == "1") {
    tkdestroy(tt)
    return(NULL)
  }

  if (tclvalue(done) == "3") {
    tkdestroy(tt)
  } else {
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

  if (n == 0) {
    return(NULL)
  }

  if (!multiple) {
    return(OUT[[1]])
  }

  as.xRingList(OUT)
}
