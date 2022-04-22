#' @title Correct Tree-Ring Borders Interactively
#' @description A Graphical User Interface (GUI) to correct tree-ring borders
#' @param x an \code{xRingList} object
#' @param chrono a data.frame with a reference chronology, if \code{NULL} a reference chronology is calculated using tree-ring width series from \code{x}
#' @details
#' This function uses the \code{tkRplot} function (tkRplotR package) to interact with X-ray microdensity profiles.
#' @return an \code{xRingList} object
#' @import tkRplotR
#' @importFrom stats cor
#' @importFrom stats na.omit
#' @importFrom grDevices dev.off
#' @export
#' @examples
#' if (interactive()) {
#'   data(PaPiRaw)
#'   data(PaPiSpan)
#'   PaPi <- detectRings(PaPiRaw, PaPiSpan)
#'   PaPiCorrect <- correctRings(PaPi)
#' }
#'
correctRings <- local({
  parent <- NULL
  flagZoom <- FALSE
  flagMotion <- FALSE
  done <- tclVar(0)
  xOriginal <- NULL
  ptsX <- c()
  ptsX0 <- c()
  ptsX1 <- c()
  series <- 1
  nSeries <- NULL
  selectSeries <- NULL
  x0 <- 0
  x1 <- NULL

  function(x, chrono = NULL) {
    tclValue2R <- function(x, numeric = TRUE) {
      if (numeric) {
        return(as.numeric(strsplit(tclvalue(x), " ")[[1]]))
      }
      # if (numeric) as.numeric(unlist(strsplit(tclvalue(x), split = ' ')))
      strsplit(tclvalue(x), " ")[[1]]
    }

    usr2tk <- function(x, y = NULL) {
      c((x - getVariable("xCoef")[2]) / getVariable("xCoef")[1])
    }

    if (!any(c("xRingList") %in% class(x))) {
      stop("Use only with \"xRingList\" objects.")
    }

    if (is.null(x[[1]]$years)) {
      stop("Please first use the detectRings function!")
    }

    if (is.null(chrono)) {
      chrono <- as.data.frame(rowMeans(getTrw(x), na.rm = TRUE))
    }

    done <<- tclVar(0)
    xOriginal <<- x
    ptsX <<- c()
    ptsX1 <<- c()
    series <<- 1
    nSeries <<- length(x)
    selectSeries <<- x[[series]]
    x0 <<- 0
    x1 <<- length(selectSeries$profile.raw)

    try(tkdestroy(parent), silent = TRUE)

    InsideCanvas <- function(x) {
      pmin(pmax(x, getVariable("usr")[1] + 1), getVariable("usr")[2])
    }

    InsideCanvasTkX <- function(x) {
      pmin(pmax(x, 1), getVariable("tkRplotRcanvasWidth") - 2)
    }

    PreviousSeries <- function() {
      ptsX <<- c()
      tkdelete(parent$env$canvas, "TK_TMP")
      series <<- max(1, series - 1)
      selectSeries <<- x[[series]]
      x0 <<- 0
      x1 <<- length(selectSeries$profile.raw)
      tkRreplot(parent)
    }

    NextSeries <- function() {
      ptsX <<- c()
      tkdelete(parent$env$canvas, "TK_TMP")
      series <<- min(nSeries, series + 1)
      selectSeries <<- x[[series]]
      x0 <<- 0
      x1 <<- length(selectSeries$profile.raw)
      tkRreplot(parent)
    }

    GoTo <- function() {
      seriesID <- c()
      series.name <-
        modalDialog(question = "Go to the next series:")
      if (length(series.name) != 0) {
        if (series.name %in% as.character(1:nSeries)) {
          seriesID <- as.integer(series.name)
          series.name <- names(x)[seriesID]
        } else {
          seriesID <- which(names(x) == series.name)
        }
      }

      if (!length(seriesID)) {
        # The user click the cancel button or seriesID is not a valid name
        invisible(tkmessageBox(
          message = "please select a series",
          icon = "warning",
          type = "ok"
        ))
      } else {
        series <<- seriesID
        ptsX <<- c()
        tkdelete(parent$env$canvas, "TK_TMP")
        selectSeries <<- x[[series]]
        x0 <<- 0
        x1 <<- length(selectSeries$profile.raw)
        tkRreplot(parent)
      }
    }

    AddRing <- function() {
      if (!is.null(ptsX)) {
        ptsX <- InsideCanvas(ptsX)
        x <<- addRing(x, unique(ptsX), series)
        selectSeries <<- x[[series]]
        tkRreplot(parent)
        ptsX <<- c()
        tkdelete(parent$env$canvas, "TK_TMP")
      }
    }

    RemoveRing <- function() {
      if (!is.null(ptsX)) {
        x <<- removeRing(x, ptsX, series)
        selectSeries <<- x[[series]]
        tkRreplot(parent)
        ptsX <<- c()
        tkdelete(parent$env$canvas, "TK_TMP")
      }
    }

    SetLastYear <- function() {
      NOT_VALID_YEAR <- FALSE
      last.year <-
        modalDialog(question = "   Last year:", entryInit = "", entryWidth = 5)
      last.year <- AsNumeric(last.year)

      if (!length(last.year)) {
        NOT_VALID_YEAR <- TRUE
      } else {
        if (!is.na(last.year)) {
          selectSeries <<- x[[series]] <<- setLastYear(x[[series]], last.year)
          ptsX <<- c()
          tkdelete(parent$env$canvas, "TK_TMP")
        } else {
          NOT_VALID_YEAR <- TRUE
        }
      }
      if (NOT_VALID_YEAR) {
        return(invisible(
          tkmessageBox(
            message = "Not a valid year",
            icon = "warning",
            type = "ok"
          )
        ))
      }
      tkRreplot(parent)
    }

    Zoom <- function() {
      if (length(ptsX) == 0) {
        x0 <<- 0
        x1 <<- length(selectSeries$profile.raw)
        tkRreplot(parent)
      } else {
        if (length(ptsX) == 1) {
          tkmessageBox(
            message = "2 limits are need to zoom in",
            icon = "warning",
            type = "ok"
          )
        } else {
          x.limits <- range(ptsX)
          x0 <<- x.limits[1]
          x1 <<- x.limits[2]
          ptsX <<- ptsX <- c()
          selectSeries <<- x[[series]]
          tkRreplot(parent)
          tkdelete(parent$env$canvas, "TK_TMP")
        }
      }
    }


    GetOriginalSeries <- function() {
      if (!identical(x[[series]], xOriginal[[series]])) {
        selectSeries <<- x[[series]] <<- xOriginal[[series]]
        tkRreplot(parent)
      }
      ptsX <<- c()
      tkdelete(parent$env$canvas, "TK_TMP")
      Zoom()
    }

    Clear <- function(...) {
      ptsX <<- c()
      tkdelete(parent$env$canvas, "TK_TMP")
    }

    Save <- function() {
      tclvalue(done) <- 1
    }

    Cancel <- function() {
      tclvalue(done) <- 2
    }

    Close <- function() {
      ans <- tk_messageBox("yesnocancel", "Save changes?")
      if (ans == "cancel") {
        return()
      }
      tkdestroy(parent)
      if (ans == "yes") {
        tclvalue(done) <- 1
      }
      if (ans == "no") {
        tclvalue(done) <- 2
      }
    }

    AddButton <- function(parent, name, fun) {
      for (i in 1:length(name)) {
        tkpack(
          tkbutton(
            parent,
            text = name[i],
            command = fun[[i]],
            relief = "solid"
          ),
          fill = "x",
          pady = 2,
          padx = 4
        )
      }
    }

    parent <<- tktoplevel(width = 1, height = 1)
    tkwm.withdraw(parent)
    parent$env$fL <- tkframe(parent, padx = 3)
    tktitle(parent) <- "CorrectRings"
    tkpack(parent$env$fL, side = "left", fill = "y")
    parent <- tkRplot(parent, function(...) {
      plotRings(
        selectSeries,
        xlim = c(x0, x1),
        id = series,
        corr = round(cor(na.omit(
          mergeRwl(chrono, selectSeries$trw)
        ))[1, 2], 2)
      )
    })

    tkconfigure(parent$env$canvas, cursor = "cross")


    AddButton(
      parent$env$fL,
      c(
        "Go to",
        "Previous",
        "Next",
        "Add ring",
        "Remove Ring",
        "Zoom",
        "Last Year",
        "Reset",
        "Clear",
        "Close"
      ),
      list(
        GoTo,
        PreviousSeries,
        NextSeries,
        AddRing,
        RemoveRing,
        Zoom,
        SetLastYear,
        GetOriginalSeries,
        Clear,
        Close
      )
    )

    tkbind(parent$env$canvas, "<Button-1>", function(W, x, y, ...) {
      ptsX0 <<- x
      ptsX <<- c(ptsX, tk2usr(x, y)[1])
      yLimits <-
        (1 - getVariable("plt")[3:4]) * getVariable("tkRplotRcanvasHeight")
      roi <-
        tkcreate(
          W,
          "line",
          c(x, yLimits[1], x, yLimits[2]),
          fill = "#ff0000",
          width = 2,
          dash = "- -"
        )
      tkaddtag(W, "TK_TMP", "withtag", roi)
      flagZoom <<- TRUE
    })

    tkbind(parent$env$canvas, "<B1-ButtonRelease>", function(W, x, y, ...) {
      tkconfigure(parent$env$canvas, cursor = "cross")
      ptsX1 <<- tk2usr(x, y)[1]
      if (!flagMotion) {
        flagZoom <<- FALSE
        ptsX <- InsideCanvas(c(ptsX, ptsX1))
        return()
      }
      if (flagZoom) {
        flagZoom <<- FALSE
        tkdelete(W, "TK_TMP_MOVE")
        if ((ptsX1 - ptsX[1]) < 0 & length(ptsX) == 1) {
          ptsX <<- c()
          tkdelete(W, "TK_TMP")
          Zoom()
          return()
        } else {
          if (identical(ptsX1, ptsX)) {
            ptsX <<- InsideCanvas(ptsX)
            return()
          }
          if (abs(ptsX1 - ptsX[1]) < 10) {
            tkdelete(W, "TK_TMP")
            ptsX <<- c()
          }
        }
        if (length(ptsX) == 1) {
          tkdelete(W, "TK_TMP")
          ptsX <<- InsideCanvas(c(ptsX, ptsX1))
          Zoom()
        }
      }
      flagZoom <<- FALSE
    })

    tkbind(parent$env$canvas, "<B1-Motion>", function(W, x, y, ...) {
      x <- InsideCanvasTkX(as.numeric(x))
      flagMotion <<- FALSE
      if (length(ptsX) == 1 &
        flagZoom) {
        tkconfigure(parent$env$canvas, cursor = "hand2")
        flagMotion <<- TRUE
        tkdelete(W, "TK_TMP_MOVE")
        yLimits <-
          (1 - getVariable("plt")[3:4]) * getVariable("tkRplotRcanvasHeight")
        roi <-
          tkcreate(
            W,
            "line",
            c(
              ptsX0,
              yLimits[1] + 2,
              ptsX0,
              yLimits[2] - 2,
              x,
              yLimits[2] - 2,
              x,
              yLimits[1] + 2,
              ptsX0,
              yLimits[1] + 2
            ),
            fill = "#ff0000",
            width = 2,
            dash = "- -",
            joinstyle = "miter",
            capstyle = "butt"
          )
        tkaddtag(W, "TK_TMP_MOVE", "withtag", roi)
      }
    })

    RefreshLinesPosition <- function(W, ptsX) {
      xNewPos <- usr2tk(ptsX)
      if (length(xNewPos) > 0) {
        tkdelete(W, "TK_TMP")
        yLimits <-
          (1 - getVariable("plt")[3:4]) * getVariable("tkRplotRcanvasHeight")
        for (i in 1:length(xNewPos)) {
          x <- xNewPos[i]
          roi <-
            tkcreate(
              W,
              "line",
              c(x, yLimits[1], x, yLimits[2]),
              fill = "#ff0000",
              width = 2,
              dash = "- -"
            )
          tkaddtag(W, "TK_TMP", "withtag", roi)
        }
      }
    }

    tkbind(parent, "<Enter>", function() {
      width <- as.numeric(.Tcl(paste("winfo width", parent$env$canvas)))
      height <-
        as.numeric(.Tcl(paste("winfo height", parent$env$canvas)))
      widthPrevious <- parent$env$width
      heightPrevious <- parent$env$height
      if (abs(width - widthPrevious) > 0 |
        abs(height - heightPrevious) >
          0) {
        parent$env$height <- height
        parent$env$width <- width
        tkpack.forget(parent$env$canvas)
        tkRreplot(parent)
        tkpack(parent$env$canvas,
          expand = 1,
          fill = "both"
        )
      }
      setVariable("tkRplotRcanvasWidth", width)
      setVariable("tkRplotRcanvasHeight", height)
      setVariable("usr", parent$env$usr)
      setVariable("plt", parent$env$plt)
      parent$env$canvas$coef <- getCoef()
      RefreshLinesPosition(parent$env$canvas, ptsX)
    })

    tkbind(parent, "<FocusIn>", function() {
      width <-
        as.numeric(.Tcl(paste("winfo width", parent$env$canvas)))
      height <-
        as.numeric(.Tcl(paste("winfo height", parent$env$canvas)))
      widthPrevious <- parent$env$width
      heightPrevious <- parent$env$height
      if ((abs(width - widthPrevious) > 0) |
        (abs(height - heightPrevious) > 0)) {
        parent$env$height <- height
        parent$env$width <- width
        tkpack.forget(parent$env$canvas)
        tkRreplot(parent)
        tkpack(parent$env$canvas,
          expand = 1,
          fill = "both"
        )
      }
      setVariable("tkRplotRcanvasWidth", width)
      setVariable("tkRplotRcanvasHeight", height)
      setVariable("usr", parent$env$usr)
      setVariable("plt", parent$env$plt)
      parent$env$canvas$coef <- getCoef()
      RefreshLinesPosition(parent$env$canvas, ptsX)
    })

    tkbind(parent, "<Expose>", function() {
      width <-
        as.numeric(.Tcl(paste("winfo width", parent$env$canvas)))
      height <-
        as.numeric(.Tcl(paste("winfo height", parent$env$canvas)))
      widthPrevious <- parent$env$width
      heightPrevious <- parent$env$height
      if ((abs(width - widthPrevious) > 0) |
        (abs(height - heightPrevious) > 0)) {
        parent$env$height <- height
        parent$env$width <- width
        tkpack.forget(parent$env$canvas)
        tkRreplot(parent)
        tkpack(parent$env$canvas,
          expand = 1,
          fill = "both"
        )
      }
      setVariable("tkRplotRcanvasWidth", width)
      setVariable("tkRplotRcanvasHeight", height)
      setVariable("usr", parent$env$usr)
      setVariable("plt", parent$env$plt)
      parent$env$canvas$coef <- getCoef()
      RefreshLinesPosition(parent$env$canvas, ptsX)
    })

    tkwm.protocol(parent, "WM_DELETE_WINDOW", Close)

    tkbind(parent, "<q>", Close)
    tkbind(parent, "<Control-KeyPress-z>", Clear)
    tkbind(parent, "<Escape>", Close)
    tkwm.minsize(parent, tclvalue(tkwinfo("reqheight", parent)), tclvalue(tkwinfo("reqheight", parent$env$fL)))

    on.exit({
      if (tclvalue(done) == 1) {
        return(x)
      } else {
        return(xOriginal)
      }
      if (tclvalue(done) == "0") {
        tkdestroy(parent)
        tclvalue(done) <- 1
      }
    })
    tkwm.deiconify(parent)
    tkwait.variable(done)
  }
})
