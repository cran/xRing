#' @import tcltk

AsNumeric <- function(x) {
  suppressWarnings(as.numeric(x))
}

AsNumericNaN <- function(x) {
  x <- AsNumeric(x)
  if (is.na(x)) x <- ""
  x
}

modalDialog <- function(title = " ",
                        question = "",
                        entryInit = "",
                        entryWidth = 20,
                        returnValOnCancel = "") {
  dlg <- tktoplevel(borderwidth = 0)
  tcl("wm", "attributes", dlg, topmost = TRUE)
  tkwm.iconify(dlg)
  tkwm.title(dlg, title)
  tkgrab.set(dlg)

  frm0Black <- tkframe(dlg, borderwidth = 1, background = "black")
  tkpack(frm0Black, anchor = "center", expand = "y")
  frm0 <- tkframe(frm0Black, borderwidth = 0)
  tkpack(frm0, anchor = "center", expand = "y")
  row1 <- tkframe(frm0)
  tkpack(row1)
  row2 <- tkframe(frm0)
  tkpack(row2)

  textEntryVarTcl <- tclVar(paste(entryInit))
  textEntryWidget <- tkentry(
    row1,
    width = paste(entryWidth),
    textvariable = textEntryVarTcl,
    background = "white"
  )

  tkpack(
    tklabel(row1, text = paste0("  ", question)),
    textEntryWidget,
    tklabel(row1, text = "  "),
    side = "left",
    pady = 10,
    padx = 1
  )


  ReturnVal <- returnValOnCancel

  onOK <- function() {
    ReturnVal <<- tclvalue(textEntryVarTcl)
    tkgrab.release(dlg)
    tkdestroy(dlg)
  }

  onCancel <- function() {
    ReturnVal <<- returnValOnCancel
    tkgrab.release(dlg)
    tkdestroy(dlg)
  }

  OK.but <- tkbutton(row2,
    text = "Ok",
    width = 6,
    command = onOK
  )
  Cancel.but <-
    tkbutton(row2,
      text = "Cancel",
      width = 6,
      command = onCancel
    )
  tkpack(Cancel.but,
    OK.but,
    side = "left",
    padx = 2,
    pady = 4
  )
  tkbind(dlg, "<Destroy>", function() {
    tkgrab.release(dlg)
  })
  tkbind(textEntryWidget, "<Return>", onOK)
  tkbind(textEntryWidget, "<KP_Enter>", onOK)
  tkwm.deiconify(dlg)
  tkfocus(dlg)

  tkwait.window(dlg)
  ReturnVal
}
