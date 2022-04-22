# Adapted from https://gist.github.com/jbryer/3342915

varEntryDialog <- function(vars,
                           labels = vars,
                           valInitials = rep("", length(vars)),
                           fun = rep(list(as.numeric), length(vars)),
                           title = "",
                           width = 10,
                           prompt = NULL) {
  stopifnot(length(vars) == length(labels), length(labels) == length(fun))

  entries <- list()
  tclvars <- list()
  results <- list()
  done <- tclVar(0)
  win <- tktoplevel()
  tcl("wm", "attributes", win, topmost = TRUE)
  tkgrab.set(win)
  tkfocus(win)
  tkwm.title(win, title)
  tkbind(win, "<Destroy>", function() {
    tclvalue(done) <- 2
  })


  frm0 <- tkframe(win, borderwidth = 1, background = "black")
  tkpack(frm0, anchor = "center", expand = "y")
  frm1 <- tkframe(frm0)
  tkpack(frm1 <- tkframe(frm0))

  reset <- function(...) {
    for (i in seq_along(entries)) {
      tclvalue(tclvars[[i]]) <<- ""
    }
  }

  ok <- function(...) {
    for (i in seq_along(vars)) {
      tryCatch(
        {
          results[[vars[[i]]]] <<- fun[[i]](tclvalue(tclvars[[i]]))
          # tclvalue(done) <- 1
        },
        error = function(e) {
          tkmessageBox(message = geterrmessage())
        },
        finally = {

        }
      )
    }
    varWithProblems <- is.na(results)
    if (all(!varWithProblems)) {
      tclvalue(done) <- 1
    }
    varWithProblemsId <- which(varWithProblems)
    for (i in varWithProblemsId) {
      tclvalue(tclvars[[i]]) <<- ""
      tkconfigure(entries[[i]], background = "red")
    }
    varWithoutProblems <- which(!varWithProblems)
    for (i in varWithoutProblems) {
      tkconfigure(entries[[i]], background = "white")
    }
  }
  tkbind(win, "<Return>", ok)
  tkbind(win, "<KP_Enter>", ok)
  cancel <- function() {
    tclvalue(done) <- 2
  }

  if (!is.null(prompt)) {
    tkpack(tklabel(frm1, text = prompt),
      anchor = "center",
      expand = TRUE
    )
  }
  frm2 <- tkframe(frm1, borderwidth = 2)
  tkpack(frm2, side = "top")

  for (i in seq_along(vars)) {
    tclvars[[i]] <- tclVar(valInitials[i])
    entries[[i]] <-
      tkentry(
        frm2,
        textvariable = tclvars[[i]],
        width = width,
        background = "white",
        relief = "sunken"
      )
  }

  for (i in seq_along(vars)) {
    tkgrid(
      tklabel(
        frm2,
        text = paste0(labels[i], ":"),
        justify = "right",
        relief = "flat"
      ),
      entries[[i]],
      pady = 5,
      padx = 2,
      sticky = "ewns",
      columnspan = 2
    )
  }
  tt2 <- tkframe(frm1, borderwidth = 2)
  tkpack(tt2, side = "top")
  reset.but <-
    tkbutton(
      tt2,
      text = "Reset",
      command = reset,
      relief = "raised",
      width = 8
    )
  cancel.but <-
    tkbutton(
      tt2,
      text = "Cancel",
      command = cancel,
      relief = "groove",
      width = 8
    )
  ok.but <-
    tkbutton(
      tt2,
      text = "Ok",
      command = ok,
      relief = "groove",
      width = 8
    )

  tkpack(
    reset.but,
    cancel.but,
    ok.but,
    pady = 2,
    padx = 2,
    side = "left"
  )
  tkfocus(win)

  tkbind(win, "<Destroy>", function() {
    tkgrab.release(win)
    tclvalue(done) <- 2
  })

  tkwait.variable(done)

  if (tclvalue(done) != 1) {
    results <- NULL
  }

  try(tkdestroy(win), silent = TRUE)
  results
}
