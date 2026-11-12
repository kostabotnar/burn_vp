# The setup dialog (Tk, shipped with base R). One window for the whole first-time setup: an
# explanation, then the progress, then the result. Everything that touches Tk lives in this file;
# the texts and the line parsing are in setup_dialog_logic.R. Sourcing this file creates no
# window, and the dialog writes nothing.
# nolint start: object_usage_linter. Functions from setup_dialog_logic.R are sourced into the
# same environment.

# TRUE when the dialog can be used: tcltk loads and a Tk window can be created.
setup_dialog_available <- function() {
  if (!isTRUE(unname(capabilities("tcltk")))) return(FALSE)
  tryCatch({
    top <- tcltk::tktoplevel()
    tcltk::tkdestroy(top)
    TRUE
  }, error = function(e) FALSE, warning = function(w) FALSE)
}

# Builds the window in its first (explanation) view and returns the widgets and the functions
# that move it through the views. `choice` is a Tcl variable that is set when a button is
# pressed: "start", "cancel", "open" or "close".
setup_dialog_build <- function() {
  txt <- setup_dialog_text()
  top <- tcltk::tktoplevel()
  tcltk::tkwm.title(top, txt$title)
  tcltk::tkwm.minsize(top, 600, 220)
  main <- tcltk::ttkframe(top, padding = 12)
  tcltk::tkgrid(main, sticky = "nsew")
  tcltk::tkgrid.columnconfigure(top, 0, weight = 1)
  tcltk::tkgrid.rowconfigure(top, 0, weight = 1)
  tcltk::tkgrid.columnconfigure(main, 0, weight = 1)
  tcltk::tkgrid.rowconfigure(main, 6, weight = 1)

  state <- new.env()
  state$phase <- "intro"
  state$lines <- character()
  choice <- tcltk::tclVar("")
  heading_var <- tcltk::tclVar("First-time setup")
  body_var <- tcltk::tclVar(txt$intro)
  status_var <- tcltk::tclVar(txt$starting)
  set_var <- function(var, text) tcltk::tclvalue(var) <- text

  heading <- tcltk::ttklabel(main, textvariable = heading_var, font = "TkHeadingFont")
  body <- tcltk::ttklabel(main, textvariable = body_var, wraplength = 570, justify = "left")
  status <- tcltk::ttklabel(main, textvariable = status_var)
  progress <- tcltk::ttkprogressbar(main, orient = "horizontal", mode = "determinate",
                                    maximum = 5, value = 0)
  busy <- tcltk::ttkprogressbar(main, orient = "horizontal", mode = "indeterminate")
  wait <- tcltk::ttklabel(main, text = txt$long_step)
  log_frame <- tcltk::ttkframe(main)
  log_text <- tcltk::tktext(log_frame, height = 14, width = 80, wrap = "word",
                            state = "disabled", font = "TkFixedFont")
  scroll <- tcltk::ttkscrollbar(log_frame, orient = "vertical",
                                command = function(...) tcltk::tkyview(log_text, ...))
  tcltk::tkconfigure(log_text, yscrollcommand = function(...) tcltk::tkset(scroll, ...))
  tcltk::tkgrid(log_text, scroll, sticky = "nsew")
  tcltk::tkgrid.columnconfigure(log_frame, 0, weight = 1)
  tcltk::tkgrid.rowconfigure(log_frame, 0, weight = 1)
  button_frame <- tcltk::ttkframe(main)
  primary <- tcltk::ttkbutton(button_frame, text = txt$start)
  secondary <- tcltk::ttkbutton(button_frame, text = txt$cancel)
  tcltk::tkgrid(primary, secondary, padx = c(0, 6))

  tcltk::tkgrid(heading, row = 0, sticky = "w", pady = c(0, 8))
  tcltk::tkgrid(body, row = 1, sticky = "w", pady = c(0, 8))
  tcltk::tkgrid(button_frame, row = 7, sticky = "e", pady = c(8, 0))

  show <- function(widget, row, ...) tcltk::tkgrid(widget, row = row, sticky = "ew", ...)
  hide <- function(...) for (w in list(...)) tcltk::tkgrid.remove(w)
  press <- function(value) function() set_var(choice, value)
  set_buttons <- function(first, second, first_value, second_value, enabled = TRUE) {
    on_state <- if (enabled) "normal" else "disabled"
    tcltk::tkconfigure(primary, text = first, command = press(first_value), state = on_state)
    if (is.null(second)) {
      tcltk::tkgrid.remove(secondary)
    } else {
      tcltk::tkgrid(secondary, row = 0, column = 1, padx = c(0, 6))
      tcltk::tkconfigure(secondary, text = second, command = press(second_value),
                         state = on_state)
    }
  }
  set_buttons(txt$start, txt$cancel, "start", "cancel")

  show_progress <- function() {
    state$phase <- "running"
    hide(body)
    set_var(heading_var, "Setting up")
    show(status, 2, pady = c(0, 4))
    show(progress, 3, pady = c(0, 4))
    tcltk::tkgrid(log_frame, row = 6, sticky = "nsew", pady = c(4, 0))
    set_buttons(txt$start, txt$cancel, "start", "cancel", enabled = FALSE)
    tcltk::tcl("update")
  }

  # Repaints after every call. "update" (not "update idletasks") is used so the window also
  # redraws after being covered or moved; the buttons are disabled, so a click cannot restart
  # anything, and a close request is ignored while setup runs.
  on_line <- function(line) {
    state$lines <- c(state$lines, line)
    tcltk::tkconfigure(log_text, state = "normal")
    tcltk::tkinsert(log_text, "end", paste0(line, "\n"))
    tcltk::tksee(log_text, "end")
    tcltk::tkconfigure(log_text, state = "disabled")
    step <- setup_progress_step(line)
    if (!is.null(step)) {
      tcltk::tkconfigure(progress, maximum = step$total, value = step$step - 1)
      set_var(status_var, sprintf("Step %d of %d: %s", step$step, step$total, step$label))
      if (step$long) {
        show(wait, 4, pady = c(0, 4))
        tcltk::tcl(busy, "start", 40)
        show(busy, 5, pady = c(0, 4))
      } else {
        tcltk::tcl(busy, "stop")
        hide(busy, wait)
      }
    }
    tcltk::tcl("update")
    invisible(step)
  }

  show_result <- function(outcome) {
    state$phase <- "result"
    tcltk::tcl(busy, "stop")
    hide(busy, wait, status)
    view <- setup_result_view(outcome, state$lines)
    if (identical(outcome, "complete")) {
      tcltk::tkconfigure(progress, value = as.numeric(tcltk::tkcget(progress, "-maximum")))
      set_buttons(txt$open, txt$close, "open", "close")
    } else {
      set_buttons(txt$close, NULL, "close", NULL)
    }
    set_var(heading_var, view$headline)
    set_var(body_var, paste(view$details, collapse = "\n"))
    show(body, 1, pady = c(0, 8))
    tcltk::tcl("update")
  }

  # The close button of the window: Cancel at first, Close at the end, ignored in between because
  # Tk handles events only when setup calls update.
  tcltk::tkwm.protocol(top, "WM_DELETE_WINDOW", function() {
    if (state$phase == "intro") set_var(choice, "cancel")
    if (state$phase == "result") set_var(choice, "close")
  })
  tcltk::tkraise(top)

  list(top = top, choice = choice, heading_var = heading_var, body_var = body_var,
       status_var = status_var, progress = progress, busy = busy, wait = wait,
       log_text = log_text, primary = primary, secondary = secondary,
       show_progress = show_progress, on_line = on_line, show_result = show_result)
}

# Blocks until a button of the window sets its choice. Tests replace it with a function that
# presses a button instead of waiting.
setup_dialog_wait <- function(win) tcltk::tkwait.variable(win$choice)

# Runs the dialog from the first view to the last. `run_setup(root, on_line)` runs the setup,
# calls `on_line` with every line it prints, and returns "complete" or "failed". Returns
# "cancel" (the user declined), "failed" (setup failed and the user closed the window), "open" or
# "close" (setup completed and the user chose that button).
setup_dialog_run <- function(root, run_setup, wait = setup_dialog_wait) {
  win <- setup_dialog_build()
  on.exit(tryCatch(tcltk::tkdestroy(win$top), error = function(e) NULL), add = TRUE)
  wait(win)
  if (!identical(tcltk::tclvalue(win$choice), "start")) return("cancel")

  win$show_progress()
  outcome <- tryCatch(run_setup(root, win$on_line), error = function(e) {
    win$on_line(paste0("What went wrong: An unexpected problem occurred: ", conditionMessage(e)))
    "failed"
  })
  outcome <- if (identical(outcome, "complete")) "complete" else "failed"
  win$show_result(outcome)
  tcltk::tclvalue(win$choice) <- ""
  wait(win)
  if (outcome == "failed") "failed" else tcltk::tclvalue(win$choice)
}
# nolint end
