# The desktop window (Tk, shipped with base R). Everything that touches Tk lives inside
# gui_build_window(); sourcing this file creates no window. The logic that does not need a window
# is in gui.R.

# Builds the window and returns the toplevel plus the widgets and helpers a test needs.
# The window is not modal: call tcltk::tkwait.window(result$top) to keep a script alive until it
# is closed.
# nolint start: object_usage_linter. Functions from gui.R are not visible here.
gui_build_window <- function(root) {
  top <- tcltk::tktoplevel()
  tcltk::tkwm.title(top, "burn_vp: data quality check")
  tcltk::tkwm.minsize(top, 560, 320)
  main <- tcltk::ttkframe(top, padding = 12)
  tcltk::tkgrid(main, sticky = "nsew")
  tcltk::tkgrid.columnconfigure(top, 0, weight = 1)
  tcltk::tkgrid.rowconfigure(top, 0, weight = 1)
  tcltk::tkgrid.columnconfigure(main, 0, weight = 1)

  state <- new.env()
  state$rows <- list() # one entry per file, named by a normalised path
  state$next_row <- 0L
  state$total <- 1L
  state$running <- FALSE
  state$report <- NULL

  tcltk::tkgrid(
    tcltk::ttklabel(main, text = paste(
      "1. Add your Excel data files.  2. Check or edit the site name next to each file.",
      "3. Press Run QC."
    ), wraplength = 520),
    sticky = "w", pady = c(0, 8)
  )

  # One grid row per file: a tick box (for Remove), the file name and an editable site name.
  list_frame <- tcltk::ttklabelframe(main, text = "Data files", padding = 8)
  tcltk::tkgrid(list_frame, sticky = "nsew")
  tcltk::tkgrid.columnconfigure(list_frame, 1, weight = 1)
  tcltk::tkgrid(tcltk::ttklabel(list_frame, text = "Remove?"),
                tcltk::ttklabel(list_frame, text = "File"),
                tcltk::ttklabel(list_frame, text = "Site name"), sticky = "w", padx = 4)
  hint <- tcltk::ttklabel(list_frame, text = "No files added yet. Press 'Add files...'.")
  tcltk::tkgrid(hint, row = 1, column = 0, columnspan = 3, sticky = "w", padx = 4, pady = 4)

  status_var <- tcltk::tclVar("Ready.")
  set_status <- function(text) tcltk::tclvalue(status_var) <- text

  add_files <- function(paths) {
    paths <- paths[nzchar(paths)]
    added <- 0L
    for (path in paths) {
      path <- normalizePath(path, winslash = "/", mustWork = FALSE)
      key <- tolower(path)
      if (key %in% names(state$rows)) next
      state$next_row <- state$next_row + 1L
      r <- state$next_row + 1L
      check <- tcltk::tclVar("0")
      site <- tcltk::tclVar(site_name_from_file(path))
      widgets <- list(
        tcltk::ttkcheckbutton(list_frame, variable = check),
        tcltk::ttklabel(list_frame, text = basename(path)),
        tcltk::ttkentry(list_frame, textvariable = site, width = 18)
      )
      tcltk::tkgrid(widgets[[1]], widgets[[2]], widgets[[3]], row = r, sticky = "w", padx = 4,
                    pady = 2)
      state$rows[[key]] <- list(path = path, check = check, site = site, widgets = widgets)
      added <- added + 1L
    }
    if (length(state$rows) > 0) tcltk::tkgrid.remove(hint)
    added
  }

  remove_checked <- function() {
    ticked <- vapply(state$rows, function(x) tcltk::tclvalue(x$check) == "1", logical(1))
    for (row in state$rows[ticked]) {
      for (w in row$widgets) tcltk::tkdestroy(w)
    }
    state$rows <- state$rows[!ticked]
    if (length(state$rows) == 0) {
      tcltk::tkgrid(hint, row = 1, column = 0, columnspan = 3, sticky = "w", padx = 4, pady = 4)
    }
    invisible(sum(ticked))
  }

  get_inputs <- function() {
    paths <- vapply(state$rows, function(x) x$path, character(1))
    sites <- vapply(state$rows, function(x) tcltk::tclvalue(x$site), character(1))
    stats::setNames(unname(paths), unname(sites))
  }

  choose_files <- function() {
    start <- file.path(root, "data", "raw")
    args <- list(parent = top, multiple = TRUE, title = "Select the site data files",
                 filetypes = "{{Excel files} {.xlsx}}")
    if (dir.exists(start)) args$initialdir <- normalizePath(start, winslash = "/")
    chosen <- do.call(tcltk::tkgetOpenFile, args)
    add_files(as.character(chosen))
  }

  file_buttons <- tcltk::ttkframe(main)
  add_button <- tcltk::ttkbutton(file_buttons, text = "Add files...", command = choose_files)
  remove_button <- tcltk::ttkbutton(file_buttons, text = "Remove ticked files",
                                    command = remove_checked)
  tcltk::tkgrid(add_button, remove_button, padx = c(0, 6))
  tcltk::tkgrid(file_buttons, sticky = "w", pady = 6)

  progress <- tcltk::ttkprogressbar(main, orient = "horizontal", mode = "determinate",
                                    maximum = 1, value = 0)
  status_label <- tcltk::ttklabel(main, textvariable = status_var)
  open_button <- tcltk::ttkbutton(main, text = "Open report", state = "disabled")

  buttons <- list()
  set_busy <- function(busy) {
    state$running <- busy
    on_state <- if (busy) "disabled" else "normal"
    tcltk::tkconfigure(add_button, state = on_state)
    tcltk::tkconfigure(remove_button, state = on_state)
    for (name in names(buttons)) {
      runnable <- is.function(gui_actions[[name]]$run)
      tcltk::tkconfigure(buttons[[name]], state = if (!busy && runnable) "normal" else "disabled")
    }
    has_report <- !busy && !is.null(state$report) && file.exists(state$report)
    tcltk::tkconfigure(open_button, state = if (has_report) "normal" else "disabled")
  }

  on_progress <- function(step, total, label) {
    state$total <- total
    tcltk::tkconfigure(progress, maximum = total, value = step)
    set_status(sprintf("%s (step %d of %d)", label, step, total))
    tcltk::tcl("update")
  }

  run_clicked <- function(name) {
    state$report <- NULL
    set_busy(TRUE)
    tcltk::tkconfigure(progress, maximum = 1, value = 0)
    set_status("Starting...")
    tcltk::tcl("update")
    tryCatch({
      run_dir <- run_action(gui_actions[[name]], get_inputs(), root, on_progress)
      state$report <- file.path(run_dir, "report.html")
      tcltk::tkconfigure(progress, maximum = state$total, value = state$total)
      set_status("Finished")
      tcltk::tkmessageBox(parent = top, title = "Finished", icon = "info", type = "ok",
                          message = paste0("The run finished. The results are in this folder:\n",
                                           run_dir))
    }, error = function(e) {
      set_status("Failed")
      tcltk::tkmessageBox(parent = top, title = "Something went wrong", icon = "error",
                          type = "ok", message = conditionMessage(e))
    }, finally = set_busy(FALSE))
  }

  open_report <- function() {
    tryCatch(utils::browseURL(state$report), error = function(e) {
      tcltk::tkmessageBox(parent = top, title = "Cannot open the report", icon = "error",
                          type = "ok", message = paste0("The report could not be opened: ",
                                                        conditionMessage(e)))
    })
  }
  tcltk::tkconfigure(open_button, command = open_report)

  action_frame <- tcltk::ttkframe(main)
  for (name in names(gui_actions)) {
    action <- gui_actions[[name]]
    runnable <- is.function(action$run)
    buttons[[name]] <- local({
      this <- name
      tcltk::ttkbutton(
        action_frame,
        text = if (runnable) action$label else paste0(action$label, " (not available yet)"),
        state = if (runnable) "normal" else "disabled",
        command = function() run_clicked(this)
      )
    })
    tcltk::tkgrid(buttons[[name]], row = 0, column = length(buttons) - 1L, padx = c(0, 6))
  }
  tcltk::tkgrid(action_frame, sticky = "w", pady = c(6, 6))
  tcltk::tkgrid(progress, sticky = "ew", pady = c(6, 2))
  tcltk::tkgrid(status_label, sticky = "w")
  tcltk::tkgrid(open_button, sticky = "w", pady = c(8, 0))

  # Tk only handles events while R calls update, so a close during a run is ignored: the
  # window would otherwise disappear under the running code.
  tcltk::tkwm.protocol(top, "WM_DELETE_WINDOW", function() {
    if (!isTRUE(state$running)) tcltk::tkdestroy(top)
  })

  list(top = top, add_button = add_button, remove_button = remove_button, buttons = buttons,
       progress = progress, status_var = status_var, open_button = open_button,
       add_files = add_files, remove_checked = remove_checked, get_inputs = get_inputs)
}
# nolint end
