# Logic behind start_burn_vp.R, the one file clinicians source from Rgui. Base R only: it runs
# before the project packages are installed. It reads and decides; it writes nothing itself. Setup
# (scripts/setup_environment.R) writes the package library and the setup log, and the window
# writes under output/.
# nolint start: object_usage_linter. Functions from project_paths.R, setup.R and gui_window.R are
# sourced into the same environment.

# The package library folder renv uses for this R version and platform: renv/library/<os>/R-x.y/
# <platform>. renv adds the "windows" part automatically for R 4.4 and later (renv/activate.R).
start_library_path <- function(root, r_version = getRversion(), platform = R.version$platform,
                               os = "windows") {
  file.path(root, "renv", "library", os, paste0("R-", format(r_version[1, 1:2])), platform)
}

# Package records from renv.lock as a named list with Version, Depends, Imports and LinkingTo,
# the shape setup_dependency_closure() expects. renv.lock is JSON written by renv with one field
# per line, which is why plain line matching is enough here and no JSON package is needed.
start_read_lock_packages <- function(root) {
  path <- file.path(root, "renv.lock")
  if (!file.exists(path)) return(list())
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  packages <- list()
  in_packages <- FALSE
  current <- NULL
  field <- NULL
  for (line in lines) {
    if (!in_packages) {
      in_packages <- grepl('^\\s*"Packages"\\s*:\\s*\\{\\s*$', line)
      next
    }
    if (is.null(current)) {
      hit <- regmatches(line, regexec('^\\s*"([^"]+)"\\s*:\\s*\\{\\s*$', line))[[1]]
      if (length(hit) == 2) {
        current <- hit[2]
        packages[[current]] <- list()
      }
      next
    }
    if (!is.null(field)) {
      if (grepl("^\\s*\\]", line)) {
        field <- NULL
      } else {
        item <- regmatches(line, regexec('^\\s*"(.*)"\\s*,?\\s*$', line))[[1]]
        if (length(item) == 2) {
          packages[[current]][[field]] <- c(packages[[current]][[field]], item[2])
        }
      }
      next
    }
    version <- regmatches(line, regexec('^\\s*"Version"\\s*:\\s*"([^"]*)"', line))[[1]]
    array_start <- regmatches(
      line, regexec('^\\s*"(Depends|Imports|LinkingTo)"\\s*:\\s*\\[\\s*$', line)
    )[[1]]
    if (length(version) == 2) {
      packages[[current]]$Version <- version[2]
    } else if (length(array_start) == 2) {
      field <- array_start[2]
    } else if (grepl("^\\s*\\},?\\s*$", line)) {
      current <- NULL
    }
  }
  packages
}

# TRUE when the project library lacks a package the tool needs or has another version than
# renv.lock. Uses the same checks as setup, without activating renv and without writing. renv
# itself only has to be present, because its bootstrap installs the locked version.
start_setup_needed <- function(root, library = start_library_path(root)) {
  if (!dir.exists(library)) return(TRUE)
  lock <- start_read_lock_packages(root)
  if (!all(project_packages %in% names(lock))) return(TRUE)
  needed <- setup_dependency_closure(lock, project_packages)
  locked <- setup_lock_versions(lock, needed)
  installed <- setup_installed_versions(needed, library)
  if (length(setup_packages_to_restore(locked, installed)) > 0) return(TRUE)
  is.na(setup_installed_versions("renv", library)[["renv"]])
}

start_ask <- function(question) {
  identical(utils::winDialog("yesno", question), "YES")
}

# Runs scripts/setup_environment.R and returns its outcome, "complete" or "failed".
start_run_setup <- function(root) {
  source(file.path(root, "scripts", "setup_environment.R"))$value
}

# Runs `run_setup(root)` with the option burn_vp.setup_progress set to `on_line`, so that
# scripts/setup_environment.R passes every line it prints to the setup dialog. The previous value
# of the option comes back when setup ends, also after an error.
start_run_with_progress <- function(root, on_line, run_setup) {
  old <- options(burn_vp.setup_progress = on_line)
  on.exit(options(old), add = TRUE)
  run_setup(root)
}

# The whole start. Returns invisibly which branch ran: "window" (packages were present),
# "setup_then_window" (setup succeeded and the user chose to open the program),
# "setup_closed" (setup succeeded and the user chose Close) or "setup_failed" (setup failed, so
# no window). Stops with a plain message when the project folder or R version is wrong, or when
# setup is needed and was not accepted. Never calls quit().
# `dialog(root, run_setup)` is the setup window (see setup_dialog_run). When Tk is not available
# (`dialog_available()` is FALSE) the question is asked with `ask` and setup output stays in the
# console.
start_burn_vp <- function(script_dir, is_interactive = interactive(),
                          is_windows = .Platform$OS.type == "windows", chooser = NULL,
                          ask = start_ask, run_setup = start_run_setup, open_window = gui_main,
                          dialog = setup_dialog_run, dialog_available = setup_dialog_available,
                          check_r_version = setup_check_r_version,
                          setup_needed = start_setup_needed, r_version = getRversion(),
                          say = function(...) cat(..., sep = "")) {
  root <- setup_find_root(script_dir, is_interactive, is_windows, chooser)
  say("Project folder: ", root, "\n")

  locked <- setup_lock_r_version(root)
  version_check <- check_r_version(locked)
  if (identical(version_check$status, "fail")) {
    needed <- if (is.na(locked)) "another version" else sub("^([0-9]+\\.[0-9]+).*$", "\\1", locked)
    stop("This tool needs R ", needed, ", but this R is version ", as.character(r_version), ". ",
         "Close R and open R ", needed, " from the Start menu, then try again. If R ", needed,
         " is not on this computer, please contact the project team. Nothing was changed.",
         call. = FALSE)
  }
  if (version_check$status %in% c("warning", "note")) say(version_check$message, "\n")

  branch <- "window"
  if (isTRUE(setup_needed(root))) {
    can_ask <- is_interactive && is_windows
    use_dialog <- can_ask && isTRUE(dialog_available())
    answer <- if (can_ask && !use_dialog) "answer Yes when asked." else "click Start setup."
    rerun <- paste("To run it, source start_burn_vp.R again in R (File > Source R code...) and",
                   answer)
    if (!can_ask) {
      stop("The first-time setup has not been run on this computer. It must run first. ", rerun,
           call. = FALSE)
    }
    say("The first-time setup has not been run on this computer.\n")
    if (use_dialog) {
      say("A setup window opens. Its Start setup button begins the setup.\n")
      choice <- dialog(root, function(root, on_line) {
        start_run_with_progress(root, on_line, run_setup)
      })
      if (identical(choice, "cancel")) {
        stop("The setup was not run, and the tool cannot start without it. ", rerun, call. = FALSE)
      }
      if (identical(choice, "close")) {
        say("The setup is complete. To open the program, source start_burn_vp.R again.\n")
        return(invisible("setup_closed"))
      }
      if (!identical(choice, "open")) {
        say("The setup did not finish, so the tool was not started. ",
            "Please read the message in the setup window and contact the project team.\n")
        return(invisible("setup_failed"))
      }
    } else {
      yes <- isTRUE(ask(paste(
        "The tool needs a one-time setup before it can start.",
        "It installs the programs the tool uses and checks that they work.",
        "It takes several minutes and needs an internet connection.",
        "\n\nRun the setup now?"
      )))
      if (!yes) {
        stop("The setup was not run, and the tool cannot start without it. ", rerun, call. = FALSE)
      }
      say("Running the setup. This takes several minutes. Please wait.\n")
      outcome <- run_setup(root)
      if (!identical(outcome, "complete")) {
        say("The setup did not finish, so the tool was not started. ",
            "Please read the message above and contact the project team.\n")
        return(invisible("setup_failed"))
      }
    }
    branch <- "setup_then_window"
  }

  say("Opening the program window. Close the window to return to R.\n")
  open_window(root)
  say("The program window was closed.\n")
  invisible(branch)
}
# nolint end
