# Logic for the setup dialog (R/setup_dialog.R) that needs no window. Base R only: it runs before
# the project packages are installed. It reads text and writes nothing.

# The lines scripts/setup_environment.R prints, as the dialog needs them. A step line looks like
# "Step 3 of 5: Preparing the package library". Returns list(step, total, label, long) or NULL
# for any other line. `long` is TRUE for the package installation, the one step that can take
# minutes without printing anything (it is FALSE when that step is skipped).
setup_progress_step <- function(line) {
  hit <- regmatches(line, regexec("^Step ([0-9]+) of ([0-9]+): (.+)$", line))[[1]]
  if (length(hit) != 4) return(NULL)
  step <- as.integer(hit[2])
  list(step = step, total = as.integer(hit[3]), label = hit[4],
       long = step == 4L && !grepl("(skipped", hit[4], fixed = TRUE))
}

# Fixed texts of the dialog, in plain English.
setup_dialog_text <- function() {
  list(
    title = "burn_vp: first-time setup",
    intro = paste(
      "The tool needs a one-time setup before it can start.",
      "",
      "The setup installs the programs the tool uses and checks that they work.",
      "It takes several minutes and needs an internet connection.",
      "Please keep R open while it runs.",
      sep = "\n"
    ),
    start = "Start setup",
    cancel = "Cancel",
    starting = "Starting the setup...",
    long_step = "Installing packages. This can take several minutes. Please wait.",
    open = "Open the program",
    close = "Close"
  )
}

# What the result view shows. `outcome` is "complete" or "failed"; `lines` are the lines setup
# printed. Returns list(headline, details) where details is a character vector, one line each.
setup_result_view <- function(outcome, lines) {
  field <- function(prefix) {
    hit <- lines[startsWith(lines, prefix)]
    if (length(hit) == 0) NA_character_ else substring(hit[length(hit)], nchar(prefix) + 1)
  }
  log_line <- field("Log file: ")
  log_text <- if (is.na(log_line)) character() else paste0("Log file: ", log_line)
  if (identical(outcome, "complete")) {
    return(list(headline = "Setup complete.",
                details = c("You can now use the tool. You do not need to run the setup again.",
                            log_text)))
  }
  reason <- field("What went wrong: ")
  action <- field("What to do: ")
  if (is.na(reason)) reason <- "The setup stopped before it could report a reason."
  if (is.na(action)) {
    action <- paste("Send the text in the window above, and the log file if there is one,",
                    "to the project team.")
  }
  list(headline = "Setup failed.",
       details = c(paste0("What went wrong: ", reason), paste0("What to do: ", action), log_text))
}
