# Window-independent logic for the desktop app: site names, progress tracking, the action
# registry and the validated runner. No Tk code here, so everything can be tested without a window.

# Site name suggested for a file: the file name without extension, cut at the first underscore.
# NBC_2026.xlsx gives NBC. Vectorised.
site_name_from_file <- function(path) {
  stem <- sub("\\.[^.]*$", "", basename(path))
  sub("_.*$", "", stem)
}

# Maps the pipeline's progress messages to a step. `sites` are the site names in the order the
# pipeline reads them. The returned function takes one message and gives
# list(step, total, label), or NULL when the message is not a progress message.
# A run has length(sites) + 4 steps: one per site, then QC, profiling, figures and the report.
progress_tracker <- function(sites) {
  n <- length(sites)
  total <- n + 4L
  function(message) {
    msg <- trimws(message)
    tick <- function(step, label) list(step = step, total = total, label = label)
    reading <- paste0("Reading ", sites, ": ")
    hit <- which(startsWith(msg, reading))
    if (length(hit) > 0) {
      i <- hit[which.max(nchar(sites[hit]))]
      return(tick(i, paste0("Reading ", sites[i])))
    }
    if (msg == "Running QC rules") return(tick(n + 1L, "Checking the data quality"))
    if (msg == "Profiling") return(tick(n + 2L, "Summarising the data"))
    if (msg == "Drawing figures") return(tick(n + 3L, "Drawing the figures"))
    if (startsWith(msg, "Run folder:")) return(tick(total, "Writing the report"))
    NULL
  }
}

# Each entry is one button in the window. `run` is function(inputs, root, ...) returning the run
# folder; run = NULL shows the button disabled. To add the analysis step later, set its `run`.
gui_actions <- list(
  qc = list(
    label = "Run QC",
    run = function(inputs, root, ...) run_pipeline(inputs, root, ...)
  ),
  analysis = list(
    label = "Run analysis",
    run = NULL
  )
)

# Plain English problems with the chosen files and site names, or character(0) when all is well.
input_problems <- function(inputs) {
  if (length(inputs) == 0) return("Please add at least one data file.")
  sites <- names(inputs)
  if (is.null(sites)) sites <- rep("", length(inputs))
  sites <- trimws(sites)
  sites[is.na(sites)] <- ""
  problems <- character()
  missing <- !file.exists(inputs)
  if (any(missing)) {
    problems <- c(problems, paste0("This file cannot be found: ", basename(inputs[missing][1]),
                                   ". Please remove it from the list or add it again."))
  }
  if (any(!nzchar(sites))) {
    problems <- c(problems, "Every file needs a site name. Please fill in the empty site name.")
  } else if (anyDuplicated(sites) > 0) {
    problems <- c(problems, paste0("Each site name must be different. The name '",
                                   sites[duplicated(sites)][1], "' is used more than once."))
  }
  problems
}

# Checks the inputs, runs `action` (an entry of gui_actions) and returns the run folder.
# `inputs` is a character vector of file paths named by site. Every progress message of the
# pipeline is turned into on_progress(step, total, label) and hidden from the console.
# Further arguments go to the action (for example out_subfolder).
run_action <- function(action, inputs, root, on_progress = function(step, total, label) NULL,
                       ...) {
  if (!is.function(action$run)) {
    stop("'", action$label, "' is not available yet.", call. = FALSE)
  }
  problems <- input_problems(inputs)
  if (length(problems) > 0) stop(problems[1], call. = FALSE)
  inputs <- stats::setNames(as.character(inputs), trimws(names(inputs)))
  tracker <- progress_tracker(names(inputs))
  withCallingHandlers(
    action$run(inputs, root, ...),
    message = function(m) {
      tick <- tracker(conditionMessage(m))
      if (!is.null(tick)) on_progress(tick$step, tick$total, tick$label)
      invokeRestart("muffleMessage")
    }
  )
}
