# Opens the desktop window for the data quality check: choose site files, set site names, press
# Run QC. Intended for clinicians who do not use the command line. Most users start the tool with
# start_burn_vp.R in the project folder, which also runs the one-time setup when needed.
#
# Usage (from any working directory; the project's renv library is activated automatically.
# If it reports missing packages, run the setup first):
#   Rscript --no-init-file scripts/run_gui.R
#   The script keeps running until the window is closed.
#   Or in Rgui: File > Source R code..., select this file. The console returns when the window
#   is closed.
#
# Reads:  the xlsx files chosen in the window, metadata/codebook.csv, metadata/qc_ranges.csv,
#         PROJECT_VERSION and the R/ functions.
# Writes: output/runs/<YYYYmmdd_HHMMSS>/ with qc/ (CSV tables), profile/ (CSV tables),
#         figures/ (PNG) and report.html. Writes only when the project folder is confirmed by
#         PROJECT_VERSION (Project: burn_vp). Nothing is written otherwise.

local({
  start <- NA_character_
  for (frame in rev(sys.frames())) {
    candidate <- get0("ofile", envir = frame, inherits = FALSE)
    if (is.character(candidate) && length(candidate) == 1 && !is.na(candidate) &&
          file.exists(candidate)) {
      start <- candidate
      break
    }
  }
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (is.na(start) && length(file_arg) > 0) {
    start <- sub("^--file=", "", file_arg[1])
    if (!file.exists(start)) start <- gsub("~+~", " ", start, fixed = TRUE)
  }
  if (is.na(start)) {
    stop("Run this file with Rscript, or source it in R, so its location is known.", call. = FALSE)
  }
  script_dir <- dirname(normalizePath(start, winslash = "/"))
  project_root <- dirname(script_dir)
  if (basename(script_dir) != "scripts") {
    stop("This script must sit in the scripts folder of the project.", call. = FALSE)
  }

  app <- new.env(parent = globalenv())
  for (f in list.files(file.path(project_root, "R"), pattern = "\\.R$", full.names = TRUE)) {
    source(f, local = app)
  }
  app$gui_main(project_root)
})
