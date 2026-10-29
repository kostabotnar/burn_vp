# Opens the desktop window for the data quality check: choose site files, set site names, press
# Run QC. Intended for clinicians who do not use the command line.
#
# Usage (from any working directory; the project's renv library is activated automatically.
# If it reports missing packages, run renv::restore() once in R opened in the project folder):
#   Rscript --no-init-file scripts/run_gui.R
#   The script keeps running until the window is closed.
#
# Reads:  the xlsx files chosen in the window, metadata/codebook.csv, metadata/qc_ranges.csv,
#         PROJECT_VERSION and the R/ functions.
# Writes: output/runs/<YYYYmmdd_HHMMSS>/ with qc/ (CSV tables), profile/ (CSV tables),
#         figures/ (PNG) and report.html. Writes only when the project folder is confirmed by
#         PROJECT_VERSION (Project: burn_vp). Nothing is written otherwise.

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script_arg) == 0) {
  stop("Run this file with Rscript so its location is known.", call. = FALSE)
}
script_dir <- dirname(normalizePath(sub("^--file=", "", script_arg[1]), winslash = "/"))
project_root <- dirname(script_dir)
if (basename(script_dir) != "scripts") {
  stop("This script must sit in the scripts folder of the project.", call. = FALSE)
}

for (f in list.files(file.path(project_root, "R"), pattern = "\\.R$", full.names = TRUE)) {
  source(f)
}
activate_project_renv(project_root)

window <- gui_build_window(project_root)
tcltk::tkwait.window(window$top)
