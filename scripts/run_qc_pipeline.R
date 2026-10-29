# Runs the data quality and profiling pipeline on one or more site files in the NBC template
# layout. Patient-level data stays inside the project's output/ folder.
#
# Usage (from any working directory; the project's renv library is activated automatically.
# If it reports missing packages, run renv::restore() once in R opened in the project folder):
#   Rscript scripts/run_qc_pipeline.R NBC=output/generated/a.xlsx UTMB=path/b.xlsx [--label demo]
#   Each SITE=path argument names a site and its xlsx file. Relative paths are resolved
#   against the project folder, not the working directory.
#   --label  optional text added to the run folder name
#
# Reads:  the xlsx files, metadata/codebook.csv, metadata/qc_ranges.csv, PROJECT_VERSION and
#         the R/ functions.
# Writes: output/runs/<YYYYmmdd_HHMMSS>[_<label>]/ with qc/ (CSV tables), profile/ (CSV tables),
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

args <- commandArgs(trailingOnly = TRUE)
usage <- "Usage: run_qc_pipeline.R SITE=path.xlsx [SITE=path.xlsx ...] [--label text]"
label <- NULL
if ("--label" %in% args) {
  at <- which(args == "--label")
  if (length(at) != 1 || at != length(args) - 1) stop(usage, call. = FALSE)
  label <- args[at + 1]
  args <- args[-c(at, at + 1)]
}
if (length(args) == 0 || !all(grepl("^[^=]+=.+$", args))) stop(usage, call. = FALSE)

sites <- sub("=.*$", "", args)
paths <- sub("^[^=]+=", "", args)
is_absolute <- grepl("^(/|~|[A-Za-z]:)", paths)
paths[!is_absolute] <- file.path(project_root, paths[!is_absolute])
inputs <- stats::setNames(paths, sites)

run_dir <- run_pipeline(inputs, project_root, run_label = label)
cat("Report:", file.path(run_dir, "report.html"), "\n")
