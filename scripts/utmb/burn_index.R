# Burn index event analysis of the UTMB Epic Caboodle extract: the first burn diagnosis record
# from 2015 to 2023 (optionally widened, see --window-pad-days) per patient, its group,
# encounter, demographics, earlier burn records and agreement with the crosswalk.
#
# Usage (from any working directory):
#   Rscript scripts/utmb/burn_index.R [--data <folder>] [--label <text>]
#                                  [--window-pad-days <n>]
#   --data   folder with the UTMB files; default <project folder>/data. A relative path is
#            resolved against the project folder, not the working directory.
#   --label  optional text added to the run folder name
#   --window-pad-days  non-negative whole number of days added to both ends of the index window
#            2015-01-01 to 2023-12-31; default 0. Use it for de-identified data whose dates were
#            shifted. The summary states the effective window.
#
# Reads:  diagnoses, encounters, demographics and crosswalk in the data folder, and the
#         functions in R/ and R/utmb/.
# Writes: output/utmb/burn_index_<YYYYmmdd_HHMMSS>[_<label>]/ with aggregate CSV files, figures/,
#         summary.txt and index_events.csv. index_events.csv is PATIENT-LEVEL: do not share it.
#         Writes only when the project folder is confirmed by PROJECT_VERSION.

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script_arg) == 0) {
  stop("Run this file with Rscript so its location is known.", call. = FALSE)
}
script_dir <- dirname(normalizePath(sub("^--file=", "", script_arg[1]), winslash = "/"))
project_root <- dirname(dirname(script_dir))
if (basename(script_dir) != "utmb" || basename(dirname(script_dir)) != "scripts") {
  stop("This script must sit in scripts/utmb of the project.", call. = FALSE)
}

for (f in list.files(file.path(project_root, "R"), pattern = "\\.R$", full.names = TRUE)) {
  source(f)
}
if (!is_project_root(project_root)) {
  stop("Project folder not confirmed by PROJECT_VERSION. Nothing was done.", call. = FALSE)
}
for (f in list.files(file.path(project_root, "R", "utmb"), pattern = "\\.R$", full.names = TRUE)) {
  source(f)
}
activate_project_renv(project_root, packages = c("data.table", "ggplot2"))

args <- commandArgs(trailingOnly = TRUE)
usage <- paste0("Usage: burn_index.R [--data <folder>] [--label <text>]",
                " [--window-pad-days <n>]")
take_option <- function(args, flag) {
  at <- which(args == flag)
  if (length(at) == 0) return(list(value = NULL, args = args))
  if (length(at) != 1 || at == length(args)) stop(usage, call. = FALSE)
  list(value = args[at + 1], args = args[-c(at, at + 1)])
}
opt <- take_option(args, "--data")
data_dir <- opt$value
opt <- take_option(opt$args, "--label")
label <- opt$value
opt <- take_option(opt$args, "--window-pad-days")
window_pad_days <- if (is.null(opt$value)) {
  0L
} else {
  tryCatch(parse_window_pad_days(opt$value),
           error = function(e) stop(conditionMessage(e), "\n", usage, call. = FALSE))
}
if (length(opt$args) > 0) stop(usage, call. = FALSE)
if (is.null(data_dir)) data_dir <- file.path(project_root, "data")
if (!grepl("^(/|~|[A-Za-z]:)", data_dir)) data_dir <- file.path(project_root, data_dir)
if (!dir.exists(data_dir)) stop("Data folder not found: ", data_dir, call. = FALSE)

run_utmb_burn_index(data_dir, project_root, label = label, window_pad_days = window_pad_days)
