# Finding the de-identified UTMB data that analysis scripts must use instead of the raw extract.

utmb_deid_run_pattern <- "^deid_[0-9]{8}_[0-9]{6}(_.*)?$"

# TRUE when `run_dir` holds the files of a finished de-identification run whose leak check
# passed. summary.txt has the line "Result: OK" only when it did; a failed run still leaves its
# data/ folder and gets "Result: FAILED, do not use these files" instead.
utmb_deid_run_valid <- function(run_dir) {
  summary_file <- file.path(run_dir, "summary.txt")
  if (!dir.exists(file.path(run_dir, "data")) || !file.exists(summary_file) ||
        !file.exists(file.path(run_dir, "column_actions.csv"))) {
    return(FALSE)
  }
  any(trimws(readLines(summary_file, warn = FALSE)) == "Result: OK")
}

# Returns the data folder of the de-identified run to analyse. Without `data_arg` it is the newest
# valid output/utmb/deid_<date>_<time>[_<label>] run under `root` (deid_keys_* folders are never
# considered). With `data_arg` (a relative path is resolved against `root`) it must be the data
# folder of a valid run inside <root>/output/utmb, so the raw extract is refused.
resolve_utmb_deid_data <- function(root, data_arg = NULL) {
  utmb_dir <- file.path(root, "output", "utmb")
  if (is.null(data_arg)) {
    runs <- if (dir.exists(utmb_dir)) {
      list.files(utmb_dir, pattern = utmb_deid_run_pattern)
    } else {
      character()
    }
    runs <- runs[vapply(runs, function(r) utmb_deid_run_valid(file.path(utmb_dir, r)),
                        logical(1))]
    if (length(runs) == 0) {
      stop("No de-identified UTMB data found in ", utmb_dir,
           ". Run `Rscript scripts/utmb/deid_utmb.R` first.", call. = FALSE)
    }
    newest <- runs[order(substr(runs, 6, 20), runs, decreasing = TRUE)][1]
    return(normalizePath(file.path(utmb_dir, newest, "data"), winslash = "/"))
  }
  if (!grepl("^(/|~|[A-Za-z]:)", data_arg)) data_arg <- file.path(root, data_arg)
  data_path <- normalizePath(data_arg, winslash = "/", mustWork = FALSE)
  run_dir <- dirname(data_path)
  allowed <- dir.exists(data_path) && basename(data_path) == "data" && dir.exists(utmb_dir) &&
    dirname(run_dir) == normalizePath(utmb_dir, winslash = "/") &&
    grepl(utmb_deid_run_pattern, basename(run_dir)) && utmb_deid_run_valid(run_dir)
  if (!allowed) {
    stop("Not the data folder of a valid de-identified UTMB run: ", data_path,
         ". UTMB analysis must run on de-identified data. ",
         "Run `Rscript scripts/utmb/deid_utmb.R` first.", call. = FALSE)
  }
  data_path
}
