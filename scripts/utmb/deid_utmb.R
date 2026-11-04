# De-identification of the UTMB Epic Caboodle extract: dates move by one random offset per
# patient, linking keys are replaced by pseudonyms, identifiers and free text are dropped, as
# listed in metadata/utmb_deid_policy.csv. Ends with a leak check and exits with status 1 if
# an original identifier value is found in the output.
#
# Usage (from any working directory):
#   Rscript scripts/utmb/deid_utmb.R [--data <folder>] [--label <text>] [--secret <file>]
#   --data    folder with the UTMB files; default <project folder>/data. A relative path is
#             resolved against the project folder, not the working directory.
#   --label   optional text added to the run folder names
#   --secret  secret.txt of an earlier run, to give a later extract the same offsets and
#             pseudonyms; default is a new random secret
#
# Reads:  the UTMB files in the data folder, metadata/utmb_deid_policy.csv and the functions in
#         R/ and R/utmb/.
# Writes: output/utmb/deid_<YYYYmmdd_HHMMSS>[_<label>]/ with data/ (one file per table, same
#         names, ready for profile_utmb.R --data), summary.txt and column_actions.csv, and the
#         separate output/utmb/deid_keys_<YYYYmmdd_HHMMSS>[_<label>]/ with secret.txt and
#         offsets.csv. The keys folder can re-identify dates: store it apart, never share it.
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
activate_project_renv(project_root, packages = c("data.table", "digest"))

args <- commandArgs(trailingOnly = TRUE)
usage <- "Usage: deid_utmb.R [--data <folder>] [--label <text>] [--secret <file>]"
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
opt <- take_option(opt$args, "--secret")
secret_file <- opt$value
if (length(opt$args) > 0) stop(usage, call. = FALSE)
is_absolute <- function(path) grepl("^(/|~|[A-Za-z]:)", path)
if (is.null(data_dir)) data_dir <- file.path(project_root, "data")
if (!is_absolute(data_dir)) data_dir <- file.path(project_root, data_dir)
if (!dir.exists(data_dir)) stop("Data folder not found: ", data_dir, call. = FALSE)
if (!is.null(secret_file) && !is_absolute(secret_file)) {
  secret_file <- file.path(project_root, secret_file)
}

result <- run_utmb_deid(data_dir, project_root, label = label, secret_file = secret_file)
cat("De-identified tables: ", file.path(result$run_dir, "data"), "\n", sep = "")
cat("Keys (store apart, never share): ", result$keys_dir, "\n", sep = "")
if (!result$ok) {
  cat("LEAK CHECK FAILED: see summary.txt in ", result$run_dir, "\n", sep = "")
  quit(save = "no", status = 1)
}
