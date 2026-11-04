# Regenerates the de-identification policy from metadata/utmb_dictionary.csv and the rules in
# the file deid.R in R/utmb.
#
# Usage (from any working directory):
#   Rscript scripts/utmb/deid_policy.R [--label <text>]
#
# Writes: output/utmb/deid_policy_<YYYYmmdd_HHMMSS>[_<label>]/utmb_deid_policy.csv. Review it and
#         copy it by hand to metadata/utmb_deid_policy.csv, which is the file deid_utmb.R applies.

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
activate_project_renv(project_root, packages = "data.table")

args <- commandArgs(trailingOnly = TRUE)
label <- NULL
if (length(args) == 2 && args[1] == "--label") {
  label <- args[2]
} else if (length(args) > 0) {
  stop("Usage: deid_policy.R [--label <text>]", call. = FALSE)
}
cat("Policy written to ", run_utmb_deid_policy(project_root, label = label), "\n", sep = "")
