# Generates a SYNTHETIC (fake) dataset in the NBC template layout, for developing and testing
# the quality control pipeline. No real patient data is read or used.
#
# Usage (from the project folder):
#   Rscript scripts/generate_synthetic_nbc.R [n] [seed]
#   n    number of fake patients, default 100
#   seed random seed, default 1 (the same seed always gives the same file)
#
# Reads:  metadata/codebook.csv, the PROJECT_VERSION file, and the R/ functions.
# Writes: output/generated/NBC_synthetic_n<n>.xlsx, sheet "Ark1", only when the project folder
#         is confirmed by PROJECT_VERSION (Project: burn_vp). Nothing is written otherwise.
#
# Conventions in the generated data:
#   - Hematocrit is a percentage (25 to 60), as the data dictionary states, not a fraction.
#   - Fluid columns for 24h, 48h and 72h are cumulative from the burn. Albumin is not part of
#     the crystalloid or total fluid sums.
#   - Ventilation days, CRRT days and length of stay are plain date differences (stop minus
#     start), not inclusive counts.
#   - Pre-NE haemodynamics exist only for patients who receive norepinephrine, and CI and CVP
#     only when PiCCO is used. Comorbidity "other" is always No because it has no text field.
#   - All records are internally consistent; no errors are seeded.

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script_arg) == 0) {
  stop("Run this file with Rscript so its location is known.", call. = FALSE)
}
script_dir <- dirname(normalizePath(sub("^--file=", "", script_arg[1]), winslash = "/"))
project_root <- dirname(script_dir)
if (basename(script_dir) != "scripts") {
  stop("This script must sit in the scripts folder of the project.", call. = FALSE)
}

for (f in c("project_paths.R", "codebook.R", "generate_synthetic.R")) {
  source(file.path(project_root, "R", f))
}

args <- commandArgs(trailingOnly = TRUE)
n <- if (length(args) >= 1) suppressWarnings(as.numeric(args[1])) else 100
seed <- if (length(args) >= 2) suppressWarnings(as.numeric(args[2])) else 1
if (is.na(n) || n < 1 || n != round(n)) stop("n must be a positive whole number.", call. = FALSE)
if (is.na(seed)) stop("seed must be a number.", call. = FALSE)

# Confirms the project folder before anything is created.
out_dir <- project_output_dir(project_root, "generated")
codebook <- read_codebook(file.path(project_root, "metadata", "codebook.csv"))
data <- generate_synthetic_nbc(codebook, n = n, seed = seed)
path <- file.path(out_dir, sprintf("NBC_synthetic_n%d.xlsx", as.integer(n)))
write_nbc_xlsx(data, codebook, path)
cat("Wrote", nrow(data), "synthetic patients to", path, "\n")
