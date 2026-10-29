# Generates a SYNTHETIC (fake) dataset in the NBC template layout, for developing and testing
# the quality control pipeline. No real patient data is read or used.
#
# Usage (from any working directory; the project's renv library is activated automatically.
# If it reports missing packages, run renv::restore() once in R opened in the project folder):
#   Rscript scripts/generate_synthetic_nbc.R [n] [seed] [--corrupt]
#   n         number of fake patients, default 100
#   seed      random seed, default 1 (the same seed always gives the same file)
#   --corrupt inject known errors (3 per error type, see R/corrupt_synthetic.R) and also write
#             the list of injected errors as CSV next to the xlsx. Needs n of at least 39.
#
# Reads:  metadata/codebook.csv, the PROJECT_VERSION file, and the R/ functions.
# Writes: output/generated/NBC_synthetic_n<n>.xlsx (seed 1, no corruption) or
#         output/generated/NBC_synthetic_n<n>_seed<seed>[_corrupt].xlsx, sheet "Ark1", plus
#         the matching *_injected.csv with --corrupt. Writes only when the project folder is
#         confirmed by PROJECT_VERSION (Project: burn_vp). Nothing is written otherwise.
#
# Conventions in the generated data:
#   - Hematocrit is a percentage (25 to 60), as the data dictionary states, not a fraction.
#   - Fluid columns for 24h, 48h and 72h are cumulative from the burn. Albumin is not part of
#     the crystalloid or total fluid sums.
#   - Ventilation days, CRRT days and length of stay are plain date differences (stop minus
#     start), not inclusive counts.
#   - Pre-NE haemodynamics exist only for patients who receive norepinephrine, and CI and CVP
#     only when PiCCO is used. Comorbidity "other" is always No because it has no text field.
#   - Without --corrupt all records are internally consistent and no errors are seeded.

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script_arg) == 0) {
  stop("Run this file with Rscript so its location is known.", call. = FALSE)
}
script_dir <- dirname(normalizePath(sub("^--file=", "", script_arg[1]), winslash = "/"))
project_root <- dirname(script_dir)
if (basename(script_dir) != "scripts") {
  stop("This script must sit in the scripts folder of the project.", call. = FALSE)
}

for (f in c("project_paths.R", "codebook.R", "generate_synthetic.R", "qc_rules.R",
            "corrupt_synthetic.R")) {
  source(file.path(project_root, "R", f))
}
activate_project_renv(project_root)

args <- commandArgs(trailingOnly = TRUE)
corrupt <- "--corrupt" %in% args
args <- args[args != "--corrupt"]
if (any(grepl("^--", args)) || length(args) > 2) {
  stop("Usage: generate_synthetic_nbc.R [n] [seed] [--corrupt]", call. = FALSE)
}
n <- if (length(args) >= 1) suppressWarnings(as.numeric(args[1])) else 100
seed <- if (length(args) >= 2) suppressWarnings(as.numeric(args[2])) else 1
if (is.na(n) || n < 1 || n != round(n)) stop("n must be a positive whole number.", call. = FALSE)
if (is.na(seed)) stop("seed must be a number.", call. = FALSE)

# Confirms the project folder before anything is created.
out_dir <- project_output_dir(project_root, "generated")
codebook <- read_codebook(file.path(project_root, "metadata", "codebook.csv"))
data <- generate_synthetic_nbc(codebook, n = n, seed = seed)
injected <- NULL
if (corrupt) {
  data <- corrupt_synthetic(data, codebook, n_per_type = 3, seed = seed)
  injected <- attr(data, "injected")
}
stem <- if (seed == 1 && !corrupt) {
  sprintf("NBC_synthetic_n%d", as.integer(n))
} else {
  sprintf("NBC_synthetic_n%d_seed%d%s", as.integer(n), as.integer(seed),
          if (corrupt) "_corrupt" else "")
}
path <- file.path(out_dir, paste0(stem, ".xlsx"))
write_nbc_xlsx(data, codebook, path)
cat("Wrote", nrow(data), "synthetic patients to", path, "\n")
if (corrupt) {
  injected_path <- file.path(out_dir, paste0(stem, "_injected.csv"))
  utils::write.csv(injected, injected_path, row.names = FALSE, fileEncoding = "UTF-8")
  cat("Wrote", nrow(injected), "injected errors to", injected_path, "\n")
}
